(module encode
  (make-image/data image-width image-height image-format image-depth image-data
   write-png-image)
  (import scheme chicken)

(use ports extras lolevel)
(use zlib1 zopfli)

(define-record-type <image>
  (%make-image width height format depth data) 
  image?
  (width  image-width)
  (height image-height)
  (format image-format)
  (depth  image-depth)
  (data   image-data))

(define-inline (write-long value port)
  (write-byte (fxand (fxshr value 24) 255) port)
  (write-byte (fxand (fxshr value 16) 255) port)
  (write-byte (fxand (fxshr value 8) 255) port)
  (write-byte (fxand value 255) port))

(: fxabs (procedure (fixnum) fixnum))
(define (fxabs x)
  (##core#inline "C_fixnum_abs" x))

; Valid image formats, the first element is the colour type as stored in the
; IHDR, following that there's number of components and the rest are the allowed
; bit depths
(define-constant +valid-formats+
  '((grey       0 1 1 2 4 8 16)
    (rgb        2 3       8 16)
    (index      3 1 1 2 4 8   )
    (grey+alpha 4 2       8 16)
    (rgb+alpha  6 4       8 16)))

;; Returns #t if 'fmt' is a valid color-format/depth combo
(define (image-format? fmt depth)
  (cond ((assq fmt +valid-formats+)
         => (lambda (x) (and (memq depth (cdddr x)) #t)))
        (else #f)))

;; Size of a pixel, used for filering
(define (format-pixel-size fmt depth)
  (cond ((assq fmt +valid-formats+)
         => (lambda (x) (fx/ (fx+ (fx* (caddr x) depth) 7) 8)))
        (else
          (error "Invalid format specified" fmt))))

;; Size of a row, in bytes
;; XXX: watch out for overflow!
(define (format-row-size width fmt depth)
  (let ((components (caddr (assq fmt +valid-formats+))))
    (fx/ (fx+ (fx* (fx* width components) depth) 7) 8)))

(define (format-ihdr-type fmt)
  (cond ((assq fmt +valid-formats+) => cadr)
        (else 
          (error "Invalid format specified" fmt))))

;; Returns the raw image data size in bytes
;; XXX: watch out for overflow!
(define (image-data-size width height fmt depth)
  (fx* height (format-row-size width fmt depth)))

(define (make-image/data width height fmt depth data)
  ;; Validate the parameters
  (##sys#check-exact width 'make-image/data)
  (##sys#check-exact height 'make-image/data)
  (##sys#check-exact depth 'make-image/data)
  (cond ((or (<= width 0) (<= height 0))
         (error "Image dimensions must be positive"))
        ((not (image-format? fmt depth))
         (error "Image format is invalid" fmt depth))
        ((and data (not (blob? data)))
         (error "Image data must be a blob"))
        ((and-let* ((data)
                    (got (blob-size data))
                    (expected (image-data-size width height fmt depth))
                    ((not (fx= got expected))))
           (error "Image data has the wrong size" expected got)))
        (else
          (%make-image width height fmt depth data))))

(define (write-chunk port ident data)
  (if data
      (let ((data-size (##sys#size data)))
        (write-long data-size port)
        (write-string ident 4 port)
        (write-string data #f port)
        (write-long (crc32 ident data) port))
      (begin
        (write-long 0 port)
        (write-string ident 4 port)
        (write-long (crc32 ident) port))))

(define (write-ihdr-chunk port image)
  (let* ((fmt (image-format image))
         (buf-port (open-output-string)))
    ; Assemble the IHDR
    (write-long (image-width image) buf-port)
    (write-long (image-height image) buf-port)
    (write-byte (image-depth image) buf-port)
    (write-byte (format-ihdr-type fmt) buf-port)
    (write-byte 0 buf-port)
    (write-byte 0 buf-port)
    (write-byte 0 buf-port)
    ; Write it out
    (write-chunk port "IHDR" (get-output-string buf-port))))

(define (write-iend-chunk port image)
  (write-chunk port "IEND" #f))

(define (filter-image image)
  (let* ((pixel-size (format-pixel-size (image-format image) (image-depth image)))
         (row-size (format-row-size (image-width image) (image-format image) (image-depth image)))
         (new-size (fx* (image-height image) (add1 row-size)))
         (out (make-blob new-size))
         (data (image-data image)))
    ;; Convenience functions to access the pixel data
    (define-inline (this)
      (##sys#byte data (fx+ in-pos x)))
    (define-inline (left)
      (if (fx< x pixel-size) 0 (##sys#byte data (fx- (fx+ in-pos x) pixel-size))))
    (define-inline (above)
      (if (fx= line 0) 0 (##sys#byte data (fx- (fx+ in-pos x) row-size))))
    (define-inline (upper-left)
      (if (fx= line 0) 0
          (if (fx< x pixel-size) 0
              (##sys#byte data (fx- (fx- (fx+ in-pos x) row-size) pixel-size)))))
    ;; Paeth predictor
    (define-inline (paeth a b c)
      (let* ((pa (fxabs (fx- b c)))
             (pb (fxabs (fx- a c)))
             (pc (fxabs (fx- (fx+ a b) (fxshl c 1)))))
        (cond ((and (fx<= pa pb) (fx<= pa pc)) a)
              ((fx<= pb pc) b)
              (else c))))
    ;; Filter a single scanline and put the result in line-buf
    (define (filter-line! line method in-pos line-buf)
      (case method
        ((0) ;; None
         (move-memory! data line-buf row-size in-pos 0))
        ((1) ;; Sub
         (do ((x 0 (fx+ x 1))) ((fx= x row-size))
           (##sys#setbyte line-buf x
            (fxand 255 (fx- (this) (left))))))
        ((2) ;; Up
         (do ((x 0 (fx+ x 1))) ((fx= x row-size))
           (##sys#setbyte line-buf x
            (fxand 255 (fx- (this) (above))))))
        ((3) ;; Average
         (do ((x 0 (fx+ x 1))) ((fx= x row-size))
           (##sys#setbyte line-buf x
            (fxand 255 (fx- (this) (fxshr (fx+ (left) (above)) 1))))))
        ((4) ;; Paeth
         (do ((x 0 (fx+ x 1))) ((fx= x row-size))
           (##sys#setbyte line-buf x
            (fxand 255 (fx- (this) (paeth (left) (above) (upper-left)))))))
        (else
          (error "Invalid method"))))

    (let ((line-buf (make-blob row-size))
          (scanlines (image-height image)))
      (let loop ((line 0) (in-pos 0) (out-pos 0))
        (when (fx< line scanlines)
          (let ((filter (random 4)))
            (filter-line! line filter in-pos line-buf)
            (##sys#setbyte out out-pos filter)
            (move-memory! line-buf out row-size 0 (fx+ 1 out-pos))
            (loop (fx+ line 1) (fx+ in-pos row-size) (fx+ out-pos (fx+ row-size 1)))))))
    out))

(define (write-idat-chunk port image)
  (let* ((filtered-buf (filter-image image))
         ; (ob (make-zopfli-options #f 5 #t 15))
         ; (out (zopfli-compress ob 'zlib filtered-buf))
         (out (zlib-compress 6 filtered-buf))
         )
    (unless out
      (error "Something bad happened"))
    (write-chunk port "IDAT" out)))

(define (write-png-image port image)
  (##sys#check-output-port port #t 'write-png-image)
  (write-string "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a" #f port)
  (write-ihdr-chunk port image)
  (write-idat-chunk port image)
  (write-iend-chunk port image))
)
