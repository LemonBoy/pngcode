(module encode
  (make-image image-width image-height image-format image-depth image-data
   write-png-image)
  (import scheme chicken)

(use ports extras lolevel)
(use typed-records)
(use zlib1 zopfli)

(define-record-type <image>
  (%make-image width height format depth data palette)
  image?
  (width   image-width   : fixnum)
  (height  image-height  : fixnum)
  (format  image-format  : symbol)
  (depth   image-depth   : fixnum)
  (data    image-data    : (or blob string))
  (palette image-palette : (or false (list-of fixnum))))

(define-inline (write-long value #!optional (port (current-output-port)))
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
  (let ((components (caddr (assq fmt +valid-formats+))))
    (fx/ (fx+ (fx* components depth) 7) 8)))

;; Size of a row, in bytes
;; XXX: watch out for overflow!
(define (format-row-size width fmt depth)
  (let ((components (caddr (assq fmt +valid-formats+))))
    (fx/ (fx+ (fx* (fx* width components) depth) 7) 8)))

(define (format-ihdr-type fmt)
  (cadr (assq fmt +valid-formats+)))

;; Returns the raw image data size in bytes
;; XXX: watch out for overflow!
(define (image-data-size width height fmt depth)
  (fx* height (format-row-size width fmt depth)))

(: make-image (procedure make-image (fixnum fixnum symbol fixnum (or blob string) (or false (list-of fixnum))) (struct <image>)))
(define (make-image width height fmt depth data palette)
  ;; Validate the parameters
  (##sys#check-exact width 'make-image)
  (##sys#check-exact height 'make-image)
  (##sys#check-exact depth 'make-image)
  (when palette (##sys#check-list palette 'make-image))
  (cond ((or (<= width 0) (<= height 0))
         (error "Image dimensions must be greater than zero" width height))
        ((not (image-format? fmt depth))
         (error "Image format is invalid" fmt depth))
        ((not (or (blob? data) (string? data)))
         (error "Image data must be a blob or a string"))
        ((and-let* ((got (##sys#size data))
                    (expected (image-data-size width height fmt depth))
                    ((not (= got expected))))
           (error "Image data has the wrong size" expected got)))
        ((and (eq? fmt 'index) (not palette))
         (error "You must specify a palette for this format" fmt))
        ((and palette (or (= depth 16) (memq fmt '(grey grey+alpha))))
         (error "Cannot specify a palette for this format" fmt))
        ((and palette (< (fxshl 1 depth) (length palette)))
         (error "Palette data has too many entries for this depth"))
        (else
          (%make-image width height fmt depth data palette))))

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
  (write-chunk port "IHDR"
    (with-output-to-string
      (lambda ()
        ; Width
        (write-long (image-width image))
        ; Height
        (write-long (image-height image))
        ; Bit depth
        (write-byte (image-depth image))
        ; Colour type
        (write-byte (format-ihdr-type (image-format image)))
        ; Compression method
        (write-byte 0)
        ; Filter method
        (write-byte 0)
        ; Interlace method
        (write-byte 0)))))

(define (write-iend-chunk port image)
  (write-chunk port "IEND" #f))

(define (write-plte-chunk port image)
  (and-let* ((palette (image-palette image)))
    (write-chunk port "PLTE"
      (with-output-to-string
        (lambda ()
          (for-each
            (lambda (v)
              (write-byte (fxand (fxshr v 16) 255))
              (write-byte (fxand (fxshr v  8) 255))
              (write-byte (fxand v 255)))
            palette))))))

(define (filter-image image)
  (let* ((pixel-size (format-pixel-size (image-format image) (image-depth image)))
         (row-size (format-row-size (image-width image) (image-format image) (image-depth image)))
         (new-size (fx* (image-height image) (fx+ 1 row-size)))
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
             (pc (fxabs (fx- (fx- (fx+ a b) c) c))))
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

    (define (sum-abs-values buf)
      (let ((buf-size (##sys#size buf)))
        (let loop ((x 0) (sum 0))
          (if (fx< x buf-size)
              (let ((byte (##sys#byte buf x)))
                ;; As suggested by the RFC "Consider the output bytes as signed
                ;; differences for this test"
                (if (fx< byte 128)
                    (loop (fx+ x 1) (fx+ sum byte))
                    (loop (fx+ x 1) (fx+ sum (fx- 255 byte)))))
              sum))))

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
         (out (zlib-compress -1 filtered-buf))
         )
    (unless out
      (error "Something bad happened"))
    (write-chunk port "IDAT" out)))

(define (write-png-image port image)
  (##sys#check-output-port port #t 'write-png-image)
  (write-string "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a" #f port)
  (write-ihdr-chunk port image)
  (write-plte-chunk port image)
  (write-idat-chunk port image)
  (write-iend-chunk port image))
)
