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
    (define (this data x in-pos)
      (##sys#byte data (fx+ in-pos x)))
    (define (left data x in-pos pixel-size)
      (if (fx< x pixel-size) 0 (##sys#byte data (fx- (fx+ in-pos x) pixel-size))))
    (define (above data x in-pos line row-size)
      (if (fx= line 0) 0 (##sys#byte data (fx- (fx+ in-pos x) row-size))))
    (define (upper-left data x in-pos line row-size pixel-size)
      (if (fx= line 0) 0
          (if (fx< x pixel-size) 0
              (##sys#byte data (fx- (fx- (fx+ in-pos x) row-size) pixel-size)))))
    ;; Paeth predictor
    (define (paeth a b c)
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
            (fxand 255 (fx- (this data x in-pos) (left data x in-pos pixel-size))))))
        ((2) ;; Up
         (do ((x 0 (fx+ x 1))) ((fx= x row-size))
           (##sys#setbyte line-buf x
            (fxand 255 (fx- (this data x in-pos) (above data x in-pos line row-size))))))
        ((3) ;; Average
         (do ((x 0 (fx+ x 1))) ((fx= x row-size))
           (##sys#setbyte line-buf x
            (fxand 255 (fx- (this data x in-pos)
                            (fxshr (fx+ (left data x in-pos pixel-size)
                                        (above data x in-pos line row-size)) 1))))))
        ((4) ;; Paeth
         (do ((x 0 (fx+ x 1))) ((fx= x row-size))
           (##sys#setbyte line-buf x
            (fxand 255
              (fx- (this data x in-pos)
                   (paeth (left data x in-pos pixel-size)
                          (above data x in-pos line row-size)
                          (upper-left data x in-pos line row-size pixel-size)))))))
        ((-1) ;; Bogus
         ;; XXX: This is needed to stop the compiler from inlining the `paeth`
         ;; and `upper-left` procedures above since the resulting loop is slower
         ;; and allocates memory
         (paeth 0 0 0)
         (upper-left 0 0 0 0 0 0))
        (else
          (error "Invalid method"))))

    (define (sum-abs-values buf from size)
      (let loop ((x from) (d 0) (sum 0))
        (if (fx< d size)
            (let ((byte (##sys#byte buf x)))
              ;; As suggested by the RFC "Consider the output bytes as signed
              ;; differences for this test"
              (if (fx< byte 128)
                  (loop (fx+ x 1) (fx+ d 1) (fx+ sum byte))
                  (loop (fx+ x 1) (fx+ d 1) (fx+ sum (fx- 255 byte)))))
            sum)))

    (let ((line-buf1 (make-blob row-size))
          (line-buf2 (make-blob row-size))
          (line-buf3 (make-blob row-size))
          (line-buf4 (make-blob row-size))
          (scanlines (image-height image)))
      (do ((line 0 (fx+ line 1))
           (in-pos 0 (fx+ in-pos row-size))
           (out-pos 0 (fx+ out-pos (fx+ row-size 1))))
          ((fx= line scanlines))
        ;; The base-case is the raw unfiltered scanline
        (let ((base0 (sum-abs-values data in-pos row-size)))
          ;; Try to find the best filtering option by applying them all and then
          ;; checking which one gives the smallest sum
          (filter-line! line 1 in-pos line-buf1)
          (filter-line! line 2 in-pos line-buf2)
          (filter-line! line 3 in-pos line-buf3)
          (filter-line! line 4 in-pos line-buf4)

          (let loop ((minsum base0) (filter 1) (best-buf #f) (best-filter 0)
                     (xs (list line-buf1 line-buf2 line-buf3 line-buf4)))
            (if (null? xs)
                (begin
                  ;; Each scanline begins with the filtering method used
                  (##sys#setbyte out out-pos best-filter)
                  (if (not best-buf)
                      (move-memory! data out row-size in-pos (fx+ 1 out-pos))
                      (move-memory! best-buf out row-size 0 (fx+ 1 out-pos))))
                (let ((lsum (sum-abs-values (car xs) 0 row-size)))
                  (if (fx< lsum minsum)
                      (loop lsum (fx+ filter 1) (car xs) filter (cdr xs))
                      (loop minsum (fx+ filter 1) best-buf best-filter (cdr xs)))))))))
    out))

(define (write-idat-chunk port image)
  (let* ((filtered-buf (filter-image image))
         ; (ob (make-zopfli-options #f 5 #t 15))
         ; (out (zopfli-compress ob 'zlib filtered-buf))
         (out (zlib-compress 9 filtered-buf))
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
