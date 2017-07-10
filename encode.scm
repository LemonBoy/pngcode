(module encode
  (make-image/data write-png-image)
  (import scheme chicken)

(use extras lolevel crc zlib)

(define-record-type <image>
  (%make-image width height format data) 
  image?
  (width image-width)
  (height image-height)
  (format image-format)
  (data image-data))

(define-inline (write-long value port)
  (write-byte (fxand (fxshr value 24) 255) port)
  (write-byte (fxand (fxshr value 16) 255) port)
  (write-byte (fxand (fxshr value 8) 255) port)
  (write-byte (fxand value 255) port))

(define (fxabs x)
  (##core#inline "C_fixnum_abs" x))

(define-constant *format-pixel-size*
  '((rgb      . 3)
    (rgba     . 4)
    (gray     . 1)
    (paletted . 1)))

;; Returns #t if 'fmt' is a valid color-format
(define (image-format? fmt)
  (and (assq fmt *format-pixel-size*) #t))

(define (image-format-to-pixel-size format)
  (cdr (assq format *format-pixel-size*)))

(define (image-format-to-bpp format)
  (case format
    ((rgb rgba gray paletted) 8)
    (else (error "Invalid image format"))))

;; Returns the image size (in bytes) that the image blob takes for the given
;; parameters
(define (image-data-size width height fmt)
  (* width (image-format-to-pixel-size fmt) height))

(define (make-image/data width height fmt data)
  ;; Validate the parameters
  (cond ((or (< width 0) (< height 0))
         (error "Image dimensions must be positive"))
        ((not (image-format? fmt))
         (error "Image format is invalid" fmt))
        ((and data (not (blob? data)))
         (error "Image data must be a blob"))
        (else
          (%make-image width height fmt data))))

(define (write-chunk port ident data)
  (if data
      (let ((data-size (string-length data)))
        (write-long data-size port)
        (write-string ident 4 port)
        (write-string data data-size port)
        (write-long (crc32 ident data) port))
      (begin
        (write-long 0 port)
        (write-string ident 4 port)
        (write-long (crc32 ident) port))))

(define (write-ihdr-chunk port image)
  (let* ((format (image-format image))
         (color-type (case format
                       ((gray gray16) 0)
                       ((rgb) 2)
                       ((rgba rgba64) 6)
                       ((paletted) 3)))
         (buf-port (open-output-string)))
    ; Assemble the IHDR
    (write-long (image-width image) buf-port)
    (write-long (image-height image) buf-port)
    (write-byte (image-format-to-bpp format) buf-port)
    (write-byte color-type buf-port)
    (write-byte 0 buf-port)
    (write-byte 0 buf-port)
    (write-byte 0 buf-port)
    ; Write it out
    (write-chunk port "IHDR" (get-output-string buf-port))))

(define (write-iend-chunk port image)
  (write-chunk port "IEND" #f))

(define (filter-image image)
  (let* ((pixel-size (image-format-to-pixel-size (image-format image)))
         (row-size (fx* (image-width image) pixel-size))
         (new-size (fx* (image-height image) (add1 row-size)))
         (out (make-blob new-size))
         (data (image-data image)))
    ;; Convenience functions to access the pixel data
    (define-inline (this)
      (##sys#byte data (fx+ in-pos x)))
    (define-inline (left)
      (if (fx< x pixel-size) 0 (##sys#byte data (fx- (fx+ in-pos x) pixel-size))))
    ;; XXX: We can exploit the fact that line-buf holds the previous scanline
    (define-inline (above)
      (if (fx= line 0) 0 (##sys#byte data (fx- (fx+ in-pos x) row-size))))
    (define-inline (upper-left)
      (if (fx= line 0) 0
          (if (fx< x pixel-size) 0
              (##sys#byte data (fx- (fx- (fx+ in-pos x) row-size) pixel-size)))))
    ;; Paeth predictor
    (define-inline (paeth a b c)
      (let* ((p  (fx- (fx+ a b) c))
             (pa (fxabs (fx- p a)))
             (pb (fxabs (fx- p b)))
             (pc (fxabs (fx- p c))))
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
         (out (open-output-string))
         (zout (open-zlib-compressed-output-port out)))
    ;; The zlib egg doesn't like blobs...
    (write-string (blob->string filtered-buf) #f zout)
    (close-output-port zout)
    ;; Write the IDAT chunk after compressing the data
    (let* ((compressed-buf (get-output-string out))
           (compressed-len (string-length compressed-buf)))
      (write-chunk port "IDAT" (get-output-string out)))))

(define (write-png-image port image)
  (write-string "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a" #f port)
  (write-ihdr-chunk port image)
  (write-idat-chunk port image)
  (write-iend-chunk port image))
)

(import encode)

; Output a good old xor-pattern
(define (make-test-data width height)
  (let ((out (make-blob (* width height))))
    (let lp ((i 0))
      (when (< i height)
        (let lp ((j 0))
          (when (< j width)
            (##sys#setbyte out (+ (* i width) j) (fxand (fxxor i j) 255))
            (lp (add1 j))))
        (lp (add1 i))))
    out))

(define (make-test-data/color width height)
  (let ((out (make-blob (* 3 width height))))
    (let lp ((i 0))
      (when (< i height)
        (let lp ((j 0))
          (when (< j width)
            (##sys#setbyte out (+ (* i width 3) (* j 3))   (fx- 255 (fxxor i j)))
            (##sys#setbyte out (+ (* i width 3) (* j 3) 1) (fxand #x55 (fxxor i j)))
            (##sys#setbyte out (+ (* i width 3) (* j 3) 2) (fxand #xaa (fxxor i j)))
            (lp (add1 j))))
        (lp (add1 i))))
    out))

(let* ((width 128)
       (height width)
       (image (make-image/data width height 'rgb (make-test-data/color width height)))
       (file (open-output-file "test.png")))
  (time (write-png-image file image)))
