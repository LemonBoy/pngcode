(use extras
     srfi-4
     lolevel
     crc
     zlib)

(define-record-type <image>
  (%make-image
    width
    height
    format
    data)

  image?

  (width image-width set-image-width!)
  (height image-height set-image-height!)
  (format image-format set-image-format!)
  (data image-data set-image-data!))

(define (write-short value port)
  (write-byte (fxshr value 8) port)
  (write-byte (fxand value 255) port))

(define (write-long value port)
  (write-byte (fxand (fxshr value 24) 255) port)
  (write-byte (fxand (fxshr value 16) 255) port)
  (write-byte (fxand (fxshr value 8) 255) port)
  (write-byte (fxand value 255) port))

;; Returns #t if 'fmt' is a valid color-format
(define (image-format? fmt)
  (if (memq fmt '(rgb rgba gray paletted)) #t #f))

(define *format-pixel-size*
  '((rgb . 3)
    (rgba . 4)
    (gray . 1)
    (paletted . 1)))

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
  (unless (and (>= width 0) (>= height 0) (image-format? fmt) (u8vector? data)
               (= (u8vector-length data) (image-data-size width height fmt)))
    (error "Incorrect parameters"))
  (%make-image width height fmt data))

(define (write-chunk port ident #!optional data)
  (if (eq? data #f)
      (begin
        (write-long 0 port)
        (write-string ident 4 port)
        (write-long (crc32 ident) port))
      (let ((data-size (string-length data)))
        (write-long data-size port)
        (write-string ident 4 port)
        (write-string data data-size port)
        (write-long (crc32 ident data) port))))

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
  (write-chunk port "IEND"))

(define (filter-image image method)
  (let* ((pixel-size (image-format-to-pixel-size (image-format image)))
         (row-size (* (image-width image) pixel-size))
         (new-size (* (image-height image) (add1 row-size)))
         (out (make-u8vector new-size))
         (data (image-data image)))
    (case method
      ((none)
       (let lp ((l 0) (out-pos 0) (in-pos 0))
         (when (< l (image-height image))
           (u8vector-set! out out-pos 0) ; Filter type 0 = none
           (move-memory! data out row-size in-pos (add1 out-pos))
           (lp (add1 l) (+ out-pos (add1 row-size)) (+ in-pos row-size)))))

      ((up)
       (let lp ((l 0) (out-pos 0) (in-pos 0))
         (when (< l (image-height image))
           (u8vector-set! out out-pos 2) ; Filter type 2 = up
           (set! out-pos (add1 out-pos))

           (if (= l 0)
               (begin
                 (move-memory! data out row-size in-pos out-pos)
                 (set! out-pos (+ out-pos row-size))
                 (set! in-pos (+ in-pos row-size)))

           (let lp ((j 0))
             (when (< j row-size)
               (let ((up (u8vector-ref data (- in-pos row-size)))
                     (this (u8vector-ref data in-pos)))
                 (u8vector-set! out out-pos (fxand (fx- this up) 255))
                 (set! out-pos (add1 out-pos))
                 (set! in-pos (add1 in-pos))
                 (lp (add1 j))))))

           (lp (add1 l) out-pos in-pos))))

      ((sub)
       (let lp ((l 0) (out-pos 0) (in-pos 0))
         (when (< l (image-height image))
             (u8vector-set! out out-pos 1) ; Filter type 1 = sub
             (set! out-pos (add1 out-pos))

             (let lp ((j 0))
               (when (< j row-size)
                 (if (< j pixel-size)
                     (u8vector-set! out out-pos (u8vector-ref data in-pos))
                     (let ((this (u8vector-ref data in-pos))
                           (prev (u8vector-ref data (- in-pos pixel-size))))
                       (u8vector-set! out out-pos (fxand (fx- this prev) 255))))
                 (set! out-pos (add1 out-pos))
                 (set! in-pos (add1 in-pos))
                 (lp (add1 j))))

             (lp (add1 l) out-pos in-pos))))
      (else (error "Unknown filter method")))
    out))

(define (write-idat-chunk port image)
  (let* ((filtered-buf (filter-image image 'up))
         (out (open-output-string))
         (zout (open-zlib-compressed-output-port out)))
    ;; The zlib egg doesn't like 'write-u8vector'
    (write-string (blob->string (u8vector->blob/shared filtered-buf)) #f zout)
    (close-output-port zout)
    ;; Write the IDAT chunk after compressing the data
    (let* ((compressed-buf (get-output-string out))
           (compressed-len (string-length compressed-buf)))
      (write-chunk port "IDAT" (get-output-string out)))))

(define (write-png-image port image)
  (write-u8vector #u8(137 80 78 71 13 10 26 10) port)
  (write-ihdr-chunk port image)
  (write-idat-chunk port image)
  (write-iend-chunk port image))

; Output a good old xor-pattern
(define (make-test-data width height)
  (let ((out (make-u8vector (* width height))))
    (let lp ((i 0))
      (when (< i height)
        (let lp ((j 0))
          (when (< j width)
            (u8vector-set! out (+ (* i width) j) (fxand (fxxor i j) 255))
            (lp (add1 j))))
        (lp (add1 i))))
    out))

(define (make-test-data/color width height)
  (let ((out (make-u8vector (* 3 width height))))
    (let lp ((i 0))
      (when (< i height)
        (let lp ((j 0))
          (when (< j width)
            (u8vector-set! out (+ (* i width 3) (* j 3)) (fxand (fxxor i j) 255))
            (u8vector-set! out (+ (* i width 3) (* j 3) 1) (fxand (fxxor i j) 255))
            (u8vector-set! out (+ (* i width 3) (* j 3) 2) (fxand (fxxor i j) 255))
            (lp (add1 j))))
        (lp (add1 i))))
    out))

(let ((image (make-image/data 128 128 'rgb (make-test-data/color 128 128)))
      (file (open-output-file "test.png")))
  (write-png-image file image))
