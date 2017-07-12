(module crc (crc32)
(import chicken scheme foreign)

(foreign-declare "#include <zlib.h>")

(define crc32-update
  (foreign-lambda* unsigned-int32 ((unsigned-int32 init) (scheme-object x))
    "C_return(crc32(init, C_data_pointer(x), C_header_size(x)));"))

;; Calculate the CRC32 of the xs objects
(: crc32 (procedure crc32 (#!rest blob) number))
(define (crc32 . xs)
  (let lp ((crc 0) (rest xs))
    (if (null? rest)
        crc
        (lp (crc32-update crc (car rest)) (cdr rest)))))
)
