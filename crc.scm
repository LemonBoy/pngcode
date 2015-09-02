(module crc (crc32)
(import chicken scheme foreign)

(foreign-declare "#include <zlib.h>")

(define (crc32-update crc str)
  ((foreign-lambda* unsigned-int32
     ((unsigned-int32 init) (blob str) (unsigned-int str_length))
     "C_return(crc32(init, str, str_length));")
   crc str (string-length str)))

;; Calculate the CRC32 of the xs objects
(define (crc32 . xs)
  (when (null? xs) 0)
  (let lp ((crc 0) (rest xs))
    (if (null? rest)
        crc
        (lp (crc32-update crc (car rest)) (cdr rest)))))
)
