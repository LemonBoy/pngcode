(module zlib1
  (crc32 zlib-compress)
  (import chicken scheme lolevel foreign)

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

(define c-zlib-compress
  (foreign-lambda* scheme-object ((int level) (scheme-object in))
    "C_ulong in_len = C_header_size(in);"
    "C_ulong out_len = compressBound(in_len);"
    "C_uchar *buf = C_malloc(sizeof(C_header) + out_len);"
    "int rc = compress2(buf + sizeof(C_header), &out_len, C_data_pointer(in), in_len, level);"
    "if (rc != Z_OK) C_return(C_SCHEME_FALSE);"
    "C_block_header_init(buf, C_make_header(C_STRING_TYPE, out_len));"
    "C_return(buf);"))

(define (zlib-compress level in)
  (set-finalizer!
    (or (c-zlib-compress level in) (error "could not compress the buffer"))
    (lambda (obj) (free obj))))
)
