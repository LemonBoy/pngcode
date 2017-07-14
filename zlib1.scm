(module zlib1
  (crc32 adler32 zlib-compress)
  (import chicken scheme lolevel foreign)

(foreign-declare "#include <zlib.h>")

(define c-crc32
  (foreign-lambda* unsigned-int32 ((unsigned-int32 init) (scheme-object x))
    "C_return(crc32(init, C_data_pointer(x), C_header_size(x)));"))

(define c-adler32
  (foreign-lambda* unsigned-int32 ((unsigned-int32 init) (scheme-object x))
    "C_return(adler32(init, C_data_pointer(x), C_header_size(x)));"))

(define c-zlib-compress
  (foreign-lambda* scheme-object ((int level) (scheme-object in))
    "C_ulong in_len = C_header_size(in);"
    "C_ulong out_len = compressBound(in_len);"
    "C_uchar *buf = C_malloc(sizeof(C_header) + out_len);"
    "int rc = compress2(buf + sizeof(C_header), &out_len, C_data_pointer(in), in_len, level);"
    "if (rc != Z_OK) C_return(C_SCHEME_FALSE);"
    "C_block_header_init(buf, C_make_header(C_STRING_TYPE, out_len));"
    "C_return(buf);"))

(: crc32 (procedure crc32 (#!rest (or blob string)) number))
(define (crc32 . xs)
  (foldl c-crc32 0 xs))

(: adler32 (procedure adler32 (#!rest (or blob string)) number))
(define (adler32 . xs)
  (foldl c-adler32 1 xs))

(: zlib-compress (procedure zlib-compress (fixnum (or blob string)) string))
(define (zlib-compress level in)
  (set-finalizer!
    (or (c-zlib-compress level in) (error "could not compress the buffer"))
    (lambda (obj) (free obj))))
)
