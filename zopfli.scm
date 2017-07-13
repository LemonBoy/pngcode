(module zopfli
  (zopfli-compress-blob open-zopfli-output-port)
  (import scheme chicken lolevel ports extras foreign foreigners)

(foreign-declare "#include <zopfli/zopfli.h>")

(define-foreign-record-type (zopfli-options "ZopfliOptions")
  (constructor: make-zopfli-options)
  (destructor:  free-zopfli-options)
  (int verbose
       zopfli-options-verbose            zopfli-options-verbose-set!)
  (int verbose_more
       zopfli-options-verbose-more       zopfli-options-verbose-more-set!)
  (int numiterations
       zopfli-options-numiterations      zopfli-options-numiterations-set!)
  (int blocksplitting
       zopfli-options-blocksplitting     zopfli-options-blocksplitting-set!)
  (int blocksplittinglast
       zopfli-options-blocksplittinglast zopfli-options-blocksplittinglast-set!)
  (int blocksplittingmax
       zopfli-options-blocksplittingmax  zopfli-options-blocksplittingmax-set!))

(define-foreign-enum-type (zopfli-format int)
  (zopfli-format->int int->zopfli-format)
  ((gzip) ZOPFLI_FORMAT_GZIP)
  ((zlib) ZOPFLI_FORMAT_ZLIB)
  ((deflate) ZOPFLI_FORMAT_DEFLATE))

; Declare the foreign types so we can use them in the type annotations
(define-type zopfli-format  symbol)
(define-type zopfli-options pointer)

; Static object holding the default options
(define-location default_options "ZopfliOptions")

(define zopfli-init-options
  (foreign-lambda void "ZopfliInitOptions" zopfli-options))

(define zopfli-compress
  (foreign-lambda* scheme-object ((zopfli-options opt) (zopfli-format fmt) (scheme-object in))
    "C_word *buf = (C_word *)C_malloc(sizeof(C_header));"
    "size_t  out = sizeof(C_header);"
    "ZopfliCompress(opt, fmt, C_data_pointer(in), C_header_size(in),"
                    "(unsigned char **)&buf, &out);"
    "out -= sizeof(C_header);"
    "C_block_header_init(buf, C_make_header(C_STRING_TYPE, out));"
    "C_return(buf);"))

(: zopfli-compress-blob (procedure ((or false zopfli-options) zopfli-format blob) string))
(define (zopfli-compress-blob opt fmt blob)
  (set-finalizer!
    (zopfli-compress (or opt #$default_options) fmt blob)
    (lambda (obj) (free obj))))

(define (open-zopfli-output-port sink)
  (##sys#check-output-port sink #t 'open-zopfli-output-port)
  (let ((options (make-zopfli-options)))
    (zopfli-init-options options)
    (set-finalizer!
      (make-output-port
        (lambda (in)
          (write-string (zopfli-compress-blob options 'zlib in) #f sink))
        void)
      (lambda (_) (free-zopfli-options options)))))

(zopfli-init-options #$default_options)
)
