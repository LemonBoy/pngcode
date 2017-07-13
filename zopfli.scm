(module zopfli
  (zopfli-compress make-zopfli-options)
  (import scheme chicken lolevel ports extras foreign foreigners)

(foreign-declare "#include <zopfli/zopfli.h>")

(define-foreign-record-type (zopfli-options "ZopfliOptions")
  (constructor: %make-zopfli-options)
  (destructor:  %free-zopfli-options)
  (bool verbose
        zopfli-options-verbose            zopfli-options-verbose-set!)
  (bool verbose_more
        zopfli-options-verbose-more       zopfli-options-verbose-more-set!)
  (int  numiterations
        zopfli-options-numiterations      zopfli-options-numiterations-set!)
  (bool blocksplitting
        zopfli-options-blocksplitting     zopfli-options-blocksplitting-set!)
  (int  blocksplittinglast
        zopfli-options-blocksplittinglast zopfli-options-blocksplittinglast-set!)
  (int  blocksplittingmax
        zopfli-options-blocksplittingmax  zopfli-options-blocksplittingmax-set!))

(define (make-zopfli-options verbose iterations block-splitting? block-splitting-max)
  (##sys#check-exact iterations 'make-zopfli-options)
  (##sys#check-boolean block-splitting? 'make-zopfli-options)
  (##sys#check-exact block-splitting-max 'make-zopfli-options)
  (let ((opts (%make-zopfli-options)))
    (set-finalizer! opts %free-zopfli-options)
    ; Fill in the structure
    (zopfli-options-verbose-set!           opts verbose)
    (zopfli-options-verbose-more-set!      opts (eq? verbose 'more))
    (zopfli-options-numiterations-set!     opts iterations)
    (zopfli-options-blocksplitting-set!    opts block-splitting?)
    (zopfli-options-blocksplittingmax-set! opts block-splitting-max)
    opts))

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

(define c-zopfli-init-options
  (foreign-lambda void "ZopfliInitOptions" zopfli-options))

(define c-zopfli-compress
  (foreign-lambda* scheme-object ((zopfli-options opt) (zopfli-format fmt) (scheme-object in))
    "C_word *buf = (C_word *)C_malloc(sizeof(C_header));"
    "size_t  out = sizeof(C_header);"
    "ZopfliCompress(opt, fmt, C_data_pointer(in), C_header_size(in),"
                    "(unsigned char **)&buf, &out);"
    "out -= sizeof(C_header);"
    "C_block_header_init(buf, C_make_header(C_STRING_TYPE, out));"
    "C_return(buf);"))

(: zopfli-compress (procedure ((or false zopfli-options) zopfli-format blob) string))
(define (zopfli-compress opt fmt blob)
  (set-finalizer!
    (c-zopfli-compress (or opt #$default_options) fmt blob)
    (lambda (obj) (free obj))))

(c-zopfli-init-options #$default_options)
)
