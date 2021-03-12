;;; stashcli command line client
(import
  :std/error
  :std/iter
  :std/sugar
  :std/srfi/13
  :std/srfi/115
  :std/net/uri
  :std/misc/list
  :std/format)

(export url url? make-url
        url-scheme
        url-user
        url-host
        url-port
        url-path-absolute?
        url-path
        url-query
        url-fragment

        url-scheme-set!
        url-user-set!
        url-host-set!
        url-port-set!
        url-path-absolute?-set!
        url-path-set!
        url-query-set!
        url-fragment-set!

        path/param path/param? make-path/param
        path/param-path
        path/param-param

        path/param-path-set!
        path/param-param-set!

        url-error url-error?
        string->url
        string->url*
        url->string)


;; Approximation to IPv6 literal addresses, to be recognized
;; in "[...]" when decoding and put back in "[...]" when encoding;
;; having at least one ":" distinguishes from other address forms:
;;
;; "^[0-9a-fA-F:]*:[0-9a-fA-F:]*$"
(def rx-ipv6-pattern '(+ (* (or alnum ":")) ":" (* (or alnum ":"))))
(def rx-ipv6 (regexp `(+ bos ,rx-ipv6-pattern eos)))

;; scheme parsing regexp
;; "^[a-zA-Z][a-zA-Z0-9+.-]*$"
(def rx-scheme-pattern '(+ alpha (* (or alnum ("+.-")))))
(def rx-scheme (regexp `(+ bos ,rx-scheme-pattern eos)))

(def rx-user-pattern '(+ alpha (* (or alnum ("+.-")))))
(def rx-user (regexp `(+ bos ,rx-user-pattern eos)))

(def rx-host-pattern '(+ (or alnum ("+.-"))))
(def rx-host (regexp `(+ bos ,rx-host-pattern eos)))

(def rx-port-pattern '(+ num))
(def rx-path-pattern '(* (~ ("?# "))))
(def rx-query-pattern '(* (~ ("# "))))

(def rx-fragment-pattern '(* (~ (" "))))
(def rx-fragment (regexp `(+ bos ,rx-fragment-pattern eos)))

;; URL parsing regexp
;; this is roughly following the regexp in Appendix B of rfc 3986, except for using
;; `*' instead of `+' for the scheme part (it is checked later anyway, and
;; we don't want to parse it as a path element), and the user@host:port is
;; parsed here.
(define rx-url
  (regexp `(+ bos
            (?? ($ ,rx-scheme-pattern) ":")               ; 1- scheme opt
            (?? "//"                                      ;
                (?? ($ ,rx-user-pattern) "@")             ; 2- user@ opt
                (?? (or                                   ;
                      (?? "[" ($ ,rx-ipv6-pattern) "]")   ; 3- ipv6 host
                      (?? ($ ,rx-host-pattern))))         ; 4- host opt
                (?? ":" ($ ,rx-port-pattern)))            ; 5- :port opt
            ($ ,rx-path-pattern)                          ; 6- path
            (?? "?" ($ ,rx-query-pattern))                ; 7- query
            (?? "#" ($ ,rx-fragment-pattern))             ; 8- fragment
            eos)))

(defstruct url (scheme         ;; (or/c false/c string?)
                user           ;; (or/c false/c string?)
                host           ;; (or/c false/c string?)
                port           ;; (or/c false/c number?)
                path-absolute? ;; boolean?
                path           ;; (listof path/param?)
                query          ;; (listof (cons/c symbol? (or/c string? false/c)))
                fragment       ;; (or/c false/c string?)
                )
  constructor: :url-init!
  final: #t)

(defmethod {:url-init! url}
  (lambda (self scheme user host port path-absolute? path query fragment)
    (unless (or (not scheme) (and (string? scheme)
                                  (regexp-matches? rx-scheme scheme)))
      (raise-url-error "not a valid url scheme" scheme))

    (unless (or (not user) (and (string? user)
                                (regexp-matches? rx-user user)))
      (raise-url-error "not a valid url user" user))

    (unless (or (not host) (and (string? host)
                                (regexp-matches? rx-host host)))
      (raise-url-error "not a valid url host" host))

    (unless (or (not port) (and (number? port)
                                (< 0 port)
                                (< port 65535)))
      (raise-url-error "not a valid url port" port))

    (unless (boolean? path-absolute?)
      (raise-url-error "not a valid url path-absolute?" path-absolute?))

    (unless (or (not path) (and (list? path)
                                (foldl (lambda (a b)
                                         (and (path/param? a) b))
                                       #t
                                       path)))
      (raise-url-error "not a valid url path" path))

    (unless (or (not query) (and (list? query)
                                 (foldl (lambda (a b)
                                          (and b
                                               (pair? a)
                                               (symbol? (car a))
                                               (string? (cdr a))))
                                        #t
                                        query)))
      (raise-url-error "not a valid url query" query))

    (unless (or (not fragment) (and (string? fragment)
                                    (regexp-matches? rx-fragment fragment)))
      (raise-url-error "not a valid url fragment" fragment))

    (struct-instance-init! self
                           scheme user host port path-absolute? path query fragment)))

(defstruct path/param (path    ;; (or/c string? (symbols 'up 'same))
                       param   ;; (listof string?)])
                       ))

(defstruct (url-error <error>) ())

(def (raise-url-error msg . irritants)
  (raise (make-url-error msg irritants #f)))

;; Like `string->url`, but returns #f for an error, intended for use
;; with pattern matching.
(def (string->url* str)
  (try
   (string->url str)
   (catch (url-error? exn) (lambda (exn) #f))))


;; string->url : str -> url
;; Original version by Neil Van Dyke
(def (string->url str)
  (let* ((url (regexp-search rx-url str))
         (x-scheme (regexp-match-submatch url 1))
         (x-user (regexp-match-submatch url 2))
         (x-ipv6host (regexp-match-submatch url 3))
         (x-host (regexp-match-submatch url 4))
         (x-port (regexp-match-submatch url 5))
         (x-path (regexp-match-submatch url 6))
         (x-query (regexp-match-submatch url 7))
         (x-fragment (regexp-match-submatch url 8)))
   (when (and x-scheme (not (regexp-matches? rx-scheme x-scheme)))
      (raise-url-error "invalid URL string; bad scheme\n  scheme: ~e\n  in: ~e" x-scheme str))
    (let* ((scheme   (and x-scheme (string-downcase x-scheme)))
           (host     (cond (x-ipv6host (and x-ipv6host (string-downcase x-ipv6host)))
                           (else (and x-host (string-downcase x-host)))))
           (user     (and x-user (uri-decode x-user)))
           (port     (and x-port (string->number x-port)))
           (abs?     (or (equal? "file" x-scheme)
                         (regexp-matches? '(: bos "/" (* (~ ("?#")))) x-path))) ; rx: "^/"
           (use-abs? (or abs?
                         ;; If an authority part is provided, the (empty) path must be
                         ;; absolute, even if it isn't written with a "/":
                         (and (or host user port) #t)))
           (path     (and x-path (separate-path-strings x-path)))
           (query    (if x-query (form-urlencoded->alist x-query) []))
           (fragment (and x-fragment (uri-decode x-fragment)))
           )
      (when (and (not abs?) (pair? path) host)
        (raise-url-error
         (string-append "invalid URL string;\n"
                        " host provided with non-absolute path (i.e., missing a slash)\n"
                        "  in: ~e")
                   str abs? x-path path))
      (make-url scheme user host port use-abs? path query fragment))))

(def (url->string url)
  (let ((scheme (url-scheme url))
        (user   (url-user url))
        (host   (url-host url))
        (port   (url-port url))
        (path   (url-path url))
        (query  (url-query url))
        (fragment (url-fragment url)))
    (when (and (equal? scheme "file")
               (not (url-path-absolute? url)))
      (raise-url-error 'url->string
                       "cannot convert relative file URL to a string: "
                       url))
    (when (and (or user host port)
               (pair? path)
               (not (url-path-absolute? url)))
      (raise-url-error 'url->string
                       "cannot convert relative URL with authority to a string: "
                       url))
    (append-strings
     (flatten [
               (if (not scheme) "" [scheme ":"])
               (if (or user host port)
                 [ "//"
                   (if (not user) "" [(uri-encode user) "@"])
                   (if (not host) ""
                     (if (regexp-matches? rx-ipv6 host)
                       [ "[" host "]"]
                       host))
                   (if (not port) "" [":" (number->string port)])]
                 (if (equal? "file" scheme) ; always need "//" for "file" URLs
                   [ "//" ]
                   ""))
               (combine-path-strings (url-path-absolute? url) path)
               (if (not query) "" [ "?" (alist->form-urlencoded query)])
               (if (not fragment) "" [ "#" (uri-encode fragment)])]))))

(define (separate-params s)
  (let ((lst (map path-segment-decode (regexp-split ";" s))))
    (make-path/param (car lst) (cdr lst))))

(def (separate-path-strings str)
  (let ((strs (regexp-split "/" str)))
    (map separate-params (if (equal? "" (car strs)) (cdr strs) strs))))

(def (path-segment-decode p)
  (cond ((equal? p "..") 'up)
        ((equal? p ".") 'same)
        (else (uri-decode p))))

(define (alist->form-urlencoded args)
  (string-join
   (map (lambda(arg)
          (define name  (uri-encode (symbol->string (car arg))))
          (define value (and (cdr arg) (uri-encode (cdr arg))))
          (if value (string-append name "=" value) name))
        args)
   (if (memq (current-alist-separator-mode) ['semi 'semi-or-amp]) ";" "&")))

(def (form-urlencoded->alist str)
  (def rx-keyval (regexp '(+ bos ($ (+ alnum)) "=" ($ (* any)) eos)))
  (def rx-sep
    (case (current-alist-separator-mode)
      ((semi) (regexp ";"))
      ((amp) (regexp "&"))
      (else (regexp '("&;")))))
  (if (null? str) []
      (map (lambda(x)
             ;; m = #f => no "=..." part
             (let* ((keyval (regexp-search rx-keyval x))
                    (match? (regexp-match? keyval)))
               (cons (string->symbol (uri-decode (if match? (regexp-match-submatch keyval 1) x)))
                     (and match? (uri-decode (regexp-match-submatch keyval 2))))))
           (regexp-split rx-sep str))))

(def current-alist-separator-mode
  (make-parameter 'amp-or-semi
    (lambda (s)
      (unless (memq s ['amp 'semi 'amp-or-semi 'semi-or-amp])
        (raise-url-error 'current-alist-separator-mode
                          "'amp, 'semi, 'amp-or-semi, or 'semi-or-amp"
                          s))
      s)))

(def (combine-path-strings absolute? path/params)
  (cond ((null? path/params) "")
        (else (let ((p (add-between (map join-params path/params) "/")))
                (if (and absolute? (pair? p)) (cons "/" p) p)))))

(def (join-params s)
  (if (equal? "" (path/param-param s))
    (path-segment-encode (path/param-path s))
    (string-join (map path-segment-encode
                      (cons (path/param-path s) (path/param-param s)))
                 ";")))

(def (path-segment-encode p)
  (cond ((eq?    p 'up)   "..")
        ((eq?    p 'same) ".")
        ((equal? p "..")  "%2e%2e")
        ((equal? p ".")   "%2e")
        (else (uri-encode p))))

(def (add-between l x)
  (cond
   ((equal? 0 (length (cdr l))) l)
   (else
    (cons (car l)
          (reverse (let loop ((i (cadr l)) (l (cddr l)) (r []))
                     (if (pair? l)
                       (loop (car l) (cdr l) (cons i (cons x r)))
                       (cons i (cons x r)))))))))
