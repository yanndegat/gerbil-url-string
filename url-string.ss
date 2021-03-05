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

        path/param path/param? make-path/param
        path/param-path
        path/param-param

        string->url
        string->url*
        url->string)


;; Approximation to IPv6 literal addresses, to be recognized
;; in "[...]" when decoding and put back in "[...]" when encoding;
;; having at least one ":" distinguishes from other address forms:
;;
;; "^[0-9a-fA-F:]*:[0-9a-fA-F:]*$"
(def rx-ipv6-hex (regexp '(+ bos (* (or alnum ":")) ":" (* (or alnum ":")) eos)))

;; scheme parsing regexp
;; "^[a-zA-Z][a-zA-Z0-9+.-]*$"
(def rx-scheme (regexp '(+ bos alpha (* (or alnum ("+.-"))) eos)))

;; URL parsing regexp
;; this is roughly following the regexp in Appendix B of rfc 3986, except for using
;; `*' instead of `+' for the scheme part (it is checked later anyway, and
;; we don't want to parse it as a path element), and the user@host:port is
;; parsed here.
(define rx-url
  (regexp '(+ bos
            (?? ($ (* (~ (":/?#")))) ":")                 ; 1- scheme opt
            (?? "//"                                      ; slash-slash authority opt
                (?? ($ (* (~ ("/?#@")))) "@")             ; 2- user@ opt
                (?? (or                                   ;
                      (?? "[" ($ (* (or alnum ":"))       ; 3- ipv6 host opt
                                 ":"                      ;
                                 (* (or alnum ":"))) "]") ;
                      (?? ($ (* (~ ("/?#:")))))))         ; 4- host opt
                (?? ":" ($ (* num))))                     ; 5- :port opt
            ($ (* (~ ("?#"))))                            ; 6- path
            (?? "?" ($ (* (~ ("#")))))                    ; 7- query
            (?? "#" ($ (* any)))                          ; 8- fragment
            eos)))

(defstruct url (scheme         ;; (or/c false/c string?)
                user           ;; (or/c false/c string?)
                host           ;; (or/c false/c string?)
                port           ;; (or/c false/c number?)
                path-absolute? ;; boolean?
                path           ;; (listof path/param?)
                query          ;; (listof (cons/c symbol? (or/c string? false/c)))
                fragment       ;; (or/c false/c string?)
                ))

(defstruct path/param (path    ;; (or/c string? (symbols 'up 'same))
                       param   ;; (listof string?)])
                       ))

(defstruct (url-error <error>) ())

(def (raise-url-error where what . irritants)
  (raise (make-url-error what irritants where)))

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
                     (if (regexp-matches? rx-ipv6-hex host)
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
