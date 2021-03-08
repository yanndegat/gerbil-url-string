(import :std/test
        :url-string/url-string
        :std/ref
        :std/error
        :std/format
        :std/misc/func
        :std/sugar)

(def (error-message=? message)
  (lambda (e) (and (url-error? e) (equal? (error-message e) message))))

(def url-string-test

  (test-suite "test :url-string"
    (def path [
               (make-path/param "foo" "")
               (make-path/param "bar.git" "")
               ])

    (def query [['foo :: "bar"] ['bar :: "foo"]])

    (def urlhttp1 (make-url "https" #f "host.com" #f #t path #f #f))
    (def urlssh1 (make-url "ssh" "git" "host.com" 22 #t path [[ 'ref :: "master" ]
                                                              [ 'foo :: "bar" ]] #f))

    (test-case "test make-url"
      (check-equal? (url-scheme urlhttp1) "https")
      (check-equal? (url-host urlhttp1) "host.com")
      (check-equal? (url-user urlhttp1) #f)
      (check-equal? (url-port urlhttp1) #f)

      (check-equal? (url-scheme urlssh1) "ssh")
      (check-equal? (url-user urlssh1) "git")
      (check-equal? (url-port urlssh1) 22)

      (check-exception (make-url "error://" "user" "host.com" 80 #t path query "frags")
                       (error-message=? "not a valid url scheme"))

      (check-exception (make-url "http" "a@" "host.com" 80 #t path query "frags")
                       (error-message=? "not a valid url user"))

      (check-exception (make-url "http" "user" "host_com" 80 #t path query "frags")
                       (error-message=? "not a valid url host"))

      (check-exception (make-url "http" "user" "host.com" "80" #t path query "frags")
                       (error-message=? "not a valid url port"))

      (check-exception (make-url "http" "user" "host.com" 80 "true" path query "frags")
                       (error-message=? "not a valid url path-absolute?"))

      (check-exception (make-url "http" "user" "host.com" 80 #t "/foo/bar" query "frags")
                       (error-message=? "not a valid url path"))

      (check-exception (make-url "http" "user" "host.com" 80 #t path "a=b&c=d" "frags")
                       (error-message=? "not a valid url query"))

      (check-exception (make-url "http" "user" "host.com" 80 #t path query 22)
                       (error-message=? "not a valid url fragment"))

      (!check-fail? (make-url #f #f "host.com" #f #t path #f #f))

      )

    (test-case "test url->string"
      (check-equal? (url->string urlhttp1) "https://host.com/foo/bar.git")
      (check-equal? (url->string urlssh1) "ssh://git@host.com:22/foo/bar.git?ref=master&foo=bar")
      )

    (test-case "test string->url"
      (def urlhttp1str (url->string urlhttp1))
      (def urlhttp2 (string->url urlhttp1str))

      (def urlssh1str (url->string urlssh1))
      (def urlssh2 (string->url urlssh1str))

      (check-equal? (url-scheme urlhttp2) "https")
      (check-equal? (url-host urlhttp2) "host.com")
      (check-equal? (url-user urlhttp2) #f)
      (check-equal? (url-port urlhttp2) #f)

      (check-equal? (url-scheme urlssh2) "ssh")
      (check-equal? (url-user urlssh2) "git")
      (check-equal? (url-port urlssh2) 22)
      )
    ))

(unless (run-tests! url-string-test) (exit 1))
