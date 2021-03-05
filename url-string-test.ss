(import :std/test
        :url-string/url-string
        :std/ref
        :std/format
        :std/misc/func
        :std/sugar)

(def url-string-test

  (test-suite "test :url-string"

    (test-case "test url->string"

      (def path [
                 (make-path/param "foo" "")
                 (make-path/param "bar.git" "")
                 ])

      (def urlhttp1 (make-url "https" #f "host.com" #f #t path #f #f))
      (def urlssh1 (make-url "ssh" "git" "host.com" 22 #t path [[ 'ref :: "master" ]
                                                                [ 'foo :: "bar" ]] #f))
      (check-equal? (url-scheme urlhttp1) "https")
      (check-equal? (url-host urlhttp1) "host.com")
      (check-equal? (url-user urlhttp1) #f)
      (check-equal? (url-port urlhttp1) #f)

      (check-equal? (url-scheme urlssh1) "ssh")
      (check-equal? (url-user urlssh1) "git")
      (check-equal? (url-port urlssh1) 22)
      (check-equal? (url->string urlhttp1) "https://host.com/foo/bar.git")
      (check-equal? (url->string urlssh1) "ssh://git@host.com:22/foo/bar.git?ref=master&foo=bar")

      (set! urlhttp1 (string->url (url->string urlhttp1)))
      (set! urlssh1 (string->url (url->string urlssh1)))

      (check-equal? (url-scheme urlhttp1) "https")
      (check-equal? (url-host urlhttp1) "host.com")
      (check-equal? (url-user urlhttp1) #f)
      (check-equal? (url-port urlhttp1) #f)

      (check-equal? (url-scheme urlssh1) "ssh")
      (check-equal? (url-user urlssh1) "git")
      (check-equal? (url-port urlssh1) 22)
      )))

(run-tests! url-string-test)
