# url-string lib for gerbil scheme

url-string is a library for parsing urls.

Port of part of https://docs.racket-lang.org/net/url.html?q=url-%3Estring#%28def._%28%28lib._net%2Furl..rkt%29._url-~3estring%29%29  on gerbil scheme.

## Usage & Example

Usage is easy enough:

```scheme
(def urlhttp (string->url "https://host.com/foo/bar.git"))
(def urlssh (string->url "ssh://git@host.com:22/foo/bar.git?ref=master&foo=bar"))

(url-scheme urlhttp)  ; => "https"
(url-host   urlhttp)    ; => "host.com"
(url-user   urlhttp)    ; => #f
(url-port   urlhttp)    ; => #f

(url-scheme urlssh)  ; => "ssh"
(url-host   urlssh)    ; => "host.com"
(url-user   urlssh)    ; => "git"
(url-port   urlssh)    ; => 22
```
