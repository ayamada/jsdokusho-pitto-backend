#!/usr/bin/env gosh
;; $Id$

(add-load-path "lib")
(use str2name)

;; ----

(define (usage self)
  (format (current-error-port)
          "usage: ~a string\n" self))

(define str2name
  (make <str2name>
    :salt-string "sio"
    ;:dbm-type <cdb>
    :sei-file "sei-ippan-14295.cdb"
    :sei-index 14295
    :mei-file "mei-17879.cdb"
    :sei-index 17879
    :cache-size 256
    ))

(define (main args)
  (if (not (= 2 (length args)))
    (usage (car args))
    ;(print (cadr args) " -> " (string->name/yomi str2name (cadr args)))
    (print (string->name str2name (cadr args)))
    ))

;; vim:set ft=scheme ts=2 sw=2 et:
