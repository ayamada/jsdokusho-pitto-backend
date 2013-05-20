#!/usr/bin/env gosh
;;; coding: utf-8
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; tcpcgi-serverの実行サンプル。

;;; usage :
;;; env - PATH="$PATH" ./tcpcgi-server-kickstart.scm

(add-load-path ".")
(use tcpcgi-server)

(use gauche.process)
(use srfi-1)
(use text.tree)
(use text.html-lite)
(use www.cgi)
(use st)


;;; ----------------------------------------------------------------
;;; ここから下は、サンプルcgi定義

(define (debug-cgi)
  (cgi-main
    (lambda (params)
      (list
        (cgi-header :content-type "text/html")
        (html:html
          (html:body
            (html:h1 "cgi-metavariables")
            (html:dl
              (map (lambda (x)
                     (list
                       (html:dt (html-escape-string (car x)))
                       (html:dd (html-escape-string (cadr x)))))
                   (sort
                     (cgi-metavariables)
                     (lambda (x y)
                       (string<? (car x) (car y))))))
            (html:hr)
            (html:h1 "form-parameters")
            (html:dl
              (map (lambda (x)
                     (list
                       (html:dt (html-escape-string (car x)))
                       (map
                         (lambda (xx)
                           (html:dd (html-escape-string xx)))
                         (cdr x))))
                   params))
            (html:hr)
            (html:h1 "environment")
            (html:ul
              (map
                (lambda (x)
                  (html:li (html-escape-string x)))
                (process-output->string-list "/usr/bin/env")))
            ))))
    :on-error report-error
    ))


(define (error404-cgi)
  (write-tree
    (list
      (cgi-header
        :status "404 Not Found"
        :pragma "no-cache"
        )
      (html:html
        (html:head
          (html:title "404 のっと ふぁうんど")
          )
        (html:body
          (html:h1 "のっと ふぁうんど")
          (html:p "みつかりません"))))))

(define (location-tcpcgi-cgi)
  (write-tree
    (cgi-header :location "http://d.tir.jp/pw?tcpcgi")))
(define (location-repos-cgi)
  (write-tree
    (cgi-header :location "http://svn.tir.jp/viewcvs.cgi/tcpcgi/")))


(define (st-cgi)
  ((with-module st main) '()))

;;; ----------------------------------------------------------------
;;; ここから下は、tcpcgi設定部分



;; alistで指定する。
(define *path-dispatch*
  `(
    ;("/" #f)
    ("/env" ,debug-cgi)

    ("/st" ,st-cgi)
    ("/img_st" "/home/yamada/st_work/img_st")

    ("/super" "/home/yamada/superthon/54an/super")
    ("/super-clone" "/home/yamada/superthon/yamada/super-clone")
    ))


;; alistで指定する。
(define *vhost-dispatch*
  `(
    ;; hoge.com
    ("133.242.135.169" ,*path-dispatch*)
    ("133.242.135.169:8888" ,*path-dispatch*)
    ;; *.hoge.com
    ;(".hoge.com"
    ; (("/" ,debug-cgi)
    ;  ("/abc" "/proc/cpuinfo")))
    ;;; mage.hoge.com (*.hoge.comよりも優先される)
    ;("mage.hoge.com" ,*path-dispatch*)
    ;;; *.fuge.com (fuge.comは含まれない)
    ;(".fuge.com"
    ; (("/" ,debug-cgi)
    ;  ("/def" "/proc/cpuinfo")))
    ))


(define *tcpcgi-server*
  (make <tcpcgi-server>
    :server-addr "0.0.0.0"
    :server-port 8888 ; 80等、1024以前のportを使うにはroot権限が必要。
    ;:server-name "hoge.com"
    :max-clients 8
    ;:child-uid "nobody"
    ;:child-gid "nobody"
    ;; ↑この二つはrootで実行している時のみ有効。
    :connection-log-port (current-error-port)

    ;; 以下のパラメータは<tcpcgi>と同じ

    ;; 以下の:dispatch-*は、この順に処理される。
    ;:dispatch-vhost *vhost-dispatch* ; vhostを使わないなら設定不要
    ;; vhostのどれにもマッチしなかった場合は次のdispatch-pathが実行される。
    :path-dispatch *path-dispatch*
    ;; pathのどれにもマッチしなかった場合は次のdispatch-fallbackが実行される。
    ;:dispatch-fallback debug-cgi
    ;:dispatch-fallback (list nph-cgi :nph #t) ; nph等にしたい場合はこうする
    ;; dispatch-fallbackも無指定なら、404が返される。

    ;:errordoc-alist `()
    ;:errordoc-alist `((404 ,debug-cgi))
    ;:errordoc-alist `((404 ,error404-cgi))

    ;; タイムアウト値等の設定
    :request-timeout 30 ; クライアントからのHTTPヘッダ読み出し時のタイムアウト
    ;; ↑人間が手入力をするなら、大きな値にする。通常は5ぐらい。
    :response-timeout 60 ; cgi実行時のタイムアウト
    :keepalive-timeout 10 ; keep-aliveタイムアウト
    ;; ↑reverse-proxyを使うなら、大きな値にする。
    ;; ↑人間が手入力をするなら、大きな値にする。通常は5ぐらい。
    :use-server-header #t ; Serverヘッダを送るか否か。デフォルトは#f。
    ;:max-requests-per-child 100
    ))

;;; ----

(define (main args)
  (tcpcgi-server-main *tcpcgi-server*))



