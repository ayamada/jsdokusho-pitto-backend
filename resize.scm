#!/usr/local/gauche/bin/gosh
;#!/usr/local/gauche/bin/speedygosh

;;; TODO: 画像によってはconvertコマンドが終了しない可能性があるようだ。
;;;       公開サービスに組込む場合はタイムアウト判定を入れて、
;;;       一定時間内に終了しなければkillして失敗扱いにする必要がある。

(define-module resize
  (use srfi-1)
  (use file.util)
  (use gauche.process)
  (debug-print-width #f)
  (export
    ))
(select-module resize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *result-width* "345")
(define *result-height* "345")

;;; 固有設定を読み込む(別サーバ用)
;;; 面倒なので、 m.sc.tir.ne.jp かどうかだけで判定する
(define *tmp-dir*
  (if (equal? "m" (sys-gethostname))
    "/tmp" ; 開発機データディレクトリ
    "/home/yamada/st_work/tmp")) ; 本番機データディレクトリ

(define (usage)
  (print "usage: ./resize.scm orig.png result.jpg"))

(define (get-w+t file)
  (let* ((identify (process-output->string
                     (string-append
                       "identify "
                       file
                       " | cut -d ' ' -f 3")))
         (m (#/^(\d+)x(\d+)/ identify))
         (_ (unless m
              (error "cannot get identify")))
         (w (x->number (m 1)))
         (h (x->number (m 2)))
         )
    (values w h)))


(define (main args)
  (if (< (length args) 3)
    (usage)
    (let* ((src-file (cadr args))
           (dst-file (caddr args))
           (tmp-file (string-append *tmp-dir*
                                    "/resize_tmp_"
                                    (x->string (sys-getpid))
                                    ".png"))
           )
      ;; ファイルの存在チェック
      (unless (file-exists? src-file)
        (error "cannot found src-file"))
      (receive (src-width src-height) (get-w+t src-file)
        (unwind-protect
          (cond
            ((= src-width src-height) ; 正方形
             (sys-system
               (string-append
                 "convert -resize "
                 *result-width* "x" *result-height*
                 " -unsharp 2x1.4+0.5+0 -colors 255 -quality 90 "
                 src-file
                 " "
                 dst-file)))
            ((< src-width src-height) ; 縦長
             (sys-system
               (string-append
                 "convert -resize "
                 *result-width* "x" #;*result-height*
                 " -unsharp 2x1.4+0.5+0 -colors 255 "
                 src-file
                 " "
                 tmp-file))
             (receive (tmp-width tmp-height) (get-w+t tmp-file)
               (let* ((x "0")
                      (y (x->string
                           (floor (/ (- tmp-height (x->number *result-height*)) 2))))
                      )
                 (sys-system
                   (string-append
                     "convert -crop "
                     *result-width* "x" *result-height*
                     "+" x "+" y
                     " -quality 90 "
                     tmp-file
                     " "
                     dst-file)))))
             ((< src-height src-width) ; 横長
             (sys-system
               (string-append
                 "convert -resize "
                 #;*result-width* "x" *result-height*
                 " -unsharp 2x1.4+0.5+0 -colors 255 "
                 src-file
                 " "
                 tmp-file))
             (receive (tmp-width tmp-height) (get-w+t tmp-file)
               (let* (
                      (x (x->string
                           (floor (/ (- tmp-width (x->number *result-width*)) 2))))
                      (y "0")
                      )
                 (sys-system
                   (string-append
                     "convert -crop "
                     *result-width* "x" *result-height*
                     "+" x "+" y
                     " -quality 90 "
                     tmp-file
                     " "
                     dst-file)))))
            (else (error "assertion")))
          (sys-unlink tmp-file)))))
  0)

;;;===================================================================

(select-module user)
(define main (with-module resize main))

;; Local variables:
;; mode: scheme
;; end:
;; vim: set ft=scheme:
