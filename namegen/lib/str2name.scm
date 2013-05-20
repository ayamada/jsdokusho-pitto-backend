;; $Id$

;; 日本全国地名しりとり（↓）から勝手にアイデアを借りた、
;; 名前自動生成モジュールです
;; 本家・日本全国地名しりとり→ http://si.tabesugi.net:8080/
;; 予め、dic2cdb.scmを使って、cdb辞書を用意しておいて下さい

;; ToDo : cache実装
;;        str => '(md5-num sei-list mei-list)

(define-module str2name
  (use dbm)
  (use dbm.cdb)
  (use rfc.md5)
  (use gauche.charconv)
  (export
    <str2name>
    string->name
    string->name/yomi
    string->name-list
    is-ippan?
    get-midasigo
    get-yomi
    ))
(select-module str2name)

(define-class <str2name> ()
  ((salt-string
     :accessor salt-string-of
     :init-keyword :salt-string
     :init-value "salt-string")
   (dbm-type
     :accessor dbm-type-of
     :init-keyword :dbm-type
     :init-form <cdb>)
   (sei-file
     :accessor sei-file-of
     :init-keyword :sei-file
     :init-value "sei-ippan-14295.cdb")
   (sei-index
     :accessor sei-index-of
     :init-keyword :sei-index
     :init-value 14295)
   (sei-dbm
     :accessor sei-dbm-of
     :init-value #f)
   (mei-file
     :accessor mei-file-of
     :init-keyword :mei-file
     :init-value "mei-17879.cdb")
   (mei-index
     :accessor mei-index-of
     :init-keyword :mei-index
     :init-value 17879)
   (mei-dbm
     :accessor mei-dbm-of
     :init-value #f)
   (cache-size
     :accessor cache-size-of
     :init-keyword :cache-size
     :init-value 256)
   (cache
     :accessor cache-of
     :init-value #f)
   ))

;; ----

(define-method string->name ((self <str2name>) str)
  (let1 name-list (cache->name-list self str)
    (cond
      ((is-ippan? (cadr name-list)) (get-midasigo (cadr name-list)))
      ((is-ippan? (caddr name-list)) (get-midasigo (caddr name-list)))
      (else
        (format "~s ~s"
                (get-midasigo (cadr name-list))
                (get-midasigo (caddr name-list))
                )))))

(define-method string->name/yomi ((self <str2name>) str)
  (let1 name-list (cache->name-list self str)
    (cond
      ((is-ippan? (cadr name-list)) (format "~s (~s)"
                                            (get-midasigo (cadr name-list))
                                            (get-yomi (cadr name-list)
                                                      (car name-list))
                                            ))
      ((is-ippan? (caddr name-list)) (format "~s (~s)"
                                             (get-midasigo (caddr name-list))
                                             (get-yomi (caddr name-list)
                                                       (car name-list))
                                             ))
      (else
        (format "~s ~s (~s ~s)"
                (get-midasigo (cadr name-list))
                (get-midasigo (caddr name-list))
                (get-yomi (cadr name-list)
                          (car name-list))
                (get-yomi (caddr name-list)
                          (car name-list))
                )))))

;; ----

(define (cache->name-list self str)
  ;; キャッシュ実装はあとで考える
  (string->name-list self str)
  )

;; ----

(define (string->name-list self str)
  (prepare-dbm self)
  (let* ((md5-number (string->md5-number
                       (string-append
                         (salt-string-of self)
                         str)))
         (sei-list (md5-number->list (sei-dbm-of self)
                                     (remainder md5-number
                                                (sei-index-of self))))
         (mei-list (md5-number->list (mei-dbm-of self)
                                     (remainder md5-number
                                                (mei-index-of self))))
        )
    (list
      md5-number
      sei-list
      mei-list)))

;; ----

(define (prepare-dbm self)
  (if (not (sei-dbm-of self))
    (set!
      (sei-dbm-of self)
      (dbm-open (dbm-type-of self)
                :path (sei-file-of self)
                :rw-mode :read)))
  (if (not (mei-dbm-of self))
    (set!
      (mei-dbm-of self)
      (dbm-open (dbm-type-of self)
                :path (mei-file-of self)
                :rw-mode :read)))
  )

;; ----

(define (string->md5-number str)
  (with-input-from-string
    (md5-digest-string str)
    (lambda ()
      (let loop ((result 0)
                 (count 0))
        (let ((num (read-byte)))
          (if (eof-object? num)
            result
            (loop (+ result
                     (* num (expt 256 count)))
                  (+ count 1))))))))

;; ----

(define (md5-number->list dbm num)
  (let* ((line-src (dbm-get dbm
                            (number->string num)
                            #f))
         (line (ces-convert line-src "EUC-JP"))
         )
    (with-input-from-string
      (string-append "(" line ")")
      read)))

;; ----
;; ----
;; ----

;; ((品詞 (名詞 固有名詞 人名 名)) ((見出し語 (康弘 2177)) (読み ヤスヒロ) (発音 ヤスヒロ) ))
;; (品詞 (名詞 固有名詞 人名 名)) ((見出し語 (紀元 2956)) (読み {ノリモト/キゲン}) (発音 {ノリモト/キゲン}) )
;; (品詞 (名詞 固有名詞 人名 一般)) ((見出し語 (フランキー堺 2306)) (読み フランキーサカイ) (発音 フランキーサカイ) )
;; (品詞 (名詞 固有名詞 人名 姓)) ((見出し語 (前田 1902)) (読み {マエダ/マエタ}) (発音 {マエダ/マエタ}) )
;; gaucheのreadは、{}も()として解釈してくれたので、コレをそのまま使う

(define (is-ippan? list)
  (eq? '|一般| (cadddr (cadr (car list)))))
(define (get-midasigo list)
  (car (cadr (car (cadr list)))))
(define (get-yomi list . seed)
  (let ((yomi (cadr (cadr (cadr list)))))
    (if (symbol? yomi)
      yomi
      (let* ((yomi-list (string-split (symbol->string (car yomi)) #\/))
             (num (remainder (if (null? seed)
                               0
                               (car seed))
                             (length yomi-list)))
             )
        (string->symbol (list-ref yomi-list num))
        ))))

(provide "str2name")

;; vim:set ft=scheme ts=2 sw=2 et:
