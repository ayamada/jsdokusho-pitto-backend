#!/usr/local/gauche/bin/speedygosh
;#!/usr/local/gauche/bin/gosh

(add-load-path "/home/yamada/copy-of-svn.tir.jp/Gauche-tir/branches/Gauche-tir01/trunk/")
(add-load-path "/home/yamada/copy-of-svn.tir.jp/Gauche-tir/trunk/lib/")
(define-module st
  (use www.cgi)
  (use rfc.http)
  (use text.html-lite)
  (use text.tree)
  (use text.tr)
  (use srfi-1)
  (use srfi-13)
  (use rfc.uri)
  (use rfc.http)
  (use rfc.cookie)
  (use rfc.json)
  (use util.list)
  (use file.util)
  (use gauche.charconv)
  (use gauche.process)
  (use dbm.fsdbm)
  (use tir.dbmwl)
  (use tir04.cgi.session)
  (use tir04.cgi.util)
  (use rfc.uuid)
  (use srfi-27)
  (use math.const)
  (debug-print-width #f)
  (export
    ))
(select-module st)

(random-source-randomize! default-random-source)

;; quick patch
(define *blacklist-tel* "05011112222")
(define *alternative-tel* "05011113333")
(define (filter-bl-tel tel-str)
  (if (equal? *blacklist-tel* tel-str)
    *alternative-tel*
    tel-str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 固有設定を読み込む(別サーバ用)
;;; 面倒なので、 m.sc.tir.ne.jp かどうかだけで判定する
;;; TODO: 以下のdefineをまとめる
(define *data-dir* ; データディレクトリ
  (if (equal? "m" (sys-gethostname))
    "/var/nekoie/ams/data" ; 開発機
    "/home/yamada/st_work/data")) ; 本番機

(define *img-dir* ; 画像ディレクトリ
  (if (equal? "m" (sys-gethostname))
    "/var/nekoie/ams/img_st" ; 開発機
    "/home/yamada/st_work/img_st")) ; 本番機

(define *tmp-prefix* ; 一時ディレクトリおよびファイル前置句部
  (if (equal? "m" (sys-gethostname))
    "/var/nekoie/ams/tmp/st_image_" ; 開発機
    "/home/yamada/st_work/tmp/st_image_")) ; 本番機

(define *resize-scm-path*
  (if (equal? "m" (sys-gethostname))
    "/var/www/ams/resize.scm" ; 開発機
    "/home/yamada/st_work/ams/resize.scm")) ; 本番機

(define *img-url-prefix* ; 画像URL
  ;(if (equal? "m" (sys-gethostname))
  ;  "http://ams.vnctst.tir.jp/img_st/" ; 開発機
  ;  "http://999.999.999.999/img_st/") ; 本番機
  "/img_st/" ; 共通
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; デバッグ出力まわり
(define-macro (debug-dump obj)
  (if (equal? "m" (sys-gethostname))
    `(let1 o ,obj (warn (format "~s" o)) o)
    obj))
(define (exception-dump e)
  (warn (call-with-output-string
          (cut with-error-to-port <> (cut report-error e))))
  e)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ダミーデータ生成用
;;; これは make repl からのみ実行可能
;;; (cgiからはcwdが合わない)
;;; ※要注意：二回実行しない事！全く同じ名前で二重登録されてしまう
;;; データ削除は、今のところ、直にdataディレクトリを捨てる方向で。
;;; ただしpostalのみはできれば残しておく事

(define (rand-int top)
  (floor (* top (/ (sys-random) RAND_MAX))))

(define (select-random . targets)
  (list-ref targets (rand-int (length targets))))

(define (generate-dummy-card! bid-list)
  (for-each
    (lambda (bid)
      (when bid
        (let* (
               (img-url (select-random "cat1.jpg"
                                       "cat2.jpg"
                                       "cat3.jpg"
                                       "cat4.jpg"
                                       "dog1.jpg"
                                       "dog2.jpg"
                                       "dog3.jpg"
                                       "dog4.jpg"
                                       "dog5.jpg"
                                       "dog6.jpg"
                                       "dog7.jpg"
                                       "dog8.jpg"
                                       "ham1.jpg"
                                       "ham2.jpg"
                                       ;; ダミーは全部画像を割り当てる事にする
                                       ;""
                                       ;""
                                       ;""
                                       ;""
                                       ))
               (cat (cond
                      ((#/^cat/ img-url) "猫")
                      ((#/^dog/ img-url) "犬")
                      ((#/^ham/ img-url) "ハムスター")
                      (else "その他")))
               (age (string-append
                      (x->string (+ 1 (rand-int 10)))
                      (select-random "歳" "ヶ月")
                      (select-random "" "ぐらい")))
               (name (string-append
                       (select-random "" "" "" "" "" "すごい")
                       (select-random
                         "わん" "にゃー" "がる" "花" "ふさ" "宙" "チュー")
                       (select-random
                         "わん" "にゃー" "がる" "ふさ" "チュー" "" "" "")
                       (select-random "太" "子" "君" "" "" "")
                       ))
               (comment (select-random "おおきいです"
                                       "ちいさいです"
                                       "げんきです"
                                       "きれいです"
                                       ))
               (params `(
                         ;("id" ,id) ; 新規登録なのでidなし
                         ("cat" ,cat)
                         ("age" ,age)
                         ("name" ,name)
                         ("comment" ,comment)
                         ("img_url_force" ,(if (equal? "" img-url)
                                             ""
                                             (string-append
                                               *img-url-prefix* img-url)))
                         )))
          (post-petinfo `(:bid ,bid) params))))
    bid-list))

(define *str2name* #f) ; NB: <cdb> がGCされる際に何故か cdb.so がsegvを起こしてしまうので、トップレベルに保持してGCされないようにする事にして回避した。
(define (generate-dummy-breeder!)
  (let* (
         (str2name (make (eval '<str2name> (current-module))
                         :salt-string "sio"
                         ;:dbm-type <cdb>
                         :sei-file "namegen/sei-ippan-14295.cdb"
                         :sei-index 14295
                         :mei-file "namegen/mei-17879.cdb"
                         :sei-index 17879
                         :cache-size 256
                         ))
         (_ (set! *str2name* str2name))
         (postal->name (lambda (p)
                         ((eval 'string->name (current-module)) str2name p)))
         (postals (filter
                    (lambda (x)
                      (and
                        (string? x)
                        (#/\d\d\d\d\d\d\d/ x)))
                    (call-with-input-file
                      ;"postal_test.csv" ; デバッグ用
                      "postal_osaka.csv" ; 梅田近辺
                      port->string-list)))
         (r (map (lambda (postal)
                   (let* ((name (string-append (x->string
                                                 (postal->name postal))
                                               " (サンプル)"))
                          ;(tel (string-append "0000" postal))
                          ;; 電話番号は全部twilioのブリーダーに
                          ;; 割り当てられてる番号にする事になった
                          (tel "05011113333")
                          (b (post-breeder () `(
                                                ("name" ,name)
                                                ("tel" ,tel)
                                                ("postal" ,postal)
                                                ) #t))
                          (json (cadr b))
                          (alist (parse-json-string json))
                          (bid (assoc-ref alist "bid" #f))
                          )
                     bid))
                 postals)))
    r))

(define (generate-dummy-data!)
  (unless (equal? "m" (sys-gethostname))
    (error "開発環境以外では実行できません"))
  (eval '(add-load-path "namegen/lib") (current-module))
  (eval '(use str2name) (current-module))
  (let* ((bid-list (generate-dummy-breeder!))
         )
    (generate-dummy-card! bid-list)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 計算用設定項目

;; 最大取得件数
(define *cards-list-max* 20)

;; 半径Nメートル以内のカードの一覧を取得するN
(define *match-radius-m* (* 10 1000)) ; 10km
;(define *match-radius-m* (* 10000 1000)) ; 10000km(デバッグ用)

;; 緯度経度簡易判定用(南北もしくは東西が上記距離離れてたら範囲外確定)
;; 緯度(y)1度の長さ(メートル)
(define *latitude-m* (* 2 pi 6378150 1/360))
;; 経度(x)1度の長さ(メートル)(あまり正確でないが、これでよいものとする)
(define *longitude-m* (* 25.153129 3600))
;; TODO: ↑のパラメータを使い、距離測定の最適化を行う
;;         (要は緯度か経度の単体で既に半径越えてたら除外するだけ)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for json
;;;
(define true 'true)
(define false 'false)
(define null 'null)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NB: 微妙に実行時cwdが固定でない為、絶対path指定が必要！


(define *cgi-session-path* (string-append *data-dir* "/session.fsdbm"))
(define *cgi-session-ttl* (* 366 24 60 60))
(define *cgi-session*
  (make
    <cgi-session>
    :dbm-type <fsdbm>
    :dbm-path *cgi-session-path*
    :expire-second (* 1 24 60 60)
    :cookie-name "st"
    :cookie-keywords `(
                       :discard #f
                       :path "/"
                       :expires ,(+ (sys-time) *cgi-session-ttl*)
                       :max-age *cgi-session-ttl*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define *dbm-type* <fsdbm>)
(define *dbm-root* *data-dir*)
(define *dbm-path-sys* (string-append *dbm-root* "/sys.fsdbm"))
(define *dbm-path-card* (string-append *dbm-root* "/card.fsdbm"))
(define *dbm-path-breeder* (string-append *dbm-root* "/breeder.fsdbm"))
;; NB: 画像は直に扱う必要があるのでdbmにできない
(define *dbm-path-postal* (string-append *dbm-root* "/postal.fsdbm"))
(define *dbm-path-user* (string-append *dbm-root* "/user.fsdbm"))

(define *dbmwl-sys*
  (make <dbmwl> :dbm-type *dbm-type* :dbm-path *dbm-path-sys*))
(define *dbmwl-card*
  (make <dbmwl> :dbm-type *dbm-type* :dbm-path *dbm-path-card*))
(define *dbmwl-breeder*
  (make <dbmwl> :dbm-type *dbm-type* :dbm-path *dbm-path-breeder*))
(define *dbmwl-postal*
  (make <dbmwl> :dbm-type *dbm-type* :dbm-path *dbm-path-postal*))
(define *dbmwl-user*
  (make <dbmwl> :dbm-type *dbm-type* :dbm-path *dbm-path-user*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 位置情報取得用関数
;;; これらの関数は外部APIに問い合わせを行う為、可能な限りキャッシングする

(define (postal->geoapi-result postal gen-mode?)
  (or
    (dbm-get *dbmwl-postal* postal #f)
    ;; NB: 今のところ、エラー時は#fを返す
    ;; TODO: 通信エラーと郵便番号該当なしは区別すべきだが…
    (guard (e (else (exception-dump e) #f))
      ;; ダミーデータ生成時は、郵便番号API負荷回避の為、休みながら行う
      (when gen-mode?
        (sys-sleep 1))
      (receive (status header body) (http-get "geoapi.heartrails.com"
                                              `("/api/json"
                                                (method "searchByPostal")
                                                (postal ,postal)))
        ;; NB: statusのチェックもすべきだが省略(parseでエラーになるので)
        (let* ((r (parse-json-string body)))
          (if (equal? "error" (car (cadr (car r))))
            #f ; 対応する住所なし
            (begin
              (dbm-put! *dbmwl-postal* postal r)
              r)))))))
#|
(("response"
  ("error" . "Cities of postal code :0000000 do not exist.")))
(("response"
  ("location" . #((("city" . "川西市")
                   ("city_kana" . "かわにしし")
                   ("town" . "新田１丁目")
                   ("town_kana" . "しんでん")
                   ("x" . "135.410791")
                   ("y" . "34.86253")
                   ("prefecture" . "兵庫県")
                   ("postal" . "6660125"))
                  (("city" . "川西市")
                   ("city_kana" . "かわにしし")
                   ("town" . "新田２丁目")
                   ("town_kana" . "しんでん")
                   ("x" . "135.407047")
                   ("y" . "34.862996")
                   ("prefecture" . "兵庫県")
                   ("postal" . "6660125"))
                  (("city" . "川西市")
                   ("city_kana" . "かわにしし")
                   ("town" . "新田３ 丁目")
                   ("town_kana" . "しんでん")
                   ("x" . "135.410779")
                   ("y" . "34.865313")
                   ("prefecture" . "兵庫県")
                   ("postal" . "6660125"))))))
 |#
(define (geo->vec geo)
  (cdr (cadr (car geo))))

(define (geo->vec0 geo)
  (vector-ref (geo->vec geo) 0))

(define (geo->address geo)
  (let* ((alist (geo->vec0 geo))
         (city (assoc-ref alist "city"))
         (prefecture (assoc-ref alist "prefecture"))
         )
    (string-append prefecture city)))

(define (num->geo-num-str num)
  (format "~6d" (exact->inexact (/ (round (* 1000000 num)) 1000000))))

(define (geo->x+y geo)
  ;; NB: (value "135.407047" "34.865313") のような値を返す。文字列注意
  (let* ((vec (geo->vec geo))
         (x+y-list (map (lambda (alist)
                          (let* ((x (assoc-ref alist "x"))
                                 (y (assoc-ref alist "y")))
                            (list x y)))
                        (vector->list vec)))
         (denom (length x+y-list)))
    (if (zero? denom)
      (values "0" "0") ; (values "135.001472222" "34.649388888") ; ダミー値
      (let1 r (fold (lambda (x+y old)
                      (list
                        (+ (string->number (car x+y)) (car old))
                        (+ (string->number (cadr x+y)) (cadr old))))
                    '(0 0)
                    x+y-list)
        (values (num->geo-num-str (/ (car r) denom))
                (num->geo-num-str (/ (cadr r) denom)))))))


(define (is-postal-valid? postal)
  ;; NB: postalは"0123456"のような、七桁の数の文字列でなくてはならない
  (and
    (string? postal)
    (#/^\d\d\d\d\d\d\d$/ postal)
    #t))


;; 電話番号を+からはじまる国際電話番号に強制する
(define (coerce-to-international-tel tel-str)
  (cond
    ((#/^\+/ tel-str) tel-str) ; 既に国際電話番号だった
    ((#/^0/ tel-str) ; 0はじまりだった、国際電話番号にする
     => (lambda (m) (string-append "+81" (m 'after))))
    ;; +でも0でも始まってない場合は仕方がないので、むりやり処理する
    (else
      (string-append "+81" tel-str))))

(define (coerce-to-non-international-tel tel-str)
  (cond
    ((#/^\+81/ tel-str) => (lambda (m) (m 'after))) ; 国際電話番号だった
    ((#/^0/ tel-str) tel-str) ; 既に0はじまりだった
    ;; +でも0でも始まってない場合は仕方がないので、そのままにする
    (else tel-str)))

(define (post-k5m type phone return)
  ;; TODO: 試験するとお金がかかるので止めてある為、
  ;;       http-postするとエラーになる。
  ;;       なのでエラーを殺してある。
  ;;       返り値は文字列として保存されるので、エラー時にも
  ;;       それらしい値を返す事(phoneがよい)
  ;; TODO: まだtwilio側バックエンドが動いてないので、処理できない！
  ;; 一時的に、何もしない関数におきかえる。
  #;phone
  (guard (e (else (exception-dump e)
              (return (output-errot-json! "登録に失敗しました"))
              ;phone
              ))
    ;; パラメータ
    ;; ・type: "user" or "breeder"
    ;; ・phone "+81xxxxxxx" 登録携帯番号（実機）
    (receive (status header body) (http-post
                                    "999.999.999.999"
                                    "/api/account"
                                    `(("type" ,type)
                                      ("phone" ,(coerce-to-international-tel
                                                  phone))
                                      ;; TODO: 本番時はこのパラメータを切る事！
                                      ("demo" "true")
                                      ))
      ;; 返り値
      ;; ・status: "ok" or "ng"
      ;; ・message": ""
      ;; ・result: "+81xxxxxxx" 登録携帯番号（twillio）
      (let* ((r (parse-json-string body))
             (status (assoc-ref r "status" #f))
             (message (assoc-ref r "message" #f))
             (result (assoc-ref r "result" #f))
             (tel-non-international
               (and result (coerce-to-non-international-tel result)))
             )
        (if (equal? "ok" status)
          (begin
            (debug-dump r)
            (return (output-errot-json!
                      (or message "twilio登録に失敗しました")))
            ;phone
            )
          tel-non-international)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (deleted? card-info)
  (equal? 'true (assoc-ref card-info "deleted")))

(define (generate-id . opts)
  (enc-like-url-shortener
    (auto-increment-dbm-entry! "ai-card-id" 1000)))

(define (generate-tid . opts)
  (receive (epoch micro) (sys-gettimeofday)
    (format "~d.~6,,,'0a" epoch (x->string micro))))

(define (generate-bid . opts)
  (random-source-randomize! default-random-source) ; tcpcgi-server初期値固定問題対策のクイックハック
  (x->string (uuid4)))

(define (generate-uid . opts)
  (x->string (uuid4)))

(define (output-raw-json! result-obj)
  (guard (e (else
              (exception-dump e)
              (output-errot-json! "内部エラーが発生しました")))
    (let* ((result-json (construct-json-string result-obj)))
      (debug-dump result-obj) ; デバッグ用
      (list
        (cgi-header :content-type "application/json; charset=utf-8"
                    :pragma "no-cache"
                    :cache-control "no-cache"
                    )
        result-json))))

(define (output-json! result-obj)
  (output-raw-json! `(("status" . "ok")
                      ,@result-obj)))

(define (output-errot-json! error-str)
  (output-raw-json! `(("status" . "error")
                      ("error" . ,error-str))))



(define (enc-like-url-shortener seed-num)
  ;; 当初は、カードidは短縮URL風の文字列にする事を考えたが、
  ;; 実際にカードを見た時に番号を表示するとの事なので、
  ;; 数値のままにする事にした
  ;(number->string seed-num 36)
  (number->string seed-num)
  )


(define (auto-increment-dbm-entry! key :optional (mzero 0))
  (with-dbm-write-lock
    *dbmwl-sys*
    (lambda ()
      (let* ((dbm (get-inner-dbm *dbmwl-sys* :write))
             (new-val (+ 1 (dbm-get dbm key mzero))))
        (dbm-put! dbm key new-val)
        new-val))))


;;;===================================================================

(define (calc-dist src-x src-y dst-x dst-y)
  ;; NB: http://emiyou3-tools.appspot.com/geocoding/distance.html
  ;;     を元にした
  (let* (
         (a-lat (* src-y pi 1/180))
         (a-lon (* src-x pi 1/180))
         (b-lat (* dst-y pi 1/180))
         (b-lon (* dst-x pi 1/180))
         (latave (/ (+ a-lat b-lat) 2))
         (sin-latave-square (expt (sin latave) 2))
         (latidiff (- a-lat b-lat))
         (longdiff (- a-lon b-lon))
         (eccentricity-factor (- 1 (* 0.006694 (expt (sin latave) 2))))
         (meridian (/ 6335439 (sqrt (expt eccentricity-factor 3))))
         (primevertical (/ 6378137 (sqrt eccentricity-factor)))
         (x (* meridian latidiff))
         (y (* primevertical (cos latave) longdiff))
         )
    (sqrt (+ (expt x 2) (expt y 2)))))

(define (get-petlist sess params)
  ;; カード一覧の取得。誰でも行える通常版と、
  ;; ブリーダーとして自カード一覧を取得するブリーダーモードがある
  ;; 未登録/未ログイン状態でブリーダーモードを見ても空になる
  (let/cc return
    (let* ((b (cgi-get-parameter "b" params :default ""))
           (breeder-mode? (not (equal? b "")))
           (sess-bid (and breeder-mode? (get-keyword :bid sess #f)))
           (x-str (cgi-get-parameter "x" params :default ""))
           (x (string->number x-str))
           (_ (when (and (not breeder-mode?) (not x))
                (return (output-errot-json! "経度情報に不備があります"))))
           (y-str (cgi-get-parameter "y" params :default ""))
           (y (string->number y-str))
           (_ (when (and (not breeder-mode?) (not y))
                (return (output-errot-json! "緯度情報に不備があります"))))
           (breeder-info-cache (make-hash-table 'equal?))
           (get-b+d (lambda (bid)
                      (or
                        (hash-table-get breeder-info-cache bid #f)
                        (let* ((breeder-info (dbm-get *dbmwl-breeder* bid '()))
                               (b-x-str (assoc-ref breeder-info "x" #f))
                               (b-y-str (assoc-ref breeder-info "y" #f))
                               ;; TODO: この時点で距離の簡易判定を行い、
                               ;;       確実に遠いと分かるものはdistに
                               ;;       +inf.0をつっこんでもよい
                               (dist (cond
                                       (breeder-mode? 0)
                                       ((not b-x-str) +inf.0)
                                       ((not b-y-str) +inf.0)
                                       (else
                                         (calc-dist
                                           x y
                                           (or (string->number b-x-str) 0)
                                           (or (string->number b-y-str) 0)))))
                               (b+d (list breeder-info dist)))
                          (hash-table-put! breeder-info-cache bid b+d)
                          b+d))))
           ;; dbmから条件に合うもの全てを取り出す
           (r (with-dbm-read-lock
                *dbmwl-card*
                (lambda ()
                  (let ((dbm (get-inner-dbm *dbmwl-card* :read)))
                    (dbm-fold
                      dbm
                      (lambda (id card-info prev)
                        (let* ((bid (assoc-ref card-info "bid"))
                               (b+d (get-b+d bid))
                               (breeder-info (car b+d))
                               (dist (cadr b+d))
                               )
                          (cond
                            ;; ブリーダーモード
                            ((identity breeder-mode?)
                             (if (equal? sess-bid bid)
                               (cons (list card-info breeder-info dist) prev)
                               prev))
                            ;; 削除判定
                            ((deleted? card-info) prev)
                            ;; 距離判定
                            ((< *match-radius-m* dist) prev)
                            (else
                              ;; card-info, breeder-info, distを結果に加える
                              (cons (list card-info breeder-info dist) prev)))))
                      '())))))
           (sorted-r (sort
                       r
                       (if breeder-mode?
                         (lambda (c+b+d1 c+b+d2)
                           ;; tid逆順
                           (string<?  (assoc-ref (car c+b+d2) "tid")
                                      (assoc-ref (car c+b+d1) "tid")))
                         (lambda (c+b+d1 c+b+d2)
                           ;; 距離が近い順でソートする
                           ;; ただし距離が同じ(bidもしくは郵便番号が同じ)
                           ;; 場合は、tidが大きいものを前にする
                           (let* ((dist1 (caddr c+b+d1))
                                  (dist2 (caddr c+b+d2))
                                  )
                             (if (= dist1 dist2)
                               (string<?  (assoc-ref (car c+b+d2) "tid")
                                          (assoc-ref (car c+b+d1) "tid"))
                               (< dist1 dist2)))))))
           (taken-r (if breeder-mode?
                      sorted-r
                      (take* sorted-r *cards-list-max*)))
           ;; NB: idのリストではなく、card-infoのまま送る事になった
           ;; TODO: この部分をどうにかしてget-petinfoと共通化する
           (id-list (map (lambda (c+b+d)
                           (let* ((card-info (car c+b+d))
                                  (breeder-info (cadr c+b+d))
                                  (c (lambda (k)
                                       (assoc-ref card-info
                                                  (keyword->string k) 'null)))
                                  (b (lambda (k)
                                       (assoc-ref breeder-info
                                                  (keyword->string k) 'null)))
                                  (r `(("id" . ,(c :id))
                                       ("tid" . ,(c :tid))
                                       ("cat" . ,(c :cat))
                                       ("img_url" . ,(c :img_url))
                                       ("img_url2" . ,(c :img_url2))
                                       ("img_url3" . ,(c :img_url3))
                                       ("age" . ,(c :age))
                                       ("name" . ,(c :name))
                                       ("comment" . ,(c :comment))
                                       ("address" . ,(b :address))
                                       ("breeder_name" . ,(b :name))
                                       ("x" . ,(b :x))
                                       ("y" . ,(b :y))
                                       ;("bid" . ,(b :bid))
                                       ;("tel" . ,(b :tel))
                                       ("tel_twilio" . ,(filter-bl-tel (b :tel_twilio)))
                                       ;; 以下は古い仕様のもの
                                       ;("cat_name" . ,(c :cat))
                                       ;("place" . ,(b :address))
                                       )))
                             r))
                         taken-r))
           )
      (output-json!
        `(;("tid" . ,(generate-tid))
          ("petlist" . ,(apply vector id-list)))))))


(define (get-petinfo sess params)
  ;; カード情報の取得。誰でも行える
  (let* ((id (cgi-get-parameter "id" params :default ""))
         (card-info (dbm-get *dbmwl-card* id #f)))
    (cond
      ((not card-info) (output-errot-json! "情報が存在しません"))
      ;; 論理削除されている場合は見れない
      ((deleted? card-info)
       (output-errot-json! "このカードは既に削除されています"))
      (else
        (let* ((g (lambda (k)
                    (assoc-ref card-info (keyword->string k) 'null)))
               (bid (g :bid))
               (breeder-info (case bid
                               ((null) '())
                               ((#f) '())
                               ((()) '())
                               (else
                                 (dbm-get *dbmwl-breeder* bid '()))))
               (b (lambda (k)
                    (assoc-ref breeder-info (keyword->string k) 'null)))
               (address (b :address))
               (breeder-name (b :name))
               (x (b :x))
               (y (b :y))
               (tel (b :tel))
               (r `(("id" . ,(g :id))
                    ("tid" . ,(g :tid))
                    ("cat" . ,(g :cat))
                    ("img_url" . ,(g :img_url))
                    ("img_url2" . ,(g :img_url2))
                    ("img_url3" . ,(g :img_url3))
                    ("age" . ,(g :age))
                    ("name" . ,(g :name))
                    ("comment" . ,(g :comment))
                    ("address" . ,address)
                    ("breeder_name" . ,breeder-name)
                    ("x" . ,x)
                    ("y" . ,y)
                    ;("bid" . ,bid)
                    ;("tel" . ,tel)
                    ("tel_twilio" . ,(filter-bl-tel (b :tel_twilio)))
                    ;; 以下は古い仕様のもの
                    ;("cat_name" . ,(g :cat))
                    ;("place" . ,address)
                    )))
          (output-json! r))))))

(define (get-breeder sess params)
  ;; ブリーダー情報の取得。自分のものしか取得はできない。
  ;; はじめてブリーダーとしてアクセスした場合は各部にはnullが入った状態で返る。
  (let* ((bid (get-keyword :bid sess #f))
         (breeder-info (if bid (dbm-get *dbmwl-breeder* bid '()) '()))
         (g (lambda (k)
              (assoc-ref breeder-info (keyword->string k) 'null)))
         (r `(("bid" . ,(or bid 'null))
              ("tid" . ,(g :tid))
              ("name" . ,(g :name))
              ("postal" . ,(g :postal))
              ("tel" . ,(g :tel))
              ("tel_twilio" . ,(filter-bl-tel (g :tel_twilio)))
              ("address" . ,(g :address))
              )))
    (output-json! r)))

(define tr:zen->han
  (build-transliterator
    "Ａ-Ｚａ-ｚ０-９\u3000！＃＄％＆’（）＊＋，－．／：；＜＝＞？＠［］＾＿｛｜｝ー"
    "A-Za-z0-9 !#$%&'()\\*+,\\-./:;<=>?@[]^_{|}\\-"))
(define (zen->han str)
  (with-string-io str tr:zen->han))
(define (reform-tel src)
  (let* ((r1 (string-trim-both (zen->han src)))
         ;; ハイフンを除去する
         (r2 (regexp-replace-all #/\-/ r1 ""))
         (rend (regexp-replace-all #/\-/ r1 "")))
    rend))
(define (reform-postal src)
  (let* ((r1 (string-trim-both (zen->han src)))
         ;; "123-4567" => "1234567"
         (rend (regexp-replace-all #/\-/ r1 "")))
    rend))

(define (check-tel! tel return)
  (when (zero? (string-length tel))
    (return (output-errot-json! "電話番号を入力してください")))
  (unless (#/^\+?\d+$/ tel)
    (return (output-errot-json! "電話番号が正しくありません")))
  (when (< 12 (string-length tel))
    (return (output-errot-json! "電話番号が長すぎます")))
  (when (< (string-length tel) 8)
    (return (output-errot-json! "電話番号が短かすぎます")))
  ;; TODO: SMSを送信する場合、携帯である事が必須になる
  ;(unless (#/^0[05789]0/ tel)
  ;  (return (output-errot-json! "携帯電話番号を入力して下さい")))
  #t)

(define (post-breeder sess params :optional (gen-mode? #f))
  ;; ブリーダー情報の登録/更新。自分のものしか変更できない。
  (let/cc return
    (let* (
           ;; まずパラメータの検証を行う。問題があった場合は、
           ;; (return (output-errot-json! "...")) で終了する
           (name (cgi-get-parameter "name" params :default ""))
           (_ (when (string=? "" name)
                (return (output-errot-json! "名前が入力されていません"))))
           (_ (when (< 32 (string-length name))
                (return (output-errot-json! "名前が長すぎます"))))
           (tel (reform-tel (cgi-get-parameter "tel" params :default "")))
           (_ (check-tel! tel return))
           (postal (reform-postal
                     (cgi-get-parameter "postal" params :default "")))
           (_ (when (zero? (string-length postal))
                (return (output-errot-json! "郵便番号を入力してください"))))
           (_ (unless (is-postal-valid? postal)
                (return (output-errot-json! "郵便番号が正しくありません"))))
           (geo (postal->geoapi-result postal gen-mode?))
           (_ (unless geo
                (return (output-errot-json!
                          "郵便番号から住所情報が取得できません"))))
           (x+y (receive r (geo->x+y geo) r))
           (x (car x+y))
           (y (cadr x+y))
           (address (geo->address geo))
           (had-bid? (get-keyword :bid sess #f))
           (old-breeder-info
             (if had-bid? (dbm-get *dbmwl-breeder* had-bid? '()) '()))
           (bid (or had-bid? (generate-bid))) ; 既にあるならそれを使う、そうでなければ生成
           ;; twilio登録(ダミー登録および電話番号無変化なら何もしない)
           (old-tel (assoc-ref old-breeder-info "tel" #f))
           (tel-twilio (if (or gen-mode?  (equal? tel old-tel))
                         tel
                         (post-k5m "breeder" tel return)))
           (breeder-info `(("bid" . ,bid)
                           ("tid" . ,(generate-tid))
                           ("name" . ,name)
                           ("tel" . ,tel)
                           ("tel_twilio" . ,(x->string tel-twilio))
                           ("postal" . ,postal)
                           ("address" . ,address)
                           ("x" . ,x)
                           ("y" . ,y)
                           ("auth_type" . null) ; 将来拡張用
                           ("auth_info" . null) ; 将来拡張用
                           ;; TODO: 内部で保持すべき情報はこれだけか？
                           ;;       もっと必要そうだが…
                           ))
           (r `(("bid" . ,bid)))) ; 登録/更新での返り値はbidのみでok

      ;; 新規登録の場合、セッションにbidを保存
      (when (and (not had-bid?) (not gen-mode?))
        (cgi-update-session! *cgi-session*
                             (list* :bid bid (delete-keyword :bid sess))))
      ;; dbmに保存
      (dbm-put! *dbmwl-breeder* bid breeder-info)
      ;; 結果を返す
      (output-json! r))))

(define (delete-petinfo sess params)
  (let/cc return
    (let* (
           (bid (get-keyword :bid sess #f))
           (_ (unless bid
                (return (output-errot-json!
                          "未登録もしくは未ログイン状態です"))))
           (id (let1 a (cgi-get-parameter "id" params :default "")
                 (if (equal? a "") #f a)))
           (_ (unless id
                (return (output-errot-json! "カードが指定されていません"))))
           (old-card-info (dbm-get *dbmwl-card* id #f))
           (_ (when (not old-card-info)
                (return (output-errot-json! "指定のカードは存在しません"))))
           (_ (when (and old-card-info (deleted? old-card-info))
                (return (output-errot-json!
                          "このカードは既に削除されています"))))
           (_ (when (and
                      old-card-info
                      (not (equal? bid (assoc-ref old-card-info "bid" #f))))
                (return (output-errot-json! "カードの所有者が違います"))))
           (g (lambda (k)
                (assoc-ref old-card-info (keyword->string k) 'null)))
           (del-img! (lambda (path)
                       (when path
                         (sys-unlink path))))
           )
      ;; まずカード内の画像を消す
      ;; 真面目に実装すると大変なので、idに対応するファイルだけ消す
      (del-img! (string-append *img-dir* "/" id ".jpg"))
      (del-img! (string-append *img-dir* "/" id "_2.jpg"))
      (del-img! (string-append *img-dir* "/" id "_3.jpg"))
      ;; エントリのバックアップ
      ;; TODO: あとで
      ;; エントリ物理削除
      ;; TODO: 論理削除にすべきか？
      (dbm-delete! *dbmwl-card* id)
      (output-json! '()))))

(define (is-image? path)
  (#/^image/ (process-output->string
               (string-append
                 "file " path " | cut -d ' ' -f 3"))))

(define *tmp-prefix-re*
  (string->regexp
    (string-append "^" (regexp-quote *tmp-prefix*))))

(define (process-img-path src-fullpath id suffix return)
  (cond
    ((not src-fullpath) #f) ; 画像はアップロードされていない(処理せず終了)
    ((not (*tmp-prefix-re* src-fullpath)) ; 不正アップロード
     (return (output-errot-json! "アップロード手法が正しくありません")))
    ((not (is-image? src-fullpath)) ; 画像ファイルでない
     (return (output-errot-json! "アップロードファイルは画像でありません")))
    ;((too-big? src-fullpath) ...) ; TODO: サイズ制限する
    (else
      (let* (
             (filename (string-append id suffix)) ; 生成後のファイル名
             ;; resize.scm生成先
             (resized-fullpath (string-append *tmp-prefix* "-_-" filename))
             ;; 最終生成先
             (target-fullpath (string-append *img-dir* "/" filename))
             ;; img_url書式のpath生成
             (img-url (string-append *img-url-prefix* filename))
             )
        ;; 念の為、ファイルが存在しない事を保証する
        (sys-unlink resized-fullpath)
        ;; リサイズする ( /path/to/resize.scm orig.png result.jpg )
        (sys-system (string-append *resize-scm-path*
                                   " "
                                   src-fullpath
                                   " "
                                   resized-fullpath
                                   " >> /dev/null 2>&1"))
        ;; 正常にリサイズできたか調べる
        (unless (file-exists? resized-fullpath)
          (output-errot-json! "ファイルのリサイズに失敗しました"))
        ;; 正常にリサイズできてたので、適切な場所に設置する
        (move-file resized-fullpath target-fullpath :if-exists :supersede)
        ;; umaskが不適切な場合があるようなので変更する
        (sys-chmod target-fullpath #o644)
        ;; img_url に収められる形式のpathを返す
        img-url))))


(define (post-petinfo sess params)
  ;; カード情報の登録/更新。自分のものしか変更できない。
  (let/cc return
    (let* (
           ;; まずパラメータの検証を行う。問題があった場合は、
           ;; (return (output-errot-json! "...")) で終了する
           ;; この機能はブリーダー登録を先に済ませてないと使えない
           (bid (get-keyword :bid sess #f))
           (_ (unless bid
                (return (output-errot-json!
                          "未登録もしくは未ログイン状態です"))))
           ;; カードid検証
           (id (let1 a (cgi-get-parameter "id" params :default "")
                 (if (equal? a "") #f a)))
           (old-card-info (and id (dbm-get *dbmwl-card* id #f)))
           (_ (when (and id (not old-card-info))
                (return (output-errot-json! "指定のカードは存在しません"))))
           (_ (when (and old-card-info (deleted? old-card-info))
                (return (output-errot-json!
                          "このカードは既に削除されています"))))
           (_ (when (and
                      old-card-info
                      (not (equal? bid (assoc-ref old-card-info "bid" #f))))
                (return (output-errot-json! "カードの所有者が違います"))))
           ;; 削除モードならこの時点でディスパッチ
           (deleted? (cgi-get-parameter "delete" params :default #f))
           (_ (when deleted?
                (return (delete-petinfo sess params))))
           (cat (cgi-get-parameter "cat" params :default ""))
           (_ (when (string=? "" cat)
                (return (output-errot-json! "種別が入力されていません"))))
           (_ (when (< 32 (string-length cat))
                (return (output-errot-json! "種別が長すぎます"))))
           (age (cgi-get-parameter "age" params :default ""))
           (_ (when (string=? "" age)
                (return (output-errot-json! "年齢が入力されていません"))))
           (_ (when (< 32 (string-length age))
                (return (output-errot-json! "年齢が長すぎます"))))
           (name (cgi-get-parameter "name" params :default ""))
           (_ (when (string=? "" name)
                (return (output-errot-json! "名前が入力されていません"))))
           (_ (when (< 32 (string-length name))
                (return (output-errot-json! "名前が長すぎます"))))
           (comment (cgi-get-parameter "comment" params :default ""))
           ;(_ (when (string=? "" comment)
           ;     (return (output-errot-json! "特徴が入力されていません"))))
           (_ (when (< 160 (string-length comment))
                (return (output-errot-json! "特徴が長すぎます"))))
           ;; ↓はデバッグ用に、直に画像URLを指定する為のもの
           (img-url-force
             (cgi-get-parameter "img_url_force" params :default ""))
           ;; この段階でidが必要な為、新規生成の場合はidを生成しておく
           ;; (この後で失敗する場合があり、その場合はidに飛びが
           ;;  発生するが、仕方がない)
           (id-true (or id (generate-id)))
           ;; ここで画像の処理を行う。returnで途中エラー終了する場合あり
           (img-path (cgi-get-parameter "img" params :default #f))
           (img-path2 (cgi-get-parameter "img2" params :default #f))
           (img-path3 (cgi-get-parameter "img3" params :default #f))
           ;; 以下のresultは #f もしくは img_url に収められる値
           (img-result (process-img-path img-path id-true ".jpg" return))
           (img-result2 (process-img-path img-path2 id-true "_2.jpg" return))
           (img-result3 (process-img-path img-path3 id-true "_3.jpg" return))
           )
      (let* (
             ;; NB: 古い画像のurlとうまいことmergeする必要がある
             (old-img-url
               (and old-card-info (assoc-ref old-card-info "img_url" 'null)))
             (old-img-url2
               (and old-card-info (assoc-ref old-card-info "img_url2" 'null)))
             (old-img-url3
               (and old-card-info (assoc-ref old-card-info "img_url3" 'null)))
             (img-url (if (equal? "" img-url-force)
                        (or img-result old-img-url 'null)
                        img-url-force))
             (img-url2 (or img-result2 old-img-url2 'null))
             (img-url3 (or img-result3 old-img-url3 'null))
             (tid (generate-tid))
             (new-card-info `(("id" . ,id-true)
                              ("tid" . ,tid)
                              ("cat" . ,cat)
                              ("age" . ,age)
                              ("name" . ,name)
                              ("comment" . ,comment)
                              ("bid" . ,bid)
                              ("img_url" . ,img-url)
                              ("img_url2" . ,img-url2)
                              ("img_url3" . ,img-url3)
                              ))
             )
        (dbm-put! *dbmwl-card* id-true new-card-info)
        ;; 結果を返す
        (output-json! `(("id" . ,id-true)
                        ("tid" . ,tid)
                        ("img_url" . ,img-url)
                        ("img_url2" . ,img-url2)
                        ("img_url3" . ,img-url3)
                        ))))))

(define (post-userinfo sess params)
  (let/cc return
    (let* (
           (had-uid? (get-keyword :uid sess #f))
           (uid (or had-uid? (generate-uid)))
           (old-user-info
             (if had-uid? (dbm-get *dbmwl-user* had-uid? '()) '()))
           (old-tel (assoc-ref old-user-info "tel" #f))
           (tel (reform-tel (cgi-get-parameter "tel" params :default "")))
           (_ (check-tel! tel return))
           (tid (generate-tid))
           ;; twilio登録(電話番号無変化なら何もしない)
           (tel-twilio (if (equal? tel old-tel)
                         tel
                         (post-k5m "user" tel return)))
           (r `(("uid" . ,uid)
                ("tid" . ,tid)
                ))
           )
      ;; 新規登録の場合、セッションにuidを保存
      (unless had-uid?
        (cgi-update-session! *cgi-session*
                             (list* :uid uid (delete-keyword :uid sess))))
      ;; dbmに保存
      (dbm-put! *dbmwl-user* uid `(("uid" . ,uid)
                                   ("tid" . ,tid)
                                   ("tel" . ,tel)
                                   ("tel_twilio" . ,tel-twilio)
                                   ))
      ;; 結果を返す
      (output-json! r))))

(define (get-userinfo sess params)
  (let* (
         (uid (get-keyword :uid sess #f))
         (user-info (if uid (dbm-get *dbmwl-user* uid '()) '()))
         (g (lambda (k)
              (assoc-ref user-info (keyword->string k) 'null)))
         (r `(("uid" . ,(or uid 'null))
              ("tid" . ,(g :tid))
              ("tel" . ,(g :tel))
              )))
    (output-json! r)))

;;;===================================================================

(define (emit-content params)
  (cgi-with-session
    *cgi-session*
    (lambda (sess-orig)
      (let* ((sess (or sess-orig (begin
                                   (cgi-create-session! *cgi-session* '())
                                   '())))
             (path-info-keylist (or (get-path-info-keylist) '()))
             (cmd (and
                    (not (null? path-info-keylist))
                    (make-keyword (car path-info-keylist))))
             ;(c (make-keyword (cgi-get-parameter "c" params :default "")))
             (post? (equal? "POST" (cgi-get-metavariable "REQUEST_METHOD")))
             )
        ;(debug-dump sess)
        (if post?
          (case cmd
            ((:breeder) (post-breeder sess params))
            ((:petinfo) (post-petinfo sess params))
            ((:userinfo) (post-userinfo sess params))
            (else (output-errot-json! "この操作はできません")))
          (case cmd
            ((:petlist) (get-petlist sess params))
            ((:petinfo) (get-petinfo sess params))
            ((:breeder) (get-breeder sess params))
            ((:userinfo) (get-userinfo sess params))
            (else (output-errot-json! "この操作はできません"))))))))


(define (main args)
  (set! (port-buffering (current-error-port)) :line)
  (cgi-main
    emit-content
    :on-error (lambda (e)
                ;; TODO: メール通知
                (exception-dump e)
                (output-errot-json! "内部エラーが発生しました"))
    :part-handlers `((("img" "img2" "img3")
                      file
                      :prefix ,*tmp-prefix*
                      :mode #o644
                      ))
    )
  0)

;;;===================================================================

(select-module user)
(define main (with-module st main))

;; Local variables:
;; mode: scheme
;; end:
;; vim: set ft=scheme:
