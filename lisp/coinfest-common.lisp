;;;; common.lisp

(in-package :cl-user)

(setf *read-default-float-format* 'double-float)


;;; Package

(defpackage :common
  (:nicknames :cfc)
  (:use :cl :parse-number)
  (:export ;; constants and variables
           :+unix-epoch-as-universal-time+
           :*default-timeout* :*log-level* :*verbose*
           ;; logging functions
           :logmsg
           ;; functions
           :convert-type :defalias :encode-datetime-string :ends-with :fl8 :hms
           :json2plist :kw :make-keyword :mkstr :starts-with :string-after
           :string-before :universal-time-to-ymdhms :unix-time :uuid :ymdhms))

(in-package :common)


;;; Globals

(defparameter *log-level* 1)
(defvar       *verbose*   nil)

(defparameter *default-timeout* 32)  ; FIXME too long for mobile

;; https://en.wikipedia.org/wiki/Unix_time
(defvar +unix-epoch-as-universal-time+
        (encode-universal-time 0 0 0 1 1 1970 0))

;; XXX should become `defvar` once we've verified this
;; https://tools.ietf.org/html/rfc4122#section-4.2.1
;; (The date of Gregorian reform to the Christian calendar.)
(defparameter +uuid-base+ (encode-universal-time 0 0 0 15 10 1582 0))

;; FIXME downcase before compares to get rid of duplicates
(defparameter *types* '(("amount"             . float)
                        ("Ask"                . float)
                        ("AskPrice"           . float)
                        ("askPrice"           . float)
                        ("askQty"             . float)
                        ("asks"               . asks-or-bids)
                        ("BaseCurrencyID"     . integer)
                        ("Basevolume"         . float)
                        ("baseVolume"         . float)
                        ("Bid"                . float)
                        ("BidPrice"           . float)
                        ("bidPrice"           . float)
                        ("bidQty"             . float)
                        ("bids"               . asks-or-bids)
                        ("BTCVolume"          . float)
                        ("BuyOrderCount"      . integer)
                        ("category"           . keyword)
                        ("Change"             . float)
                        ("commission"         . float)
                        ("cost"               . float)
                        ("date"               . ymdhms)
                        ("fee"                . float)
                        ("free"               . float)
                        ("high"               . float)
                        ("high24hr"           . float)
                        ("highestBid"         . float)
                        ("HighPrice"          . float)
                        ("highPrice"          . float)
                        ("isFrozen"           . boolean)
                        ("Last"               . float)
                        ("last"               . float)
                        ("LastPrice"          . float)
                        ("lastPrice"          . float)
                        ("lastQty"            . float)
                        ("limitprice"         . float)
                        ("locked"             . float)
                        ("low"                . float)
                        ("low24hr"            . float)
                        ("lowestAsk"          . float)
                        ("LowPrice"           . float)
                        ("lowPrice"           . float)
                        ("makerFee"           . float)
                        ("margin"             . float)
                        ("marginMakerFee"     . float)
                        ("marginTakerFee"     . float)
                        ("MarketAssetID"      . integer)
                        ("MarketID"           . integer)
                        ("MarketName"         . keyword)
                        ("nextTier"           . float)
                        ("OpenBuyOrders"      . integer)
                        ("openPrice"          . float)
                        ("orderNumber"        . integer)
                        ("OpenSellOrders"     . integer)
                        ("ordertype"          . keyword)
                        ("pair"               . keyword)
                        ("percentChange"      . float)
                        ("prevClosePrice"     . float)
                        ("PrevDay"            . float)
                        ("price"              . float)
                        ("price2"             . float)
                        ("priceChange"        . float)
                        ("priceChangePercent" . float)
                        ("qty"                . float)
                        ("rate"               . float)
                        ("quoteVolume"        . float)
                        ("SellOrderCount"     . integer)
                        ("startingAmount"     . float)
                        ("stopprice"          . float)
                        ("takerFee"           . float)
                        ("thirtyDayVolume"    . float)
                        ("total"              . float)
                        ("TradeCount"         . integer)
                        ("tradeID"            . integer)
                        ("type"               . keyword)
                        ("vol"                . float)
                        ("vol_exec"           . float)
                        ("Volume"             . float)
                        ("volume"             . float)
                        ("weightedAvgPrice"   . float)))


;;; Functions

(defun defalias (function alias)
  "Defines an alias for FUNCTION, so it can be called with ALIAS as well."
  (setf (symbol-function alias) function))


;; FIXME double-float not needed anymore since we set *READ-FLOAT-FORMAT*?
(defun convert-type (key value)
  (let ((type (cdr (assoc key *types* :test 'string=))))
    (cond ((and type (equal type 'asks-or-bids))
           (loop for lst in value
                 for aob = (first lst)
                 ;for sum = (second lst)
                 ;; not as exact but easier to read when debugging
                 for sum = (if (numberp (second lst))
                               ;; Poloniex
                               (coerce (second lst) 'double-float)
                               ;; Kraken
                               (parse-number (second lst)
                                             :float-format 'double-float))
                 collect (list (parse-number aob :float-format 'double-float)
                               sum)))
          ((and type (equal type 'boolean))
           (if (equal value "0")
               nil
               t))
          ((and type (equal type 'float) (stringp value))
           (parse-number value :float-format 'double-float))
          ((and type (equal type 'integer) (stringp value))
           (parse-integer value))
          ((and type (equal type 'keyword))
           (make-keyword value))
          ((and type (equal type 'ymdhms))
           (encode-datetime-string value))
          ;; XXX bit of hack for Binance account values
          ((and (string= key "balances") (listp value))
           (loop for obj in value
                 when (or (string/= (jsown:val obj "free")   "0.00000000")
                          (string/= (jsown:val obj "locked") "0.00000000"))
                   collect (json2plist obj)))
          (t
           value))))


(defun ends-with (sequence subsequence)
  (let ((seqlen (length sequence))
        (sublen (length subsequence)))
    (when (and (> sublen 0)
               (<= sublen seqlen))
      (equal (subseq sequence (- seqlen sublen)) subsequence))))


(defun fl8 (number)
  "Returns NUMBER as a stringified float with 8 digits."
  (format nil "~,8F" number))


(defun json2plist (json &key recurse-keys)
  (loop for key in (jsown:keywords json)
        append (list (make-keyword key)
                     (if (member key recurse-keys :test #'string=)
                         (json2plist (jsown:val json key))
                         (convert-type key (jsown:val json key))))))


(defun make-keyword (string)
  (intern (string-upcase string) :keyword))

(defalias #'make-keyword 'kw)


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


(defun starts-with (sequence subsequence)
  (let ((sublen (length subsequence)))
    (when (and (> sublen 0)
               (<= sublen (length sequence)))
      (equal (subseq sequence 0 sublen) subsequence))))


(defun string-after (substring string)
  (loop with first-substring-char = (elt substring 0)
        with substr-len = (length substring)
        for i from 0 to (- (length string) substr-len)
        for current-char = (elt string i)
        do (when (and (char= current-char first-substring-char)
                      (string= substring (subseq string i (+ i substr-len))))
             (return-from string-after (subseq string (+ i substr-len))))))


(defun string-before (substring string)
  (loop with first-substring-char = (elt substring 0)
        with substr-len = (length substring)
        for i from 0 to (- (length string) substr-len)
        for current-char = (elt string i)
        do (when (and (char= current-char first-substring-char)
                      (string= substring (subseq string i (+ i substr-len))))
             (return-from string-before (subseq string 0 i)))))


;; Losely based on https://tools.ietf.org/html/rfc4122
;; - no global lock
;; - no system-wide shared stable store
;; - no fresh `*random-state*`
;; - random node id
;; - no attention paid to version
;; This is a v1'ish UUID which is sufficient for our purpose.
;; (All CL UUID libraries I could find pulled in Ironclad, so no.  I'm
;;  seeing if I can get rid of Ironclad since it's so heavy at it might
;;  shave off (a) second(s) from the app startup time.)
(defun uuid ()
  (let* ((32-bit-mask #b11111111111111111111111111111111)
         (16-bit-mask #b1111111111111111)
         ( 8-bit-mask #b11111111)
         (current-time (+ (* 1000000000/100
                             (+ (abs +uuid-base+) (get-universal-time)))
                          ;; for a little bit more resolution
                          (get-internal-real-time)))
         (clock-sequence (random (- (expt 2 16) 1)))
         (node-id (random (- (expt 2 48) 1))))
    (string-downcase (format nil "~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X"
                             ;; time_low
                             (logand current-time 32-bit-mask)
                             ;; time_mid
                             (logand (ash current-time -32) 16-bit-mask)
                             ;; time_hi
                             (logand (ash current-time -48) 16-bit-mask)
                             ;; clk_seq_hi_res and clk_seq_low
                             clock-sequence
                             ;; node
                             node-id))))


;;; Time Functions

(defun encode-datetime-string (datetime-string)
  (let ((yyyy (parse-integer (subseq datetime-string  0  4)))
        (mon  (parse-integer (subseq datetime-string  5  7)))
        (dd   (parse-integer (subseq datetime-string  8 10)))
        (hh   (parse-integer (subseq datetime-string 11 13)))
        (mm   (parse-integer (subseq datetime-string 14 16)))
        (ss   (parse-integer (subseq datetime-string 17 19))))
    (encode-universal-time ss mm hh dd mon yyyy)))


(defun hms ()
  (multiple-value-bind (sec min hour)
      (decode-universal-time (get-universal-time))
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))


(defun universal-time-to-ymdhms (universal-time)
  (multiple-value-bind (sec min hr day mon year)
      (decode-universal-time universal-time)
    (format nil "~2,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year mon day hr min sec)))


(defun unix-time (universal-time)
  (- universal-time +unix-epoch-as-universal-time+))


(defun ymdhms ()
  (multiple-value-bind (sec min hour day mon year)
      (decode-universal-time (get-universal-time))
    (format nil "~2,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
            year mon day hour min sec)))


;;; Logging Functions

(defun logmsg (&rest args)
  (when (>= *log-level* 1)
    (let ((*print-pretty* nil))
      (format *standard-output* "~A: " (hms))
      (apply #'format (append (list *standard-output*) args)))
    (force-output *standard-output*)))
