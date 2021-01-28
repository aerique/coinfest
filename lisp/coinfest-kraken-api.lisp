;;;; kraken-api.lisp
;;;;
;;;; https://www.kraken.com/help/api

(in-package :cl-user)


;;; Package

(defpackage :kraken
  (:use :cl :parse-number)
  (:export :asset-pairs :ohlc :ticker))

(in-package :kraken)


;;; Globals

(defparameter *api-url* "https://api.kraken.com")

;(defparameter *api-delay* 0.25)  ; wait this long between API calls
(defparameter *api-delay* 0.01)   ; mainly a concern when refreshing tickers


;;; Functions

;;; Kraken API Functions

(defun api-call (api &key (method :GET) parameters additional-headers content)
  (sleep *api-delay*)
  ;(cfc:logmsg "Making call to ~A~A (with ~S)...~%"
  ;           *api-url* api (list method parameters additional-headers
  ;                                      content))
  (handler-case
      (bt:with-timeout (cfc:*default-timeout*)
        (drakma:http-request (cfc:mkstr *api-url* api) :method method
                             :additional-headers additional-headers
                             :parameters parameters :content content))
    (t (condition) (flexi-streams:string-to-octets
                    (format nil "{\"error\": \"~A\"}" condition)))))


(defun convert-a (ticker-entry)
  (list :ask
        (list :price (parse-number (elt ticker-entry 0))
              :whole-lot-volume (parse-number (elt ticker-entry 1))
              :lot-volume (parse-number (elt ticker-entry 2)))))


(defun convert-b (ticker-entry)
  (list :bid
        (list :price (parse-number (elt ticker-entry 0))
              :whole-lot-volume (parse-number (elt ticker-entry 1))
              :lot-volume (parse-number (elt ticker-entry 2)))))


(defun convert-c (ticker-entry)
  (list :last-trade-closed
        (list :price (parse-number (elt ticker-entry 0))
              :lot-volume (parse-number (elt ticker-entry 1)))))


(defun convert-v (ticker-entry)
  (list :volume
        (list :today (parse-number (elt ticker-entry 0))
              :last-24-hours (parse-number (elt ticker-entry 1)))))


(defun convert-p (ticker-entry)
  (list :volume-weighted-average-price
        (list :today (parse-number (elt ticker-entry 0))
              :last-24-hours (parse-number (elt ticker-entry 1)))))


(defun convert-t (ticker-entry)
  (list :number-of-trades
        (list :today (elt ticker-entry 0)
              :last-24-hours (elt ticker-entry 1))))


(defun convert-l (ticker-entry)
  (list :low
        (list :today (parse-number (elt ticker-entry 0))
              :last-24-hours (parse-number (elt ticker-entry 1)))))


(defun convert-h (ticker-entry)
  (list :high
        (list :today (parse-number (elt ticker-entry 0))
              :last-24-hours (parse-number (elt ticker-entry 1)))))


(defun convert-o (ticker-entry)
  (list :opening-price
        (parse-number ticker-entry)))


(defun convert-ticker-entry (key ticker-entry)
  (cond ((equal key "a") (convert-a ticker-entry))
        ((equal key "b") (convert-b ticker-entry))
        ((equal key "c") (convert-c ticker-entry))
        ((equal key "v") (convert-v ticker-entry))
        ((equal key "p") (convert-p ticker-entry))
        ((equal key "t") (convert-t ticker-entry))
        ((equal key "l") (convert-l ticker-entry))
        ((equal key "h") (convert-h ticker-entry))
        ((equal key "o") (convert-o ticker-entry))
        (t (format *debug-io* "[convert-ticker-entry] unknown: ~S~%" key))))


;;; API Functions

(defun asset-pairs ()
  (let* ((api "/0/public/AssetPairs")
         (res (api-call api))
         (json (jsown:parse (flexi-streams:octets-to-string res))))
    (if (jsown:val json "error")
        (list :error (jsown:val json "error"))
        (loop with result = (jsown:val json "result")
             for key in (jsown:keywords result)
             collect (cons (cfc:make-keyword key)
                           (append (list :id key)
                                   (cfc:json2plist (jsown:val result key))))))))


(defun ohlc (ticker &key (interval 15) since)
  (let* ((interval (if (stringp interval) interval (cfc:mkstr interval)))
         (since (if (stringp since) since (cfc:mkstr since)))
         (api "/0/public/OHLC")
         (res (api-call api :parameters
                            (append (list (cons "pair" ticker)
                                          (cons "interval" interval))
                                    (when since
                                      (list (cons "since" since))))))
         (json (jsown:parse (flexi-streams:octets-to-string res))))
    (if (jsown:val json "error")
        (list :error (jsown:val json "error"))
        (let (;; NOTE: we assume the returned data is sorted on timestamp!
              (ohlc-data (jsown:val (jsown:val json "result") ticker)))
          (loop for entry in ohlc-data
                for ts     = (elt entry 0)
                for open   = (parse-number (elt entry 1))
                for high   = (parse-number (elt entry 2))
                for low    = (parse-number (elt entry 3))
                for close  = (parse-number (elt entry 4))
                for vwap   = (parse-number (elt entry 5))
                for volume = (parse-number (elt entry 6))
                for count  = (elt entry 7)
                collect (list :time ts :open open :high high :low low
                              :close close
                              :volume-weighted-average-price vwap
                              :volume volume :count count))))))


(defun ticker (currency-pair)
  (let* ((api "/0/public/Ticker")
         (res (api-call api :parameters `(("pair" . ,currency-pair))))
         (json (if (stringp res)
                   (jsown:new-js ("error" "Got back HTML page"))
                   (jsown:parse (flexi-streams:octets-to-string res)))))
    (if (jsown:val json "error")
        (list :error (jsown:val json "error"))
        (loop with ticker-info = (jsown:val (jsown:val json "result")
                                            currency-pair)
              for key in (jsown:keywords ticker-info)
              append (convert-ticker-entry key (jsown:val ticker-info key))))))
