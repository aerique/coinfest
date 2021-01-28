;;;; meta-api.lisp

(in-package :cl-user)


;;; Package

(defpackage :meta-api
  (:use :cl)
  (:export :chart-data :currency-pairs :tickers :supported-exchanges))

(in-package :meta-api)


;;; Globals

; …


;;; Functions

;; Maybe add which functions are supported by which exchange?
(defun supported-exchanges ()
  (list :kraken))


;;; Kraken Functions

(defun kraken-chart-data (currency-pair period start)
  (multiple-value-bind (asset-a asset-b)
      (values (cfc:string-before "_" currency-pair)
              (cfc:string-after  "_" currency-pair))
    (loop for plist in (kraken:ohlc asset-a asset-b :interval period
                                    :since start)
        for ts    = (getf plist :time)
        for high  = (getf plist :high)
        for low   = (getf plist :low)
        for open  = (getf plist :open)
        for close = (getf plist :close)
        for bvol  = (getf plist :volume)
        for qvol  = nil
        for wa    = (getf plist :volume-weighted-average-price)
        collect (list :timestamp ts :high high :low low :open open :close close
                      :base-volume bvol :quote-volume qvol
                      :weighted-average wa))))


(defun kraken-currency-pairs ()
  (loop for ap in (kraken:asset-pairs) by #'cddr
        collect ap))


;;; Exported Functions

;; Maybe use keywords here
;;
;; Some examples since this is a complicated function:
;; - `(meta-api:chart-data :kraken "XBT_EUR" 0 nil 15)`
;;
;; These examples show that the Meta API isn't a straight drop in replacement.
(defun chart-data (exchange currency-pair start end period)
  (declare (ignore end))  ; was used for Poloniex I think, not removing it
  (case exchange
    (:kraken   (kraken-chart-data currency-pair period start))
    (otherwise :not-supported)))


(defun currency-pairs (exchange)
  (case exchange
    (:kraken   (kraken-currency-pairs))
    (otherwise :not-supported)))


;(defun ticker (exchange)
;  (case exchange
;    (:kraken   (kraken-currency-pairs))
;    (otherwise :not-supported)))


;; For Kraken this will be very expensive.
(defun tickers (exchange)
  (declare (ignore exchange))  ; will be used once we support more than Kraken
  ;(case exchange
  ;  (otherwise :not-supported)))
  :not-supported)
