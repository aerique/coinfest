;;;; coinfest.lisp

(in-package :cl-user)


;;; Package

(defpackage :coinfest
  (:nicknames :cf)
  (:use :cl)
  (:export :add-ticker :delete-ticker :get-ticker-refresh
           :get-ticker-refresh-for-combobox :set-ticker-refresh :read-config
           :read-tickers :refresh-tickers :set-models :set-overview-model
           :ticker-id :ticker-display-name :ticker-exchange :ticker-price
           :ticker-previous-price :ticker-timestamp :ticker-previous-timestamp
           :update-tickers-model))

(in-package :coinfest)


;;; Globals

(defparameter *ticker-refresh* 7200)  ; seconds

(defparameter *exchanges-model* '("Kraken"))
(defparameter *tickers-model* '())  ; set again at `kraken-tickers-for-model`

(defparameter *internal-tickers* '())
(defparameter *overview-model*   *internal-tickers*)

;; only initialized once per app run
(defparameter *kraken-currency-pairs* nil)


;;; Models

(defun set-overview-model ()
  (setf *overview-model* (tickers-for-overview))
  (eql:qlet ((data (eql:qvariant-from-value *overview-model* "QStringList")))
    (eql:|setContextProperty| (qml:root-context) "overviewModel" data)))

(defparameter *update-model-p* nil)  ; Hack!

(defun update-model-p ()
  (when *update-model-p*
    (setf *update-model-p* nil)
    t))


(defun set-exchanges-model ()
  (eql:qlet ((data (eql:qvariant-from-value *exchanges-model* "QStringList")))
    (eql:|setContextProperty| (qml:root-context) "exchangesModel" data)))


(defun set-tickers-model ()
  (eql:qlet ((data (eql:qvariant-from-value *tickers-model* "QStringList")))
    (eql:|setContextProperty| (qml:root-context) "tickersModel" data)))


(defun set-models ()
  (set-overview-model)
  (set-exchanges-model)
  (set-tickers-model))


;;; Common Functions

(defun feedback (message)
  (qml:qml-set "feedbackLabel" "text" (cfc:mkstr "<br>" message))
  (qml:qml-set "feedback" "visible" t))


;;; Getters and Setters

(defun get-ticker-refresh ()
  *ticker-refresh*)


;; XXX temporary hack (2021-01-26)
;; FIXME closely coupled to UI!
(defun get-ticker-refresh-for-combobox ()
  (case (get-ticker-refresh)
    ( 1800 0)  ; 30m
    ( 7200 1)  ;  2h
    (21600 2)  ;  6h
    (43200 3)  ; 12h
    (86400 4)  ;  1d
    (otherwise -1)))


(defun set-ticker-refresh (refresh-time)
  ;; A bit blunt but I don't want to import CL-PPCRE.
  ;; XXX update: CL-PPCRE is imported by Drakma anyway
  (let ((value (parse-integer (subseq refresh-time 0
                                      (position #\space refresh-time))))
        (multiplier (cond ((or (cfc:ends-with refresh-time "minute")
                               (cfc:ends-with refresh-time "minutes"))
                           60)
                          ((or (cfc:ends-with refresh-time "hour")
                               (cfc:ends-with refresh-time "hours"))
                           (* 60 60))
                          ((or (cfc:ends-with refresh-time "day")
                               (cfc:ends-with refresh-time "days"))
                           (* 60 60 24))
                          (t (pf-feedback (mkstr "Unknown multiplier: "
                                                 refresh-time))
                             (return-from set-ticker-refresh)))))
    (setf *ticker-refresh* (* value multiplier)))
  (format t "Ticker refresh time set to ~Ds.~%" *ticker-refresh*)
  (write-config))


;;; Functions

(defun tickers-for-overview ()
  (loop for ticker in *internal-tickers*
        collect (getf ticker :uuid)))


(defun refresh-ticker (exchange currency-pair)
  (cond ((string= exchange "Kraken")
         (let* ((display-name (getf currency-pair :wsname))
                ;; FIXME this needs an error handler
                (ticker (kraken:ticker (getf currency-pair :id)))
                (price (getf (getf ticker :last-trade-closed) :price)))
           (if (numberp price)
               (list :uuid (cfc:uuid) :exchange exchange
                     :id (getf currency-pair :id) :display-name display-name
                     :quote (getf currency-pair :quote)  ; XXX not used ATM
                     :price price :previous-price nil
                     :timestamp (get-universal-time) :previous-timestamp nil)
               (feedback (cfc:mkstr "Problem getting price for " exchange "::"
                                    display-name ": " price)))))
        (t
         (feedback (cfc:mkstr "Exchange not supported: " exchange)))))


(defun kraken-currency-pairs ()
  (unless *kraken-currency-pairs*
    (setf *kraken-currency-pairs* (kraken:asset-pairs)))
  *kraken-currency-pairs*)


(defun find-internal-ticker (uuid)
  (loop for ticker in *internal-tickers*
        when (string= uuid (getf ticker :uuid))
          do (return-from find-internal-ticker ticker)))


(defun find-currency-pair-by-display-name (display-name)
  (loop for cp in (kraken-currency-pairs)
        for plst = (cdr cp)
        when (string= display-name (getf plst :wsname))
          do (return plst)))


(defun add-ticker (exchange display-name)
  (let* ((cp (find-currency-pair-by-display-name display-name))
         (internal-ticker (refresh-ticker exchange cp)))
    ;; Error already signaled by `refresh-ticker`.
    (when internal-ticker
      (push internal-ticker *internal-tickers*)
      (write-tickers)
      (set-overview-model)
      (format t "Ticker ~A::~A added.~%" exchange display-name))))


(defun delete-ticker (uuid)
  (setf *internal-tickers*
        (loop for ticker in *internal-tickers*
              for ticker-uuid = (getf ticker :uuid)
              when (string= uuid ticker-uuid)
                do (format t "Deleting ~A::~A.~%" (getf ticker :exchange)
                                                  (getf ticker :display-name))
              unless (string= uuid ticker-uuid)
                collect ticker))
  (write-tickers)
  (set-overview-model))


(defun kraken-tickers-for-model ()
  (loop with asset-pairs = (kraken-currency-pairs)
        for ap in asset-pairs
        for display-name = (getf (cdr ap) :wsname)
        when display-name collect display-name into tickers
        finally (return (sort tickers #'string<))))

;; Neither very pretty nor very nice, but now that we have the function,
;; populate `*tickers-model*`.
;;
;; Since we compile it in it will get outdated.
(setf *tickers-model* (kraken-tickers-for-model))


(defun path-to-config-file ()
  (let* ((dir (directory-namestring (eql:|writableLocation.QStandardPaths|
                                     eql:|QStandardPaths.AppConfigLocation|)))
         (path (cfc:mkstr dir "config.lisp")))
    (ensure-directories-exist path)
    path))


(defun path-to-tickers-file ()
  (let* ((dir (directory-namestring (eql:|writableLocation.QStandardPaths|
                                     eql:|QStandardPaths.AppDataLocation|)))
         (path (cfc:mkstr dir "tickers.lisp")))
    (ensure-directories-exist path)
    path))


(defun read-config ()
  (let ((cfg (path-to-config-file)))
    (format t "Reading config ~S... " cfg)
    (finish-output)
    (if (probe-file cfg)
        (progn (load cfg)
               (format t "Config read.~%"))
        (format t "No config file found.~%" cfg))))


(defun write-config ()
  (let ((cfg (path-to-config-file)))
    (format t "Writing config ~S... " cfg)
    (finish-output)
    (with-open-file (f cfg :direction :output :if-exists :supersede)
      (format f "(in-package :coinfest)~%~%~
                 (defparameter *ticker-refresh* ~S)~%"
              *ticker-refresh*)))
  (format t "Config written.~%"))


(defun read-tickers ()
  (let ((tickers-file (path-to-tickers-file)))
    (format t "Reading tickers from ~S... " tickers-file)
    (finish-output)
    (if (probe-file tickers-file)
        (progn (load tickers-file)
               (format t "~D tickers read.~%"
                       (length *internal-tickers*))
               (set-overview-model))
        (format t "No tickers file found.~%" tickers-file))))


(defun write-tickers ()
  (let ((tickers-file (path-to-tickers-file)))
    (format t "Writing tickers to ~S... " tickers-file)
    (finish-output)
    (with-open-file (f tickers-file :direction :output :if-exists :supersede)
      (format f "(in-package :coinfest)~%~%~
                 (defparameter *internal-tickers*~%  '~S)~%"
              *internal-tickers*)))
  (format t "~D tickers written.~%" (length *internal-tickers*)))


(defun refresh-tickers-thread ()
  (format t "Refreshing all tickers:~%")
  (setf *internal-tickers*
        (loop for internal-ticker in *internal-tickers*
              for exchange = (getf internal-ticker :exchange)
              for dpn      = (getf internal-ticker :display-name)
              for cp       = (find-currency-pair-by-display-name dpn)
              for plst     = (refresh-ticker exchange cp)
              when plst do (format t "- ~A~%" dpn)
                           (setf (getf plst :previous-price)
                                 (getf internal-ticker :price))
                           (setf (getf plst :previous-timestamp)
                                 (getf internal-ticker :timestamp))
              when plst collect plst))
  (write-tickers)  ; FIXME do this at app close
  ;(set-overview-model)      ; does not work from a (different) thread
  (setf *update-model-p* t)  ; so we have this hack
  (qml:qml-set "busy_label" "running" nil))

(defun refresh-tickers ()
  (qml:qml-set "busy_label" "text" "Refreshing all tickers")
  (qml:qml-set "busy_label" "running" t)
  ;; Fire off a thread so we don't block the GUI.  We use ECL thread code and
  ;; not a portable thread library since EQL5 is ECL-only anyway.  Startup time
  ;; is getting long enough as it is already.
  (mp:process-run-function "refresh tickers" #'refresh-tickers-thread))


(defun ticker-property (uuid property)
  (let ((ticker (find-internal-ticker uuid)))
    (if ticker
        (let ((property (getf ticker property)))
          (if property
              property
              "Unknown Property"))
        (feedback (cfc:mkstr "Cannot find ticker: " uuid)))))


(defun ticker-id (uuid)
  (ticker-property uuid :id))


(defun ticker-display-name (uuid)
  (ticker-property uuid :display-name))


(defun ticker-exchange (uuid)
  (ticker-property uuid :exchange))


(defun ticker-price (uuid)
  (format nil "~,F" (ticker-property uuid :price)))


(defun ticker-previous-price (uuid)
  (let ((price (ticker-property uuid :previous-price)))
    (if (and (stringp price)
             (string= price "Unknown Property"))
        "  -"  ; known unknown
        (format nil "~,F" (ticker-property uuid :previous-price)))))


(defun ticker-timestamp (uuid)
  (cfc:universal-time-to-ymdhms (ticker-property uuid :timestamp)))


(defun ticker-previous-timestamp (uuid)
  (let ((ts (ticker-property uuid :previous-timestamp)))
    (if (and (stringp ts)
             (string= ts "Unknown Property"))
        ""     ; known unknown
        (cfc:universal-time-to-ymdhms ts))))


(defun update-tickers-model (exchange)
  (cond ((string= exchange "Kraken")
         (setf *tickers-model* (kraken-tickers-for-model))
         (set-tickers-model))
        (t
         (feedback (cfc:mkstr "Unsupported exchange: " exchange)))))
