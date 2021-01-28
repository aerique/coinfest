(defsystem app
  :serial t
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :around-compile (lambda (thunk)
                    #+app-debug
                    (proclaim '(optimize (debug 3) (safety 3) (speed 0)))
                    (funcall thunk))
  :depends-on (:drakma :flexi-streams :jsown :parse-number)
  :components ((:file "qml")
               (:file "coinfest-common")
               (:file "coinfest-kraken-api")
               (:file "coinfest-meta-api")
               (:file "coinfest")
               (:file "app")))
