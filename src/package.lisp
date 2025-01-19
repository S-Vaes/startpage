(defpackage :static-site
  (:use :cl :spinneret :parenscript)
  (:import-from :spinneret :with-html :html #:with-html-string #:with-html)
  (:import-from :alexandria :when-let :with-gensyms)
  (:local-nicknames (:ps :parenscript)
                    (:iter :iterate))
  (:export :generate-site))
