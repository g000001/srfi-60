;;;; srfi-60.asd

(cl:in-package :asdf)

(defsystem :srfi-60
  :serial t
  :depends-on (:fiveam)
  :components ((:file "package")
               (:file "srfi-60")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-60))))
  (load-system :srfi-60)
  (flet ((mksym (pkg sym)
           (intern (symbol-name sym) (find-package pkg))))
    (let ((result
           (funcall (mksym :fiveam :run) (mksym :srfi-60.internal :srfi-60))))
      (or
       (progn
        (funcall (mksym :fiveam :explain!) result)
        (funcall (mksym :fiveam :results-status) result))
       (error " test-op failed")))))
