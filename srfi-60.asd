;;;; srfi-60.asd

(cl:in-package :asdf)


(defsystem :srfi-60
  :version "20200207"
  :description "SRFI 60 for CL: Integers as Bits"
  :long-description "SRFI 60 for CL: Integers as Bits
https://srfi.schemers.org/srfi-60"
  :author "Aubrey Jaffer"
  :maintainer "CHIBA Masaomi"
  :license "MIT"
  :serial t
  :depends-on (:fiveam)
  :components ((:file "package")
               (:file "srfi-60")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-60))))
  (let ((name "https://github.com/g000001/srfi-60")
        (nickname :srfi-60))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-60))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-60#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-60)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
