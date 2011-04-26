;;;; qpj1.asd

(cl:in-package :asdf)

(defsystem :qpj1
  :serial t
  :depends-on (:fiveam
               :quickproject)
  :components ((:file "package")
               (:file "qpj1")))

(defmethod perform ((o test-op) (c (eql (find-system :qpj1))))
  (load-system :qpj1)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :qpj1-internal :qpj1))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

