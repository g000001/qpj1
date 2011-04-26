;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :qpj1
  (:export :make-project))

(defpackage :qpj1-internal
  (:use :qpj1 :cl :fiveam))

