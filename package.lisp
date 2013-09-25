;;;; package.lisp

(in-package :cl-user)

(defpackage #:smacktutor
  (:nicknames :stutor)
  (:use #:cl #:alexandria)
  (:export #:tutor
           #:tutorial
           #:lisp-env
           #:load-tutorial
           #:*active-tutor*))

