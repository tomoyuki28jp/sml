(in-package :cl-user)

(defpackage :sml
  (:use :cl :my-util)
  (:export :*markup-lang*
           :*indent-mode*
           :*tab-width*
           :*indent-level*
           :*output-stream*
           :*escape*
           :*safe*
           :*xml-version*
           :*encoding*
           :*doctypes*
           :*doctype*
           :doctype
           :safe
           :escape
           :p
           :pe
           :pr
           :indent
           :sml->ml
           :tag
           :/
           :input
           :form
           :multipart-form
           :input-text
           :input-file
           :submit
           :input-checked
           :select-form
           :define-template
           :read-sml
           :load-sml
           :with-template
           :with-sml-file
           ))
