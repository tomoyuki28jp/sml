(in-package :cl-user)

(defpackage :sml
  (:use :cl :my-util)
  (:export :*markup-lang*
           :*indent-mode*
           :*tab-width*
           :*indent-level*
           :*sml-output*
           :*escape*
           :*safe*
           :*xml-version*
           :*encoding*
           :*non-break*
           :*non-breaking-tags*
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
           :find-input
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
