(in-package :cl-user)

(defpackage :sml-asd (:use :cl :asdf))

(in-package :sml-asd)

(defsystem  :sml
  :name     "sml"
  :author   "Tomoyuki Matsumoto <tomoyuki28jp@no-spam@yahoo.co.jp>"
  :licence  "BSD"
  :description "s-expression markup language"
  :depends-on  (:my-util)
  :components  ((:module "src"
                         :serial  t
                         :components ((:file "package")
                                      (:file "sml")))))
