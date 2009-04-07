(in-package :sml)

(defvar *markup-lang* :xhtml
  "Marckup Language. Either :xhtml, :html or :xml")

(defvar *indent-mode* t
  "Indent mode flag")

(defvar *tab-width* 4
  "The number of spaces per indentation")

(defvar *indent-level* 0
  "Current level of indentation")

(defvar *sml-output* *standard-output*
  "Markup language output stream")

(defvar *xml-version* "1.0"
  "XML version")

(defvar *encoding* "UTF-8"
  "Encoding")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *escape* t
    "Escape mode flag")

  (defvar *safe* nil
    "Safe mode flag")

  (defvar *sml* (make-hash-table)
    "Hash table where the defined sml template are set to"))

; --- Doctype ---------------------------------------------------

(defvar *doctypes*
  '((:xhtml ((:strict . 
"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
             (:transitional . 
"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
             (:frameset . 
"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">")))
    (:html  ((:strict . 
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
\"http://www.w3.org/TR/html4/strict.dtd\">")
             (:transitional . 
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
\"http://www.w3.org/TR/html4/loose.dtd\">")
             (:frameset . 
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"
\"http://www.w3.org/TR/html4/frameset.dtd\">")))))

(defvar *doctype* :transitional
  "Default doctype")

(defun doctype ()
  (concat (when (member *markup-lang* '(:xml :xhtml))
            (format nil "<?xml version=\"~A\" encoding=\"~A\"?>~%"
                    *xml-version* *encoding*))
          (awhen (assoc *doctype*
                        (cadr (assoc *markup-lang* *doctypes*)))
            (concat (cdr it) #\Newline))))

; --- Escape ----------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass safe ()
    ((obj :initarg :obj)))

  (defmacro safe (&rest body)
    `(if *safe*
         ,@body
         (let ((*safe* t))
           (make-instance 'safe :obj ,@body))))

  (defgeneric escape (obj))

  (defmethod escape ((char character))
    (if *escape*
        (case char
          (#\& "&amp;")
          (#\< "&lt;")
          (#\> "&gt;")
          (#\' "&#039;")
          (#\" "&quot;")
          (t (string char)))
        char))

  (defmethod escape ((string string))
    (if *escape*
        (apply #'concat
               (loop for c across string collect (escape c)))
        string))

  (defmethod escape ((safe safe))
    (slot-value safe 'obj))

  (defmethod escape ((obj t))
    (escape (->string obj))))

; --- Output ----------------------------------------------------

(defun p (&rest args)
  (dolist (a args)
    (when a (princ a *sml-output*)))
  nil)

(defun pe (x)
  (p (escape x)))

(defun pr (x)
  (if (listp x)
      (mapcar #'pr x)
      (pe x)))

; --- Util ------------------------------------------------------

(defun indent ()
  (when *indent-mode*
    (make-string (* *indent-level* *tab-width*)
                 :initial-element #\Space)))

(defun attr (attr value)
  (awhen value
    (format nil " ~A=\"~A\"" (escape (->string-down attr)) (escape it))))

(defmacro sml->ml (&rest sml)
  `(let ((*sml-output* (make-string-output-stream)))
     ,@sml
     (get-output-stream-string *sml-output*)))

; --- Markup language read macro --------------------------------

(defmacro tag (&rest args)
  (let* ((end? (not (when (eq (car (last args)) '/)
                      (setf args (subseq args 0 (1- (length args)))))))
         (tag  (awhen (pop args) (escape (->string-down it))))
         (textarea? (string= tag "textarea")))
    `(progn
       ,(when (and (string= tag "html")
                   (member *markup-lang* '(:html :xhtml)))
         `(p (doctype)))
       (p (indent) "<" ,tag)
       ,@(loop while (keywordp (car args))
               collect `(p (attr ,(pop args) ,(pop args))))
       ,(if end? `(p ">" #\Newline)
            `(p (if (eq *markup-lang* :html) ">" " />") #\Newline))
       ,@(loop for i in args collect
               `(let ((*indent-level* (1+ *indent-level*))) (pr ,i)))
       ,(when end? `(p (indent) "</" ,tag ">" #\Newline)))))

(set-macro-character #\] (get-macro-character #\)))
(set-macro-character #\[
  #'(lambda (stream char)
      (declare (ignore char))
      `(tag ,@(read-delimited-list #\] stream t))))

; --- Form utilities --------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-input (type sequence &optional alias)
    (or (and alias (eq (car sequence) alias))
        (and (eq (nth 1 sequence) 'input)
             (equalp (awhen (member :type sequence) (nth 1 it))
                     type))
        (remove nil (loop for s in sequence
                          when (listp s)
                          collect (find-input type s alias))))))

(defmacro form (&rest args)
  `(progn
     (p (indent) "<form")
     ,@(loop while (keywordp (car args))
             collect `(p (attr ,(pop args) ,(pop args))))
     (p ">" #\Newline)
     ,@(loop for a in args collect
             `(let ((*indent-level* (1+ *indent-level*))) (pr ,a)))
     ,(unless (find-input "submit" `',args 'submit)
       `(let ((*indent-level* (1+ *indent-level*))) (submit)))
     (p (indent) "</form>" #\Newline)))

(defmacro multipart-form (&rest args)
  `(form :enctype "multipart/form-data" ,@args))

(defmacro input-text (name &rest args)
  `(tag input :type "text" :name ,name ,@args /))

(defmacro input-file (name &rest args)
  `(tag input :type "file" :name ,name ,@args /))

(defmacro submit (&rest args)
  `(tag input :type "submit" ,@args /))

(defmacro input-checked (type input &rest args)
  `(if ,(awhen (position :value args)
          (aand (nth (1+ it) args) `(string= (->string ,it) ,input)))
       (tag input :type ,type :checked "checked" ,@args /)
       (tag input :type ,type ,@args /)))

(defun select-form (name values &optional selected)
  (tag select :name name :id name
       (loop for v in values collect
             (tag option :value v :selected
                  (when (equal selected v) "selected") v))))

; --- Template --------------------------------------------------

(defmacro define-template (name sml)
  `(setf (gethash ,name *sml*) ',sml))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-template (name)
    (or (gethash name *sml*)
        (error "sml not defined: ~A" name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-sml (sml &optional package (readtable (copy-readtable)))
    (if (probe-file sml)
        (with-open-file (stream sml)
          (with-standard-io-syntax 
            (let ((*package*   package)
                  (*readtable* readtable))
              (read stream nil nil))))
        (error "sml file not found: ~A" sml))))

(defmacro load-sml (sml)
  (read-sml (eval sml) *package*))

(defmacro replace-sml (sml &rest body)
  (labels ((by-tag (sml)
             (awhen (position (->keyword (nth 1 sml)) body)
               (nth (1+ it) body)))
           (by-id (sml)
             (awhen (position :id sml)
               (let ((id (->keyword (concat "#" (nth (1+ it) sml)))))
                 (aand (position id body) (nth (1+ it) body)))))
           (replace! (sml)
             (or (by-tag sml)
                 (by-id  sml)
                 (loop for e in sml
                       collect (if (listp e) (replace! e) e)))))
    (replace! sml)))

(defmacro with-template ((name) &rest body)
  `(replace-sml ,(get-template name) ,@body))

(defmacro with-sml-file (file-path &rest body)
  `(replace-sml ,(read-sml (eval file-path) *package*) ,@body))
