(defpackage :blisp (:use :cl))
(in-package :blisp)

(defun main () (print "balls"))

(defvar *server* nil
  "Running Hunchentoot acceptor")

(defvar *cwd*
  (uiop/pathname:ensure-pathname
   (find-if #'identity (list (uiop/os:getenvp "BLOG_POST_PATH") (uiop/os:getcwd)))))

(defvar *search-pattern* 
  (concatenate 'string 
               (uiop/filesystem:native-namestring *cwd*)
               "*.md")
  "Search pattern for markdown files")

(defun start-server (&key (port 8080))
  "Start the web server on PORT (default 8080).
If a server is already running it is stopped first."
  (when *server*
    (format t "~&; Server already running – restarting…~%")
    (stop-server))
  (setf *server* (make-instance 'hunchentoot:easy-acceptor :port port))
  (hunchentoot:start *server*)
  (format t "~&; Server started on http://localhost:~A~%" port)
  *server*)

(defun stop-server ()
  "Stop the running web server and set *SERVER* to NIL."
  (if *server*
      (progn
        (hunchentoot:stop *server* :soft t)
        (setf *server* nil)
        (format t "~&; Server stopped.~%"))
      (format t "~&; No server is running.~%")))

(defun nav-bar ()
  (spinneret:with-html
     (:nav
      (:div :style "display: flex; flex-direction: row; justify-content: space-evenly" 
       (:div (:a :href "/home" "home")) 
       (:div (:a :href "/post_list" "Posts")))
    )))

(defmacro with-page ((&key title) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body ,@body))))

(defmacro with-page-raw ((&key title) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body
       (nav-bar)
       (:raw ,@body)))))
(defun home-page ()
  (with-page 
    (:title "Home page")
    (:header
     (nav-bar)
    )))

(hunchentoot:define-easy-handler (test :uri "/test") ()
  (print "Hitting /test")
  (setf (hunchentoot:content-type*) "text/html")
  (home-page))

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (progn
    (print "hitting /yo")
    (format nil "Hey~@[ ~A~]!" name)))

(hunchentoot:define-easy-handler (home :uri "/home") () 
  (setf (hunchentoot:content-type*) "text/html")
  (spinneret:with-html-string (:h1 "test")))

(defvar *routes* (make-hash-table :test #'equal)
  "Maps URI strings to their dispatcher, so redefinitions replace rather than stack.")
(defun sync-dispatch-table ()
  "Replace only our own dispatchers in *dispatch-table*, leaving others untouched."
  (let ((ours (loop for d being the hash-values of *routes* collect d)))
    (setf hunchentoot:*dispatch-table*
          (append ours
                  (remove-if (lambda (d) (member d ours)) 
                             hunchentoot:*dispatch-table*)))))

(defmacro define-route (uri &body body)
  "Define (or redefine) a route for URI. Safe to re-evaluate at the REPL."
  `(progn
     (setf (gethash ,uri *routes*)
           (hunchentoot:create-prefix-dispatcher ,uri (lambda () ,@body)))
     (sync-dispatch-table)
     ',uri))

(defun to-url-list (file-name-list)
  (mapcar 
   (lambda (filename) 
     (spinneret:with-html (:a :href (concatenate 'string "/md/" filename) filename)))
   file-name-list))

(define-route "/post_list"
  (let* ((file-list (directory *search-pattern*))
         (file-names (mapcar 'pathname-name file-list)))
    
    
    (setf (hunchentoot:content-type*) "text/html")
    (with-page 
        (:title "Post list")
      (to-url-list file-names)))
  )

(define-route "/file/"
  (let* ((uri (hunchentoot:request-uri*))
         (filename (subseq uri (length "/file/"))))
    (setf (hunchentoot:content-type*) "text/plain")
    (uiop:read-file-string (merge-pathnames filename *cwd*))))

(defun markdown-from-file (file title) 
  (with-page-raw 
    (:title title)
    (nth-value 
     1 
     (cl-markdown:markdown (uiop:read-file-string file) :format :html :stream nil))
   ))

(define-route "/md/"
  (let* ((uri (hunchentoot:request-uri*))
         (filename (subseq uri (length "/md/")))
         (file-path (merge-pathnames (concatenate 'string filename ".md") *cwd*)))
    (setf (hunchentoot:content-type*) "text/html")
    (markdown-from-file file-path filename)))