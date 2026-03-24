(defpackage :blisp (:use :cl))
(in-package :blisp)

(defun main () 
  (progn 
    (init-article-list)
    (start-server)
    (defvar *rss-feed* (generate-rss-from-metadata))
    ))

(defvar *server* nil
  "Running Hunchentoot acceptor")

(defvar *pub-url* (uiop/os:getenvp "BLOG_POST_URL"))

(defvar *cwd*
  (uiop/pathname:ensure-pathname
   (find-if #'identity 
            (list (uiop/os:getenvp "BLOG_POST_PATH") (uiop/os:getcwd)))))

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


