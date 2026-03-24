(in-package :blisp)

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

(define-route "/post_list"
  (setf (hunchentoot:content-type*) "text/html")
  (with-page 
      (:title "Post list")
    (:raw (post-list)))
  )

(define-route "/home"
  (setf (hunchentoot:content-type*) "text/html")
  (with-page (:title "Home") (:raw (home-page))))

(define-route "/md/"
  (let* ((uri (hunchentoot:request-uri*))
         (filename (subseq uri (length "/md/")))
         (file-path (merge-pathnames (concatenate 'string filename ".md") *cwd*)))
    (setf (hunchentoot:content-type*) "text/html")
    (markdown-from-file file-path filename)))

(define-route "/feed"
  (setf (hunchentoot:content-type*) "text/xml")
  (generate-rss-from-metadata))

(hunchentoot:define-easy-handler (test :uri "/styles.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (uiop:read-file-string "/home/chiffa/Dev/Projects/blisp/styles.css"))

(hunchentoot:define-easy-handler (home :uri "/") () 
  (setf (hunchentoot:content-type*) "text/html")
  (hunchentoot:redirect "/post_list"))

