(in-package :blisp)

(defun init-article-list ()
  (dolist 
      (file (directory *search-pattern*))
    (let 
        ((cl-markdown:*parse-active-functions* '(metadata))
         (file-name (pathname-name file)))
      (declare (special file-name))
      (cl-markdown:markdown file :format :none :stream nil))
    )
  )

(defstruct post-metadata
  (title "" :type string)
  (description "" :type string)
  (filename "" :type string)
  (pub-date "" :type string))

(defvar *title-description-map* (make-hash-table :test 'equalp))
(defvar *title-file-map* (make-hash-table :test 'equalp))
(defvar *title-metadata-map* (make-hash-table :test 'equalp))

(defun plist-to-hash (plist)
  (let ((table (make-hash-table :test #'equalp)))
    (loop :for (key value) :on plist :by #'cddr
          :do (setf (gethash key table) value))
    table))

(defun list-system-deps-with-versions (system-name)
  "Return a list of (name version) pairs for all explicit dependencies."
  (let* ((system (asdf:find-system system-name))
         (deps   (asdf:system-depends-on system)))
    (mapcar (lambda (dep)
              (let* ((dep-name (etypecase dep
                                 (string dep)
                                 (symbol (string-downcase dep))
                                 (list   (string-downcase
                                          (case (first dep)
                                            (:version (second dep))
                                            (:feature (third  dep))
                                            (t        (second dep)))))))
                     (dep-sys     (asdf:find-system dep-name nil))
                     (dep-version (when dep-sys
                                    (asdf:component-version dep-sys))))
                (list dep-name (or dep-version "unknown"))))
            deps)))

(cl-markdown:defsimple-extension versions
  (let* ((dependencies (list-system-deps-with-versions :blisp)))
    (spinneret:with-html-string
      (:ul 
       (dolist (version dependencies)
         (:li (str:concat (first version) " - " (second version))))
       ))
    ))

(declaim (special file-name))
(cl-markdown:defextension (metadata :arguments ((data :required :whole))) :insertp t
  (when (eq phase :parse)
    (let* ((table (plist-to-hash data))
           (metadata (make-post-metadata
                      :title (gethash :title table)
                      :description (gethash :description table)
                      :pub-date (gethash :pub-date table)
                      :filename file-name))
           (title (gethash :title table))
           (description (gethash :description table)))
      (setf (gethash title *title-description-map*) description)
      (setf (gethash title *title-metadata-map*) metadata)
      (setf (gethash title *title-file-map*) file-name)))
  )

(defun render-markdown (file)
  (let 
      ((cl-markdown:*render-active-functions* 
         (append '(versions) cl-markdown:*render-active-functions*)))
    
    (cl-markdown:markdown (uiop:read-file-string file) :format :html :stream nil)))

(defun markdown-from-file (file title) 
  (with-page
      (:title title)
    (blog-inner
      (nth-value 1
                 (spinneret:with-html
                   (render-markdown file))))
    ))