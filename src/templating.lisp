(in-package :blisp)

(defun nav-bar ()
  (spinneret:with-html
    (:nav
     (:div :class "nav"  
      (:div (:a :href "/post_list" "posts"))
      (:div (:a :href "/feed" "rss"))
      )
     )))

(defmacro with-page ((&key title) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:link :rel :stylesheet :href "/styles.css")
       (:title ,title))
      (:body
       (nav-bar)
       ,@body
       ))))

(defmacro blog-inner (&body body)
  `(spinneret:with-html
     (:div
      :class "blog-inner"
      (:article
       :id "blog-page"
       :style "width: 80%;"
       (:raw ,@body)))))

(defun home-page () 
  (spinneret:with-html-string (:h1 "test")))

(spinneret:deftag :post-list-url (default attrs &key name description url)
  `(:a :href ,url
       :class "post-block"
    (:div 
     (:p :class "post-name" ,name) (:p :class "post-description" ,description)
     ,@default ,@attrs
     )))

(defun link-list ()
  (spinneret:with-html-string
     (dolist (article (alexandria:hash-table-keys *title-file-map*))
       (:post-list-url 
        :url (str:concat "/md/" (gethash article *title-file-map*))
        :description (gethash article *title-description-map*)
        :name article)
       )))

(defun post-list ()
  (spinneret:with-html-string 
    (:div 
     :style "display: flex; flex-direction: column; justify-content: center; align-items: center;
              margin: 20px auto; width: 80%;"
     (:raw (link-list)))))