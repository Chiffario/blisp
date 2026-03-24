(in-package :blisp)

(defstruct feed-item
  (title "" :type string)
  (description "" :type string)
  (link "" :type string)
  (pub-date nil :type string))

(defun metadata->feed-item (metadata)
  (declare (type post-metadata metadata))
  (make-feed-item
   :title (post-metadata-title metadata)
   :description (post-metadata-description metadata)
   :link (str:concat *pub-url* "md/" (post-metadata-filename metadata))   
   :pub-date (post-metadata-pub-date metadata)
   )
  )

(defstruct feed-channel
  (title "" :type string)
  (description "" :type string)
  (link "" :type string)
  (language "en-us" :type string)
  (pub-date nil :type string)
  (generator "blisp rss generator")
  (last-build-date "")
  (items '() :type list))

(defun default-channel (items)
  (make-feed-channel
     :title "Chiffa's Hole"
     :description "Chiffa's personal little blog"
     :link "http://localhost:8080/"
     :pub-date "Sun, 22 Mar 2026 06:18:59 GMT"
     :last-build-date "Sun, 22 Mar 2026 06:18:59 GMT"
     :items items)
  )

(defun %text-element (tag text)
  (fxml:with-element tag (fxml:text text)))

(defun %optional-text-element (tag value)
  "Emit <TAG>VALUE</TAG> only when VALUE is a non-empty string."
  (when (and value (not (string= value "")))
    (%text-element tag value)))
 
(defun %write-item (item)
  "Serialise a FEED-ITEM as an RSS <item> element."
  (fxml:with-element "item"
    (%text-element "title"       (feed-item-title item))
    (%text-element "description" (feed-item-description item))
    (%text-element "link"        (feed-item-link item))
    ;; <guid> falls back to the link when not supplied explicitly
    (%text-element "guid" (feed-item-link item))
    (%optional-text-element "pubDate" (feed-item-pub-date item))))

(defun %write-channel (channel)
  "Serialise a FEED-CHANNEL as an RSS <channel> element."
  (fxml:with-element "channel"
    (%text-element "title"       (feed-channel-title channel))
    (%text-element "description" (feed-channel-description channel))
    (%text-element "link"        (feed-channel-link channel))
    (%optional-text-element "language"      (feed-channel-language channel))
    (%optional-text-element "pubDate"       (feed-channel-pub-date channel))
    (%optional-text-element "lastBuildDate" (feed-channel-last-build-date channel))
    (%optional-text-element "generator"     (feed-channel-generator channel))
    (dolist (item (feed-channel-items channel))
      (%write-item item))))

(defun generate-rss (channel sink)
  "Write an RSS 2.0 document for CHANNEL to the fxml serialisation SINK.
 
  CHANNEL must be a FEED-CHANNEL struct whose :items slot holds a list of
  FEED-ITEM structs.  SINK is any fxml sink (string, stream, file …)."
  (fxml:with-xml-output sink
    (fxml:with-element "rss"
      (fxml:attribute "version" "2.0")
      ;; Atom namespace – useful for self-link extension, kept optional here
      (%write-channel channel))))
 
(defun generate-rss-to-string (channel &key (indentation 2))
  "Return a pretty-printed RSS 2.0 XML string for CHANNEL.

  INDENTATION controls how many spaces per nesting level (default 2).
  Pass NIL to get a compact single-line document."
  (generate-rss channel
                (fxml:make-string-sink
                 :indentation indentation)))

(defun generate-rss-from-metadata ()
  (let* ((posts (alexandria:hash-table-values *title-metadata-map*))
        (feed-posts (mapcar #'metadata->feed-item posts)))
    (generate-rss-to-string 
     (default-channel feed-posts))
    )
  )