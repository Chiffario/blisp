(in-package :blisp) 
(import '(cl-markdown::render-block
          cl-markdown::chunk
          cl-markdown::chunk-kind 
          cl-markdown::chunk-lines 
          cl-markdown::chunk-properties))

(defun %run (program args &key input)
  (with-output-to-string (out)
    (with-input-from-string (in (or input ""))
      (uiop:run-program (cons program args)
                   :input in
                   :output out
                   :error-output :output
                   :ignore-error-status t))))

(defvar cli-path "/home/chiffa/Dev/Projects/ts-cli/target/debug/ts-cli")

(defun %call-ts-highlight (code language)
  "Call ts-highlight --code CODE [--language LANGUAGE].
Returns the stdout HTML string on success, NIL on any failure."
  (handler-case
      (let ((command (append (list cli-path "--code" code)
                             (when (and language (not (string= language "")))
                               (list "--language" language)))))
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program command
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (declare (ignore error-output))
          (if (zerop exit-code)
              output
              (progn
                (warn "ts-highlight exited with status ~a~@[ for language ~S~]; ~
falling back to plain rendering." exit-code language)
                nil))))
    (error (condition)
      (warn "ts-highlight invocation failed (~a); falling back to plain rendering."
            condition)
      nil)))
 
;;; -----------------------------------------------------------------------
;;; Hook: intercept code-block rendering and pipe through ts-highlight.
;;;
;;; cl-markdown dispatches rendering via render-block-to-html.  An :around
;;; method lets us inspect the chunk kind first and either delegate to the
;;; primary method (for non-code blocks, or when ts-highlight fails) or emit
;;; the highlighted HTML directly.
 
(defmethod render-block-to-html :around ((chunk chunk) (stream stream))
  (if (not (eq (chunk-kind chunk) 'cl-markdown::code))
      (call-next-method)
      (let* ((lines    (chunk-lines chunk))
             (language (chunk-properties chunk))
             (code     (format nil "~{~a~%~}" lines))
             (html     (%call-ts-highlight code language)))
        (if html
            (format nil
                    "<div class=\"highlight~@[ language-~a~]\">~a</div>~%"
                    language html)
            (call-next-method)))))