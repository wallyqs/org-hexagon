;; Client for the API from OrgHexagon
(require 'url)
(require 'json)

(setq org-hexagon-url "http://127.0.0.1:9393/")
(setq org-hexagon-api-url "http://127.0.0.1:9393/api/")

(defun org-hexagon-put-text (beg end)
  (interactive "r")

  (let* ((url-request-method "POST")
         (org-text 
	  (buffer-substring-no-properties beg end))
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (encode-coding-string
           (json-encode
            `((content . ,org-text))
            ) 'utf-8)
          )
	 (uri (concat 
	       org-hexagon-api-url 
	       "texts.json"))
         )                               ;end of let varlist

    (url-retrieve uri
                  '(lambda (status)
                     (switch-to-buffer (current-buffer))
                     ))
    )
  )
