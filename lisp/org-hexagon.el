;; Client for the API from OrgHexagon
(require 'url)

(setq org-hexagon-url "http://127.0.0.1:9393/api/text.json")

(defun org-hexagon-get-text ()
  (interactive)
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (encode-coding-string (concat "{"
                                        "\"content\": \"* HELLO WORLD\""
                                        "}"
                                        ) 'utf-8))
         )
    (url-retrieve org-hexagon-url
                  '(lambda (status)
                     (switch-to-buffer (current-buffer))
                     ))
    )
  )
