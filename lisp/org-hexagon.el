;; Client for the API from OrgHexagon
(require 'url)
(require 'json)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(setq org-hexagon-url "http://127.0.0.1:9393/")
(setq org-hexagon-api-url "http://127.0.0.1:9393/api/")

(defun org-hexagon-request (method url &optional data)
  "Returns a JSON object with the contents from the request"

  (let* ((url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-method method)
         (url-request-data
          (if data (encode-coding-string data 'utf-8)))
         (buf))

    (setq buf (url-retrieve-synchronously url))
    (with-current-buffer buf
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
        (kill-buffer))
      )))

(defun org-hexagon-all-texts ()
  "Creates a buffer with all the texts so far merged"
  (interactive)

  (let* ((json
          (org-hexagon-request
           "GET" (concat org-hexagon-api-url "texts.json")))
         (buf (get-buffer-create "*Org Hexagon Texts*")))

    (with-current-buffer buf
      (mapcar (lambda(org-text)
                (insert
                 (concat
                  (cdr
                   (assoc 'content org-text)) "\n"))
                ) json))

    (switch-to-buffer buf)
    (org-mode)))

(defun org-hexagon-sync-text-region(beg end)
  "Create org-mode text from region"
  (interactive "r")

  (let* ((region
          (buffer-substring-no-properties beg end))
         (org-text
          (json-encode
           `((content . ,region))))
         (url (concat org-hexagon-api-url "texts.json"))
         (response
          (org-hexagon-request
           "POST" url org-text))
         (status (cdr (assoc 'status response))))

    (if (equal status 200)
	(message (concat "Org text created with id: " (cdr (assoc '_id response))))
      (message "Not saved"))
    )
  )

