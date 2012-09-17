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

    (pp data)

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

(defun org-hexagon-create-text-region(beg end)
  "Create org-mode text from region"
  (interactive "r")

  (let* ((region
          (buffer-substring-no-properties beg end))
	 (property-drawer-list 
	  (org-hexagon-preprocess-property-drawer-string region))
         (org-text
          (json-encode
           `((content . ,region)
	     (properties . ,property-drawer-list))))
         (url (concat org-hexagon-api-url "texts.json"))
         (response
          (org-hexagon-request "PUT" url org-text))
         (status (cdr (assoc 'status response))))

    (if (equal status 200)
	(let ((text-id (cdr (assoc '_id response))))
	  (message (concat "Org text created with id: " text-id))
	  ;; (org-hexagon-update-property-drawer region text-id))
      (message "Text not saved"))
    ))

(defun org-hexagon-update-property-drawer(region text-id)
  (with-current-buffer
      (let* ((beg (point-min))
	     (end (point-max))
	     (property-drawer-range (org-get-property-block beg end)))
	(pp property-drawer-range)
	(when property-drawer-range
	  (goto-char (car property-drawer-range))
	  )
	)
      ))

;; helper methods
(defun org-hexagon-preprocess-property-drawer-string (org-text-region)
  "This function creates an assoc list of the values inside of
the PROPERTIES drawer of the _first level_ of the ORG-TEXT-REGION string,
other levels are ignored...

Example:

:PROPERTIES:              => ..will return an assoc-list like this:
:shelf: memorias                 (('shelf' . 'memorias')
:slug:  memoria-1                 ('slug' . 'memoria-1'))
:END:

It is based on some code found within org.el.
"
  (with-temp-buffer
    (insert org-text-region)
    (org-mode)
    (let* ((beg (point-min))
           (end (point-max))
           (excluded '("TODO" "TAGS" "ALLTAGS" "PRIORITY" "BLOCKED"))
           ;; range where each one of the things from the PROPERTY drawer
           (property-drawer-range (org-get-property-block beg end))
           property-drawer-list)
      (when property-drawer-range
        (goto-char (car property-drawer-range))
        (while (re-search-forward
                (org-re "^[ \t]*:\\([[:alpha:]][[:alnum:]_-]*\\):[ \t]*\\(\\S-.*\\)?")
                (cdr property-drawer-range) ;; boundary
                t)
          (setq key (org-match-string-no-properties 1)
                value (org-trim (or (org-match-string-no-properties 2) "")))
          (unless (member key excluded)
            (push (cons key (or value "")) property-drawer-list))) ;; end of while
        ) ;; end of when getting the properties
      ;; return
      property-drawer-list)
    ))
