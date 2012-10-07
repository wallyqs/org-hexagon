;; Client for the API from OrgHexagon
(require 'org)
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

(defun org-hexagon-texts()
  "Creates a buffer with all the texts so far merged"
  (interactive)

  (let* ((json
          (org-hexagon-request
           "GET" (concat org-hexagon-api-url "texts.json")))
	 (buf (org-hexagon-create-shelf-buffer json "*Org Hexagon Texts*")))

    (switch-to-buffer buf)
    (org-mode)))

(defun org-hexagon-shelf()
  "Creates a buffer with all the texts from a shelf."
  (interactive)

  (let* ((shelf-name (read-string "Shelf: "))
	 (json 
	  (org-hexagon-request
	   "GET" (concat org-hexagon-api-url "shelves/" shelf-name ".json")))
	 (shelf-buffer-title (concat "*Org Hexagon Shelf: " shelf-name "*"))
	 (buf (org-hexagon-create-shelf-buffer json shelf-buffer-title)))

    (message (concat "Getting texts from " shelf-name))
    (switch-to-buffer buf)
    (org-mode)))

(defun org-hexagon-create-shelf-buffer(json &optional shelf-buffer-title)
  "Creates a buffer from the json string of the request."

  (let* ((buf (get-buffer-create shelf-buffer-title)))
    (with-current-buffer buf
      (mapcar (lambda(org-text)
		(insert
		 (concat
		  (cdr
		   (assoc 'content org-text)) "\n"))
		) json))
    buf))

(defun org-hexagon-sync-text-region(beg end)
  "Create or update org-mode text from region.
In case an :id: is present in the propety drawer, it will
attempt to use it to update the text rather than creating another one.
In case of update, it will also sync the properties
from the text in the property drawer.
NOTE: _id cannot be used as an identifier so we use :id: all the time"
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
        (let* ((text-id (cdr (assoc 'id response)))
	       (properties `(
			     (":id:" . ,text-id)
			     )))
          (message (concat "Org text created with id: " text-id))
          (org-hexagon-update-property-drawer beg end properties))
      (message "Text not saved!"))
    ))

(defun org-hexagon-update-property-drawer(region-begin region-end properties)
  "Given the beginning and start of the property-drawer in the text,
this will update the property-drawer with the fields in the assoc list.
If no property-drawer was present at the time, it will create one."

  (let* ((property-drawer-range 
	  (org-get-property-block region-begin region-end)))
    
    (goto-char region-begin)
    (if property-drawer-range
	(setq property-drawer-beg (car property-drawer-range))
      ;; In case we don't have a property drawer, create it here
      (progn
	(org-insert-property-drawer)
	(org-cycle 3)
	(next-line 1)
	(setq property-drawer-beg (point))))
    (goto-char property-drawer-beg)

    ;; Make a list of the current properties again into an assoc list
    ;; TODO

    ;; Then override the elements on the list that already exists with the new ones
    (dolist (pair properties)
      (let* ((property-key (car pair))
	     (property-value (cdr pair))
	     (property-string (concat "  " property-key " " property-value "\n")))

	(insert property-string))
      )
    (message "Org Hexagon Text Synced!")))

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
          (setq key (downcase (org-match-string-no-properties 1))
                value (org-trim (or (org-match-string-no-properties 2) "")))
          (unless (member key excluded)
            (push (cons key (or value "")) property-drawer-list))) ;; end of while
        ) ;; end of when getting the properties
      ;; return
      property-drawer-list)
    ))
