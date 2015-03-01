(defun org-sg-filter-files-regex (files regex)
  (delq nil
        (mapcar (lambda (f) (when (string-match regex f) f))
             files ))
  )

(defun org-sg-blog-index (project)
  (let* ((project-plist (cdr project))
         (exclude-regexp (plist-get project-plist :exclude))
         (blog-regex (plist-get project-plist :blog-regex))
         (candidate-files (org-publish-get-base-files project exclude-regexp))
         (files (org-sg-filter-files-regex candidate-files blog-regex))
         (preparation-function
          (plist-get project-plist :preparation-function)))
    (when preparation-function (run-hooks 'preparation-function))
    (plist-get project-plist :preparation-function)
    project-plist
    )

  )


(defun org-sg-cut(backend)
  (goto-char (point-min))
  (when (re-search-forward "^#\\+CUT" nil t)
    (beginning-of-line)
    (kill-region (point) (point-max))
    )
  )

(defun org-sg-remove-cut(backend)
  (goto-char (point-min))
  (when (re-search-forward "^#\\+CUT" nil t)
    (beginning-of-line)
    (kill-line)
    (kill-line)

    )
  )


(defun org-sg-publish-preparation-function()
  (add-hook 'org-export-before-parsing-hook 'org-sg-remove-cut)
  )


;;(add-hook 'org-export-before-parsing-hook 'org-sg-remove-cut)


;;(org-sg-blog-index (assoc org-testproject org-publish-project-alist))




(defun org-sg-publish-to-html (plist filename pub-dir)
  (let* ((org-export-before-parsing-hook org-export-before-parsing-hook))
    (add-hook 'org-export-before-parsing-hook 'org-sg-remove-cut)
    (org-publish-org-to 'html filename
                        (concat ".body.html")
                        plist pub-dir)
    )
  (let* ((org-export-before-parsing-hook org-export-before-parsing-hook))
    (add-hook 'org-export-before-parsing-hook 'org-sg-cut)
    (org-publish-org-to 'html filename
                        (concat ".excerpt.html")
                        plist pub-dir)
    )
)



(defun org-sg-get-inbuffer-options (filename)
  (let* ((org-inhibit-startup t)
         (visitingp (find-buffer-visiting filename))
         (work-buffer (or visitingp (find-file-noselect filename))))
    (with-current-buffer work-buffer
      (org-export--get-inbuffer-options)
  )))

(defun org-sg-get-option (options-plist option-name)
  (let ((option (plist-get options-plist option-name)))
    (when option
      (if (listp option)
          (substring-no-properties (car option))
        (substring-no-properties option)
          )
      )
    )
  )

(defun org-sg-get-posts (project)
  (let* ((project-plist (cdr project))
         (exclude-regexp (plist-get project-plist :exclude))
         (blog-regex (plist-get project-plist :blog-regex))
         (candidate-files (org-publish-get-base-files project exclude-regexp))
         (files (org-sg-filter-files-regex candidate-files blog-regex))
         options
         result
         posts-with-timestamp
         posts-without-timestamp
         result-sorted
         )
    (dolist (file files result)
      (setq options (org-sg-get-inbuffer-options file))
      (setq result (cons (list :file file
                               :author (org-sg-get-option options :author)
                               :date (when (org-sg-get-option options :date)
                                       (apply 'encode-time
                                              (org-parse-time-string
                                               (org-sg-get-option options :date))))
                               :title (org-sg-get-option options :title)
                               :keywords (when (org-sg-get-option options :keywords)
                                           (split-string (org-sg-get-option options :keywords))))
                         result))
      )
    (setq posts-with-timestamp (delq nil (mapcar (lambda (x)
                                                   (and (plist-get x :date) x))
                                                 result)))
    (setq posts-without-timestamp (delq nil (mapcar (lambda (x)
                                                      (and (not (plist-get x :date)) x))
                                                    result)))
    (append (sort posts-with-timestamp
                  (lambda (a b) (time-less-p (plist-get b :date)
                                             (plist-get a :date))))
            (sort posts-without-timestamp
                  (lambda (a b) (string< (plist-get b :file)
                                         (plist-get a :file))))
            )
    )
  )

(defun org-sg-get-output-file-name (filename)
    (let* ((project
          (or (org-publish-get-project-from-filename filename)
              (error "File %s not part of any known project"
                     (abbreviate-file-name filename))))
         (project-plist (cdr project))
         (ftname (expand-file-name filename))
         (base-dir
          (file-name-as-directory
           (expand-file-name
            (or (plist-get project-plist :base-directory)
                (error "Project %s does not have :base-directory defined"
                       (car project))))))
         (pub-dir
          (file-name-as-directory
           (file-truename
            (or (eval (plist-get project-plist :publishing-directory))
                (error "Project %s does not have :publishing-directory defined"
                       (car project))))))
         (tmp-pub-dir
               (file-name-directory
                (concat pub-dir
                        (and (string-match (regexp-quote base-dir) ftname)
                             (substring ftname (match-end 0))))))
         (visitingp (find-buffer-visiting filename))
         (work-buffer (or visitingp (find-file-noselect filename)))
         output-file)
    (with-current-buffer work-buffer
      (setq output-file
            (org-export-output-file-name ".html" nil tmp-pub-dir))

      output-file
      )
    )
  )

(defun org-sg-get-post-excerpt (filename)
  (let* ((org-inhibit-startup t)
         (output-file (org-sg-get-output-file-name filename))
         (excerpt-file (concat (file-name-sans-extension output-file)
                               ".excerpt.html"))
         (visitingp (find-buffer-visiting excerpt-file))
         (work-buffer (or visitingp (find-file-noselect excerpt-file)))
         buffer-as-string
         )
    (with-current-buffer work-buffer
      (setq buffer-as-string
            (buffer-substring-no-properties (point-min) (point-max)))
      )
    buffer-as-string
    )
  )

(defun org-sg-get-post-body (filename)
  (let* ((org-inhibit-startup t)
         (output-file (org-sg-get-output-file-name filename))
         (excerpt-file (concat (file-name-sans-extension output-file)
                               ".body.html"))
         (visitingp (find-buffer-visiting excerpt-file))
         (work-buffer (or visitingp (find-file-noselect excerpt-file)))
         buffer-as-string
         )
    (with-current-buffer work-buffer
      (setq buffer-as-string
            (buffer-substring-no-properties (point-min) (point-max)))
      )
    buffer-as-string
    )
  )


(defun org-sg-gen-post-excerpts (posts start end)
  (dotimes (i (- end start))
    (let* ((post (nth i posts)))
      (insert "<h1>")
      (insert (or (plist-get post :title)
                  "Untitled"))
      (insert "</h1>\n")
      (insert (org-sg-get-post-excerpt (plist-get post :file)))
      (insert "<hr/>\n")
      )
    )
  )

(defun org-sg-generate-post-list (project)
  (insert "<hr>\n")
  (let ((posts (org-sg-get-posts project)))
    (org-sg-gen-post-excerpts posts 0 (length posts))
    )
  )

(defun org-sg-generate-site-header (project)
  (insert "")
  )

(defun org-sg-generate-site-body (project)
  (insert "<html>\n")
  (insert "<head></head>\n")
  (insert "<body>\n")
  (org-sg-generate-post-list project)
  (insert "<body>\n")
  (insert "</html>\n")

  )

(defun org-sg-generate-site-index-file (project)
  (let* ((project-plist (cdr project))
         (pub-dir
          (file-name-as-directory
           (file-truename
            (or (eval (plist-get project-plist :publishing-directory))
                (error "Project %s does not have :publishing-directory defined"
                       (car project))))))
         (filename (concat pub-dir "index.html"))
         (visitingp (find-buffer-visiting filename))
         (work-buffer (or visitingp (find-file-noselect filename)))
         )

    (with-current-buffer work-buffer
      (erase-buffer)
      (org-sg-generate-site-body project)
      (save-buffer)
      )

    )
  )

(defun org-sg-publish-completion-function()
  (org-sg-generate-site-index-file project)
  )
