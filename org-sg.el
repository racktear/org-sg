;;; org-sg.el --- org-mode static site/blog generator

;; Copyright (C) 2015 Konstantin Nazarov <mail@racktear.com>

;; Author: Konstantin Nazarov <mail@racktear.com>
;; URL: https://github.com/racktear/org-sg

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; org-sg is a static site generator implemented on top of excellent
;; org-mode package. It has support for blogging, publishing static
;; pages and developer-friendly customization.

;;; Code:

;;; Dependencies

(require 'ox)
(require 'ox-publish)

;;; Internal Variables

(setq org-sg-css-directory (concat
                            (expand-file-name (file-name-directory load-file-name))
                            "css/"))



(setq org-sg-style-default
  "<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/
  body {
    -webkit-font-feature-settings: 'kern' 1;
    -moz-font-feature-settings: 'kern' 1;
    -ms-font-feature-settings: 'kern' 1;
    -o-font-feature-settings: 'kern' 1;
    font-feature-settings: 'kern' 1;
    height: 100%;
    max-height: 100%;
    font-family: \"Helvetica Neue\", Arial, sans-serif;
    /*font-size: 2.2rem;*/
    line-height: 1.7em;
    color: #3A4145;
  }

  .main-header {
    position: relative;
    display: table;
    width: 100%;
    margin-bottom: 5rem;
    text-align: center;
    background-size: cover;
    overflow: hidden;
  }

  .main-header .inner {
    width: 80%;
  }
  .post {
    position: relative;
    width: 80%;
    max-width: 780px;
    margin: 4rem auto;
    padding-bottom: 4rem;
    border-bottom: #EBF2F6 1px solid;
    word-break: break-word;
    hyphens: auto;
  }

  a {
    color: #18BC9C;
    text-decoration: none;
  }
  a:hover {
    text-decoration: underline;
  }

 /*]]>*/-->
 </style>"
  )

(defconst org-sg-htmlize-output-type 'css)

(defconst org-sg-default-posts-per-page 10)

(defun org-sg-filter-files-regex (files regex)
  (delq nil
        (mapcar (lambda (f) (when (string-match regex f) f))
             files ))
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


(defun org-sg-publish-to-html (plist filename pub-dir)
  (let* ((org-export-before-parsing-hook org-export-before-parsing-hook)
         (org-html-htmlize-output-type org-sg-htmlize-output-type))
    (add-hook 'org-export-before-parsing-hook 'org-sg-remove-cut)
    (org-publish-org-to 'html filename
                        (concat ".body.html")
                        plist pub-dir)
    )
  (let* ((org-export-before-parsing-hook org-export-before-parsing-hook)
         (org-html-htmlize-output-type org-sg-htmlize-output-type))
    (add-hook 'org-export-before-parsing-hook 'org-sg-cut)
    (org-publish-org-to 'html filename
                        (concat ".excerpt.html")
                        plist pub-dir)
    )
)

(defun org-sg-get-project-pub-dir (project)
  (let ((project-plist (cdr project)))
    (file-name-as-directory
     (file-truename
      (or (eval (plist-get project-plist :publishing-directory))
          (error "Project %s does not have :publishing-directory defined"
                 (car project)))))
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


(defun org-sg-get-posts (project)
  (let* ((project-plist (cdr project))
         (exclude-regexp (plist-get project-plist :exclude))
         (blog-regex (plist-get project-plist :blog-regex))
         (candidate-files (org-publish-get-base-files project exclude-regexp))
         (files (org-sg-filter-files-regex candidate-files blog-regex))
         (pub-dir (org-sg-get-project-pub-dir project))
         options
         output-file
         result
         posts-with-timestamp
         posts-without-timestamp
         result-sorted
         )
    (dolist (file files result)
      (setq options (org-sg-get-inbuffer-options file))
      (setq output-file (org-sg-get-output-file-name file))

      (setq result (cons (list :file file
                               :excerpt (concat (file-name-sans-extension output-file)
                                                ".excerpt.html")
                               :body (concat (file-name-sans-extension output-file)
                                             ".body.html")
                               :output output-file
                               :pub-dir pub-dir

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

(defun org-sg-get-file-content (filename)
  (let* ((org-inhibit-startup t)
         (visitingp (find-buffer-visiting filename))
         (work-buffer (or visitingp (find-file-noselect filename)))
         buffer-as-string
         )
    (with-current-buffer work-buffer
      (setq buffer-as-string
            (buffer-substring-no-properties (point-min) (point-max)))
      )
    buffer-as-string
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


(defun org-sg-generate-post-body (project post)
  (insert "<article class=\"post\">")
  (insert "<header class=\"post-header\">")
  (insert "<h2>")
  (insert (or (plist-get post :title)
              "Untitled"))
  (insert "</h2>\n")
  (insert "</header>")
  (insert "<section class=\"post-content\">")
  (insert (org-sg-get-file-content (plist-get post :body)))
  (insert "</section>")
  (insert "<footer>")
  (insert "</footer>")
  (insert "</article>")
  )

(defun org-sg-generate-post-excerpt (project post)
  (let* ((post-output-file (plist-get post :output))
         (pub-dir (plist-get post :pub-dir))
         (output-rel (file-relative-name post-output-file pub-dir)))
    (insert "<article class=\"post\">")
    (insert "<header class=\"post-header\">")
    (insert "<h2>")
    (insert (or (plist-get post :title)
                "Untitled"))
    (insert "</h2>\n")
    (insert "</header>")
    (insert "<section class=\"post-excerpt\">")
    (insert (org-sg-get-file-content (plist-get post :excerpt)))
    (insert "<br/>")
    (insert "<a href=") (insert output-rel) (insert "> More </a>")
    (insert "</section>")
    (insert "<footer>")
    (insert "</footer>")
    (insert "</article>")
    )
  )


(defun org-sg-gen-post-excerpts (project posts start end)
  (let* ((posts-per-page (org-sg-get-posts-per-page project))
         )
    (dotimes (i (+ 1 (- end start)))
      (let* ((post (nth (+ start i) posts))
             (post-output-file (plist-get post :output))
             (pub-dir (plist-get post :pub-dir))
             (output-rel (file-relative-name post-output-file pub-dir)))
        (org-sg-generate-post-excerpt project post)
        )
      )

  ))

(defun org-sg-get-posts-per-page (project)
  (let* ((project-plist (cdr project))
         (posts-per-page (plist-get project-plist :posts-per-page)))
    (or posts-per-page org-sg-default-posts-per-page)
    )
  )

(defun org-sg-pagination-numpages (total perpage)
  (+ 1 (/ (- total 1) perpage))
  )

(defun org-sg-pagination-nth (total perpage n)
  (when (< (- (org-sg-pagination-numpages total perpage) 1)
           n)
    (error "Page out of range: %d" n))
  (list (* n perpage)
        (- (min total (* (+ n 1) perpage)) 1))
  )

(defun org-sg-pagination-begin (total perpage n)
  (first (org-sg-pagination-nth total perpage n))
  )

(defun org-sg-pagination-end (total perpage n)
  (second (org-sg-pagination-nth total perpage n))
  )

(defun org-sg-generate-post-list (project pagenum)
  (let* ((posts (org-sg-get-posts project))
         (posts-per-page (org-sg-get-posts-per-page project))
         (numposts (length posts)))
    (org-sg-gen-post-excerpts
     project
     posts
     (org-sg-pagination-begin numposts posts-per-page pagenum)
     (org-sg-pagination-end numposts posts-per-page pagenum))
    )
  )


(defun org-sg-css-to-inline (css-file)
  (let ((css-content (org-sg-get-file-content css-file)))
    (concat
     "<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/"
     css-content
     " /*]]>*/-->
 </style>"
     )
    )
  )


(defun org-sg-generate-site-header (project)
  (let* ((project-plist (cdr project))
         (title (or (plist-get project-plist :org-sg-title)
                    "Untitled"))
         (description (or (plist-get project-plist :org-sg-description)
                          "")))

    (insert "<html>\n")
    (insert "<head>")
    (insert "<meta charset=\"urf-8\"/>")
    (insert "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>")
    (insert "<link href='http://fonts.googleapis.com/css?family=Raleway:400,300,600' rel='stylesheet' type='text/css'>")

    (insert (org-sg-css-to-inline (concat org-sg-css-directory "normalize.css")))
    (insert (org-sg-css-to-inline (concat org-sg-css-directory "skeleton.css")))
    (insert (org-sg-css-to-inline (concat org-sg-css-directory "custom.css")))
    (insert "</head>\n")
    (insert "<body>\n")
    (insert "<div class=\"container\">")
    (insert "<section class=\"header\">")
    (insert "<header class=\"main-header\">")
    (insert "<h1 class=\"page-title\">")(insert title)(insert "</h1>\n")
    (insert "<h2 class=\"page-description\">")(insert description)(insert "</h2>\n")
    (insert "</header>")
    (insert "</section>")
    (insert "</div>")
    (insert "<hr>\n")
    (insert "<div class=\"container\">")
  ))

(defun org-sg-generate-site-footer (project)
  (insert "</div>")
  (insert "<body>\n")
  (insert "</html>\n")
  )

(defun org-sg-generate-pagination-nav (project pagenum)
  (let* ((posts (org-sg-get-posts project))
         (posts-per-page (org-sg-get-posts-per-page project))
         (numposts (length posts))
         (numpages (org-sg-pagination-numpages numposts posts-per-page)))
    (insert "<div><nav class=\"pagination\">\n")

    (when (> pagenum 0)
      (insert "<a class=\"button newer-posts\" href=\"")
      (insert "page_")(insert (int-to-string (- pagenum 1)))(insert ".html")
      (insert "\">&larr; Newer Posts</a>\n")
      )

    (insert "<span class=\"page-number\">")
    (insert "Page ")
    (insert (int-to-string (+ 1 pagenum)))
    (insert " of ")
    (insert (int-to-string numpages))
    (insert "</span>")

    (when (< pagenum (- numpages 1))
      (insert "<a class=\"button older-posts\" href=\"")
      (insert "page_")(insert (int-to-string (+ 1 pagenum)))(insert ".html")
      (insert "\">Older Posts &rarr;</a>\n")
      )

    (insert "</nav></div>")

      )
  )

(defun org-sg-generate-site-page (project pagenum)
  (org-sg-generate-site-header project)
  (org-sg-generate-pagination-nav project pagenum)
  (org-sg-generate-post-list project pagenum)
  (org-sg-generate-pagination-nav project pagenum)
  (org-sg-generate-site-footer project)
  )

(defun org-sg-generate-full-post (project post)
  (org-sg-generate-site-header project)
  (org-sg-generate-post-body project post)
  (org-sg-generate-site-footer project)
  )


(defun org-sg-generate-site-index-file (project)
  (let* ((pub-dir (org-sg-get-project-pub-dir project))
         (filename (concat pub-dir "index.html"))
         (visitingp (find-buffer-visiting filename))
         (work-buffer (or visitingp (find-file-noselect filename)))
         )

    (with-current-buffer work-buffer
      (erase-buffer)
      (org-sg-generate-site-page project 0)
      (save-buffer)
      )
    )
  )

(defun org-sg-generate-site-page-file (project pagenum)
  (let* ((pub-dir (org-sg-get-project-pub-dir project))
         (filename (concat pub-dir "/page_" (int-to-string pagenum) ".html"))
         (visitingp (find-buffer-visiting filename))
         (work-buffer (or visitingp (find-file-noselect filename)))
         )

    (with-current-buffer work-buffer
      (erase-buffer)
      (org-sg-generate-site-page project pagenum)
      (save-buffer)
      )
    )
  )

(defun org-sg-generate-single-post (project post)
  (let* ((project-plist (cdr project))
         (filename (org-sg-get-output-file-name (plist-get post :file)))
         )
    (let* ((visitingp (find-buffer-visiting filename))
           (work-buffer (or visitingp (find-file-noselect filename))))
      (with-current-buffer work-buffer
        (erase-buffer)
        (org-sg-generate-full-post project post)
        (save-buffer)
        )
      )
    )
  )

(defun org-sg-generate-posts (project)
  (let* ((project-plist (cdr project))
         (posts (org-sg-get-posts project))
         result
         )
    (dolist (post posts result)
      (org-sg-generate-single-post project post)
      )
    )
  )

(defun org-sg-generate-pages (project)
  (let* ((project-plist (cdr project))
         (posts (org-sg-get-posts project))
         (posts-per-page (org-sg-get-posts-per-page project))
         (numposts (length posts))
         (numpages (org-sg-pagination-numpages numposts posts-per-page))
         result
         )
    (dotimes (numpage numpages)
      (org-sg-generate-site-page-file project numpage)
      )
    numposts
    )
  )



(defun org-sg-publish-completion-function()
  (if (boundp 'project)
      (progn
        (org-sg-generate-site-index-file project)
        (org-sg-generate-posts project)
        (org-sg-generate-pages project)
        )
      (error "variable 'project' is not defined"))
  )

(provide 'org-sg)
;;; org-sg.el ends here
