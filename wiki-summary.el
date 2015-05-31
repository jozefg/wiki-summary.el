;; -*- lexical-binding: t -*-
;;; wiki-summary.el --- View Wikipedia summaries in Emacs easily.

;; Copright (C) 2015 Danny Gratzer <jozefg@cmu.edu>

;; Author: Danny Gratzer
;; URL: https://github.com/jozefg/wiki-summary.el
;; Package-Version: 20150408.1422
;; Keywords: wikipedia, utility
;; Package-Requires: ((emacs "24"))
;; Version: 0.2

;;; Commentary:

;; It's often the case when reading some document in Emacs (be it
;; code text or prose) that I come across a word or phrase that I
;; don't know. In order to simplify my feedback loop when wiki-summary
;; lets me look up something in a couple seconds.
;;
;; To use this package, simply call M-x wiki-summary (or bind it to a key).
;; This will prompt you for an article title to search. For convience,
;; this will default to the word under the point. When you hit enter
;; this will query Wikipedia and if an article is found, bring up the
;; title in a separate window. Spaces will be properly escaped so
;; something like "Haskell (programming language)" will bring up the
;; intended page.
;;
;; I'm not sure exactly what else people would want out of this package.
;; Feature request and issues are welcome.

(require 'url)
(require 'json)
(require 'thingatpt)
(require 'cl)

(eval-when-compile
  ;; This stops the compiler from complaining.
  (defvar url-http-end-of-headers))

;;;###autoload
(defun wiki-summary/make-api-query (s)
  "Given a wiki page title, generate the url for the API call
   to get the page info"
  (let ((pre "http://en.wikipedia.org/w/api.php?continue=&action=query&titles=")
        (post "&prop=extracts&exintro=&explaintext=&format=json&redirects")
        (term (wiki-summary/url-format-search-term s)))
    (concat pre term post)))

;;;###autoload
(defun wiki-summary/url-format-search-term (search-term)
  "Format the string for URL."
  (url-hexify-string (replace-regexp-in-string " " "_" search-term)))

;;;###autoload
(defun wiki-summary/insert-button-for-full-page (search-term)
  "Insert a button into the current buffer to browse the full article for a page"
  (let ((url (concat "http://en.wikipedia.org/?search="
                     (wiki-summary/url-format-search-term search-term))))
    (insert-button "Read the full article"
                   'action (lambda (x) (browse-url (button-get x 'url)))
                   'url url)))

;;;###autoload
(defun wiki-summary/extract-summary (resp)
  "Given the JSON reponse from the webpage, grab the summary as a string"
  (let* ((query (plist-get resp 'query))
         (pages (plist-get query 'pages))
         (info (cadr pages)))
    (plist-get info 'extract)))

;;;###autoload
(defun wiki-summary/tidy-up-displayed-buffer ()
  "Tidies up the current buffer (presumed to be the wiki-summary buffer)"
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\n\n"))
  (fill-region (point-min) (point-max)))


;;;###autoload
(defun wiki-summary/format-summary-in-buffer (summary search-term)
  "Given a summary, stick it in the *wiki-summary* buffer and display the buffer"
  (let ((buf (get-buffer-create (concat "*wiki-summary: " search-term "*"))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (princ summary buf)
        (insert "\n")
        (wiki-summary/insert-button-for-full-page search-term)
        (wiki-summary/tidy-up-displayed-buffer)
        (view-mode 1)
        (goto-char (point-min))))
    (display-buffer buf)))

;;;###autoload
(defun wiki-summary/parse-json ()
  "Parse the json returned from the Wikipedia API. This returns a Lisp term"
  (goto-char url-http-end-of-headers)
  (let ((json-object-type 'plist)
        (json-key-type 'symbol)
        (json-array-type 'vector))
    (json-read)))

;;;###autoload
(defun wiki-summary (s)
  "Return the wikipedia page's summary for a term"
  (interactive
   (list
    (read-string (concat
                  "Wikipedia Article"
                  (if (thing-at-point 'word)
                      (concat " (" (thing-at-point 'word) ")")
                    "")
                  ": ")
                 nil
                 nil
                 (thing-at-point 'word))))

  (save-excursion
    (let ((query-url (wiki-summary/make-api-query s)))
      (url-retrieve query-url
                    (lambda (status s)
                      (when (plist-member status :error)
                        (message "Couldn't retrieve URL")
                        (return))

                      (message "") ; Clear the annoying minibuffer display
                      (let* ((result (wiki-summary/parse-json))
                             (summary (wiki-summary/extract-summary result)))
                        (if summary
                            (wiki-summary/format-summary-in-buffer summary s)
                          (message "No article found"))))
                    (list s)))))

(provide 'wiki-summary)

;;; wiki-summary.el ends here
