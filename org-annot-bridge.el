;;; org-annot-bridge.el --- Build bridge between annot and org-mode.  -*- lexical-binding: t; -*-

;; Author: Yuchen Li
;; Url: https://github.com/yuchen-lea/org-annot-bridge
;; Version: 0.0.2
;; Package-Requires: ((emacs "24.4") (transient "0.1.0"))

;;; Commentary:
;;; License:

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

;;; Code:
;;;; Requirements

(require 'bookmark)
(require 'transient)

;;;; Customization

(defgroup org-annot-bridge nil
  "Build bridge between annot and org note."
  :prefix "org-annot-bridge-"
  :group 'org)

(defcustom org-annot-bridge-image-dir org-directory
  "Directory to store extracted note images."
  :type 'directory)


(defcustom org-annot-bridge-image-zoom-factor 4.0
  "Default zoom factor for exported images."
  :type 'number)

(defcustom org-annot-bridge-annot-template nil
  "Template for annotation items using mako template syntax.

If this is nil, the default template from pdfhelper will be used.
For more details on the template syntax, see:
https://github.com/yuchen-lea/pdfhelper"
  :type 'string)

(defcustom org-annot-bridge-toc-template nil
  "Template for table of contents items using mako template syntax.

If this is nil, the default template from pdfhelper will be used.
For more details on the template syntax, see:
https://github.com/yuchen-lea/pdfhelper"
  :type 'string)

(defcustom org-annot-bridge-bib-files nil
  "List of paths to .bib files to find cite key."
  :type '(repeat file))

(defcustom org-annot-bridge-find-pdf-file-function (lambda ()
                                                     nil)
  "Function to find pdf file."
  :type 'function)

;;;; Commands

(defun org-annot-bridge-check-pdfhelper-version ()
  "Check if the pdfhelper program exists and its version is >= 2.3.1."
  (interactive)
  (let ((pdfhelper-path (executable-find "pdfhelper"))
        (required-version "2.3.1"))
    (if (not pdfhelper-path)
        (error "Command pdfhelper is not found. Please install it from https://github.com/yuchen-lea/pdfhelper")
      (let ((current-version (string-trim (shell-command-to-string "pdfhelper --version"))))
        (if (version< current-version required-version)
            (error "Your pdfhelper version (%s) is outdated. Please upgrade to the latest version from https://github.com/yuchen-lea/pdfhelper"
                   current-version)
          (message "pdfhelper version is sufficient."))))))

;;;; PDF
;;;;; PDF transient
(defun org-annot-bridge-pdfhelper-export-annot (&optional args)
  "Run `pdfhelper export-annot` with the provided options."
  (interactive (list (transient-args 'org-annot-bridge-export-pdf-annot-transient)))
  (org-annot-bridge-check-pdfhelper-version)
  (bookmark-set "org-annot-bridge-temp-bookmark")
  (let* ((pdf-file (or (funcall org-annot-bridge-find-pdf-file-function)
                       (read-file-name "Choose PDF file: "
                                       nil
                                       nil
                                       t
                                       nil
                                       (lambda (f)
                                         (string-match-p "\\.pdf\\'" f)))))
         (output-buffer (generate-new-buffer "*pdfhelper-output*"))
         (cmd (mapconcat #'identity
                         (list "pdfhelper export-annot"
                               (string-join (mapcar (lambda (arg)
                                                      (replace-regexp-in-string "=" " " arg))
                                                    args)
                                            " ")
                               (format "--annot-image-dir '%s'" org-annot-bridge-image-dir)
                               (if org-annot-bridge-bib-files
                                   (format "--bib-path %s"
                                           (mapconcat (lambda (item)
                                                        (format "'%s'" item))
                                                      org-annot-bridge-bib-files
                                                      " ")))
                               (if org-annot-bridge-annot-template
                                   (format "--annot-list-item-format '%s'" org-annot-bridge-annot-template))
                               (if org-annot-bridge-toc-template
                                   (format "--toc-list-item-format '%s'" org-annot-bridge-toc-template))
                               (format "'%s'" pdf-file))
                         " "))
         (async-shell-command-display-buffer nil)
         (proc (progn
                 (async-shell-command cmd output-buffer)
                 (get-buffer-process output-buffer))))
    (if (process-live-p proc)
        (set-process-sentinel proc
                              #'(lambda (process signal)
                                  (when (memq (process-status process)
                                              '(exit signal))
                                    (bookmark-jump "org-annot-bridge-temp-bookmark")
                                    (sleep-for 1)
                                    (goto-char (org-element-property :end (org-element-context)))
                                    (insert-buffer-substring (process-buffer process))
                                    (bookmark-delete "org-annot-bridge-temp-bookmark")
                                    (kill-buffer (process-buffer process))
                                    (shell-command-sentinel process signal))))
      (message-box "No process running."))))


(transient-define-prefix org-annot-bridge-export-pdf-annot-transient ()
  "Transient for `pdfhelper export-annot`."
  ["Arguments"
   ;; ("-o" "OCR Service" completing-read "OCR Service: " '("paddle" "ocrspace"))
   ;; ("-l" "OCR Language" completing-read "OCR Language: " '("zh-Hans" "zh-Hant" "en" "ja"))
   ;; ;; (if ocr-p
   ;; ;;     (format "--ocr-service '%s'" ocr-service)
   ;; ;;   "")
   ;; ;; (format "--ocr-language '%s'" ocr-language)
   ("z" "Image Zoom Factor" "--image-zoom=" :always-read t  :allow-empty nil
    :init-value (lambda (obj) (oset obj value (number-to-string org-annot-bridge-image-zoom-factor))))
   ("t" "With TOC" "--with-toc"
    :init-value (lambda (obj) (oset obj value "--with-toc")))
   ("s" "Creation Start" "--creation-start=" :prompt "Annot Creation Start YYYY-MM-DD: ")
   ("e" "Creation End" "--creation-end=" :prompt "Annot Creation End YYYY-MM-DD: ")
   ("r" "Run Test"  "--run-test")]
  ["Actions"
   ("RET" "export-annot" org-annot-bridge-pdfhelper-export-annot)])

;;;;; PDF link

(defcustom org-annot-bridge-pdf-link-prefix "pdf"
  "Prefix for pdf link"
  :type 'string)

(defcustom org-annot-bridge-path-generator #'abbreviate-file-name
  "Translate file path the way you like. Take full-path as the argument."
  :type 'function)

(defcustom org-annot-bridge-path-resolver #'expand-file-name
  "Resolve your translated PDF file path back to an absolute path."
  :type 'function)

(require 'pdf-view)

;;;###autoload
(defun org-annot-bridge-store-pdf-link ()
  "Store a link to a pdfview buffer."
  (cond
   ((eq major-mode 'pdf-view-mode)
    (let* ((file (funcall org-annot-bridge-path-generator (pdf-view-buffer-file-name)))
           (page (number-to-string (pdf-view-current-page)))
           (height (org-annot-bridge--pdf-height-percent))
	   (locator (if (string= "0.00" (org-annot-bridge--pdf-height-percent))
			 page
		       (format "%s++%s" page height))))
      ;; pdf://path::page++height_percent
      (org-link-store-props :type org-annot-bridge-pdf-link-prefix
                              :link (format "%s:%s::%s"
					    org-annot-bridge-pdf-link-prefix
                                            file locator)
                              :description locator)))))

(defun org-annot-bridge--pdf-height-percent ()
  "Return current pdf height percent, a float value between 0 and 1."
  (let* ((height (/ (or (image-mode-window-get 'vscroll)
			0)
		    (float (cdr (pdf-view-image-size))))))
    (format "%.2f" height)))

;; pdf://path::page++height_percent
(defun org-annot-bridge-open-pdf-link (link)
  "Internal function to open pdf LINK."
  (let ((link-regexp "\\(.*\\)::\\([0-9]*\\)\\(\\+\\+\\)?\\([[0-9]\\.*[0-9]*\\)?"))
    (cond
     ((string-match link-regexp link)
      (let ((path (match-string 1 link))
            (page (match-string 2 link))
            (height (match-string 4 link)))
        (if (and path
                 (not (string-empty-p path))
                 (file-exists-p path))
            (org-open-file (funcall org-annot-bridge-path-resolver path)
                           1))
        (if (and page
                 (not (string-empty-p page)))
            (progn
              (setq page (string-to-number page))
              (pdf-view-goto-page page))
          (setq page nil))
        (when (and height
                   (not (string-empty-p height)))
          (image-set-window-vscroll (round (* (string-to-number height)
                                                 (cdr (pdf-view-image-size))))))))
     ((org-open-file link 1)))))
;;;###autoload
(defun org-annot-bridge-setup-link ()
  "Set up pdf: links in org-mode."
  (org-link-set-parameters org-annot-bridge-pdf-link-prefix
                           :follow #'org-annot-bridge-open-pdf-link
                           :store #'org-annot-bridge-store-pdf-link))
;;;; Footer
(provide 'org-annot-bridge)
;;; org-annot-bridge.el ends here
