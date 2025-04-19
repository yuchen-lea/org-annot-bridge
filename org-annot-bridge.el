;;; org-annot-bridge.el --- Build bridge between annot and org-mode.  -*- lexical-binding: t; -*-

;; Author: Yuchen Li
;; Url: https://github.com/yuchen-lea/org-annot-bridge
;; Package-Requires: ((emacs "24.4") (org "9.3") (transient "0.1.0"))

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

;;;;; PDF

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


;;;; Footer
(provide 'org-annot-bridge)
;;; org-annot-bridge.el ends here
