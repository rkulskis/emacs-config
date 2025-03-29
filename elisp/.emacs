;; Initialize package system if not already initialized
(unless (package-installed-p 'use-package)
  (package-initialize))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
(defun my-java-mode-hook ()
  (setq tab-width 2)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 2))  ;; Set basic indentation level for Java

(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun org-require-package (package)
  "Ensure PACKAGE is installed."
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))
(use-package magit
	:ensure t)
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))
(set-face-attribute 'default nil :height 110)

;; Enable minted for LaTeX export in Org mode
(setq org-latex-listings 'minted)

;; Define the LaTeX class to include minted in the preamble
(setq org-latex-packages-alist '(("" "minted")))

;; Add options for minted (optional, customize as needed)
(setq org-latex-minted-options
      '(("frame" "lines")
        ("linenos" "true")
        ("breaklines" "true")
        ("fontsize" "\\small")))

;; Ensure LaTeX is called with the -shell-escape option (required for minted)
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Disable the splash screen
(setq inhibit-splash-screen t)

;; UI Configuration
(menu-bar-mode -1)   ;; Hide menu bar
(tool-bar-mode -1)   ;; Hide tool bar
(setq initial-buffer-choice t) ; Opens *scratch* by default
(add-to-list 'default-frame-alist '(fullscreen . maximized)) 
(setq org-file-apps
    (quote
        ((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . "/usr/bin/firefox %s")
        ("\\.pdf\\'" . default))))
;; Enable line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'visual)

(defun bugfix-display-line-numbers--turn-on (fun &rest args)
  "Avoid display-line-numbers-mode' in image-mode' and related modes."
  (unless (derived-mode-p 'image-mode 'docview-mode 'pdf-view-mode)
    (apply fun args)))
(advice-add 'display-line-numbers--turn-on :around #'bugfix-display-line-numbers--turn-on)

;; Disable line numbers in certain modes
(add-hook 'doc-view-mode-hook 'disable-line-numbers-in-doc-view)

;; (add-to-list 'load-path "~/.config/emacs-config/emacs-reveal")
;; (require 'emacs-reveal)

(add-to-list 'load-path "~/.config/emacs-config/emacs-libvterm")
(require 'vterm)

;; Disable both horizontal and vertical scrollbars
(scroll-bar-mode -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 '(auctex disable-mouse ein eink-theme elfeed-org elfeed-score forge
						helm latex-preview-pane magit nov org-ref pdf-tools rustic
						yaml-mode))
 '(safe-local-variable-values '((org-src-preserve-indentation . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq org-src-fontify-natively t)
(setq org-export-html-prioritize-blocks t)  ;; For HTML export
(setq org-export-with-smart-quotes t)      ;; Optional: for nicer quote marks

;; Enable Org-Babel languages, including Python, Bash, and C
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)   ;; Enable Bash
   (C . t)))     ;; Enable C
(setq org-babel-python-command "python3")
(setq org-confirm-babel-evaluate nil)

;; PDF Tools
(pdf-tools-install)
(pdf-loader-install)
(setq pdf-view-display-size 'fit-page) ; Fit the page to the window size
(setq pdf-view-continuous t) ; Enable continuous scrolling

;; for easy scheduling TODO items
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files '("~/todo/"))
;; (setq tags-table-list '("~/TAGS" "~/dev/quest/quest_driveos"))
(defun man-at-point ()
  "Pull up the man page for the symbol at point."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (if symbol
        (man symbol)
      (message "No symbol at point"))))

