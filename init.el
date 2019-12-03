;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open"))))
 '(cdlatex-paired-parens "$[{(")
 '(doc-view-continuous t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(org-latex-remove-logfiles t)
 '(package-selected-packages
   (quote
    (flyspell-popup rainbow-delimiters company solarized-theme yasnippet aggressive-indent haskell-mode go-mode smooth-scrolling imenu-list doom-modeline rainbow-mode sublimity smex indent-guide focus evil undo-tree auto-package-update use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(require 'package)
;; Add Melpa to package list
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

;; Org-mode repo
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Local elisp lib dir
(add-to-list 'load-path "~/.emacs.d/local")

;; Install use-package if it isn't installed to allow installing packages that aren't installed
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

;; Auto-update every 2 days
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 2)
  (auto-package-update-maybe))
;; Always ensure
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install packages listed in 'package-selected-packages
;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))
;; (package-install-selected-packages)

(use-package markdown-mode)
(use-package imenu-anywhere)
(use-package popup-imenu)
(use-package powershell)
(use-package yasnippet-snippets)
(use-package bison-mode)
(use-package lex)
(use-package dockerfile-mode)
(use-package flycheck-golangci-lint)
(use-package go-snippets)
(use-package hindent)
(use-package ac-haskell-process)
(use-package flycheck-haskell)
(use-package haskell-snippets)
(use-package haskell-tab-indent)
(use-package browse-kill-ring)
(use-package kill-ring-search)
(use-package indent-guide)
(use-package latex-math-preview)
;; (use-package matlab-mode)
(use-package org-ac)
(use-package org-edit-latex)
(use-package org-beautify-theme)
(use-package company-auctex)
(use-package ess)
(use-package polymode
  :ensure markdown-mode
  :ensure poly-R
  :mode ("\\.Rmd" . poly-markdown+r-mode))

(use-package latex
  :ensure auctex)

(use-package undo-tree
  :init (global-undo-tree-mode)
  :config (setq undo-tree-auto-save-history t))

(use-package evil
  :init (evil-mode 1)
  :config (setq evil-want-fine-undo t))

(use-package evil-surround
  :init (global-evil-surround-mode 1))

(use-package org
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cc" . org-capture)
         ("\C-cb" . org-iswitchb))
  :config
  ;; org-goto
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil))

;;(use-package helm
;;  :defer t)

(use-package focus
  :init (focus-mode 1))

(use-package indent-guide
  :init (indent-guide-global-mode))

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command))
  :init (smex-initialize))

(use-package sublimity
  :init (sublimity-mode 1))

(use-package beacon
  :init (beacon-mode 1))

(use-package rainbow-mode
  :init (rainbow-mode 1))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package reftex
  :hook ((LaTeX-mode . turn-on-reftex)   ; with AUCTeX LaTeX mode
         (latex-mode . turn-on-reftex)   ; with Emacs latex mode
         ))

(use-package imenu-list
  :bind ("C-'" . imenu-list-smart-toggle))

(use-package smooth-scrolling
  :init (smooth-scrolling-mode 1))

(use-package go-mode
  :hook (go-mode . flycheck-mode))

(use-package haskell-mode
  :hook ((haskell-mode . hindent-mode)
         (haskell-mode . flycheck-mode)))

(use-package aggressive-indent
  :init (global-aggressive-indent-mode 1))

(use-package tuareg
  :hook (tuareg-interactive-mode . (lambda ()
                                     (local-set-key (kbd "<up>") 'comint-previous-input)
                                     )))

(use-package yasnippet
  :init (yas-global-mode t)
  :config (setq yas-triggers-in-field t))

(use-package solarized-theme
  :init (load-theme 'solarized-dark t))

(use-package company
  :init (company-mode 1))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (text-mode . rainbow-delimiters-mode)))

(add-hook 'LaTeX-mode-hook 'rainbow-delimiters-mode)
(use-package flyspell
  :hook ((org-mode . turn-on-flyspell)
         (text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (flyspell-mode . flyspell-popup-auto-correct-mode)))

(use-package flyspell-popup
  :hook (flyspell-mode . flyspell-popup-auto-correct-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF USE-PACKAGEs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load tutch-mode from local elisp dir
(load "tutch-mode")
(load "beluga-mode")

;; Fix reference to free variable
(defvar latex-mode)
(defvar latex-extra-mode)

(setq gc-cons-threshold 100000000)

;; Enable column-number-mode by default
(setq column-number-mode t)

;; Enable these modes by default
(electric-pair-mode 1)
(show-paren-mode 1)
;; Make linum-mode a global minor under the name global-linum-mode
(define-globalized-minor-mode global-linum-mode linum-mode
  (lambda () (linum-mode 1)))

(global-linum-mode 1)

(global-visual-line-mode t) ;; To wrap lines like on other text editors like Notepad

;; Highlight LaTeX stuff in org-mode
(eval-after-load 'org
  '(setf org-highlight-latex-and-related '(latex)))

;; Keybindings
;;(global-set-key (kbd "C-z") 'undo) ; Ctrl+z undo
;;(global-set-key (kbd "C-y") 'redo) ; Ctrl+y redo

;; Change spellchecker to aspell
(setq-default ispell-program-name "aspell")
(setq ispell-list-command "--list")

(defun turn-on-outline-minor-mode ()
  (outline-minor-mode 1))

;; Outline mode in LaTeX
(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
(setq outline-minor-mode-prefix "\C-c \C-o")

(add-hook 'org-mode-hook 'org-ac)

;; Org export to another folder
(defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  (unless pub-dir
    (setq pub-dir "exported-org-files")
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))
(advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

;; More LaTeX stuff
;; Default to xetex and compile to pdf
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)
(setq-default TeX-parse-self t)
;; Don't override font-map in latex-extra-mode
;; Disable remap of C-c C-f from latex-extra-mode
(defvar latex/override-font-map nil)
(add-hook 'LaTeX-mode-hook 'latex-extra-mode)
;; (latex-preview-pane-enable)
;; Enable preview pane mode by default
;; (add-hook 'LaTeX-mode-hook
;; 	  (lambda ()
;; 	    add-hook 'latex-preview-pane-mode 'make-it-local
;; 	    )
;; 	  )
;; (add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)
(add-hook 'LaTeX-mode-hook 'company-mode)
(add-hook 'LaTeX-mode-hook 'flycheck-mode)
;; (add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
;; (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
;; (setq magic-latex-enable-block-highlight nil
;;       magic-latex-enable-suscript        t
;;       magic-latex-enable-pretty-symbols  t
;;       magic-latex-enable-block-align     nil
;;       magic-latex-enable-inline-image    nil
;;       magic-latex-enable-minibuffer-echo nil)


;; Commenting
;; C-x C-/ comments
(global-set-key [3 67108911] (quote comment-line))

;; Fonts
(add-to-list 'default-frame-alist
             '(font . "Fira Code-11"))

;;(ac-config-default)
;; C
(setq-default c-basic-offset 4 c-default-style "linux")
;; (setq-default tab-width 4 indent-tabs-mode t)

;; Tab size to be 4 spaces
(setq tab-stop-list (number-sequence 4 200 4))
(setq tab-width 4)
;; Disable tab indentation, use spaces
(setq-default indent-tabs-mode nil)

;; (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(global-set-key "\C-x\C-z" 'compile)
;; The following either compiles with a makefile or makes command to compile current file as its name
(require 'compile)
(add-hook 'c-mode-hook
	  (lambda ()
	    (unless (file-exists-p "Makefile")
	      (set (make-local-variable 'compile-command)
		   ;; emulate make's .c.o implicit pattern rule, but with
		   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
		   ;; variables:
		   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
		   (let ((file (file-name-nondirectory buffer-file-name)))
		     (format "%s -o %s %s %s %s"
			     (or (getenv "CC") "gcc")
			     (file-name-sans-extension file)
			     (or (getenv "CPPFLAGS") "-DDEBUG=9")
			     (or (getenv "CFLAGS") " -pedantic -Wall -g")
			     file))))))
(electric-pair-mode)
(add-hook 'LaTeX-mode-hook
          '(lambda ()
			 (define-key LaTeX-mode-map (kbd "$") 'self-insert-command)))

(add-hook 'org-mode-hook
          '(lambda ()
			 (define-key local-set-key (kbd "$") 'self-insert-command)))
;; (eval-after-load 'LaTeX
;;   (progn
;;     (define-key LaTeX-mode-map (kbd "\C-f") nil)))
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; Shortcut to browse kill ring
(global-set-key (kbd "C-x p") 'browse-kill-ring)
;; Change smerge command prefix, for dealing with git
(setq smerge-command-prefix "\C-cv")

;; Add ox-hugo support, export org files to Hugo compatible md
;; (with-eval-after-load 'ox
;;   (require 'ox-hugo))

;; Add auto-fill to text-mode by default
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Delete whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; R stuff
;; Allow evaluation of R and latex in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (latex . t)))
;; No confirmation for evaluation
(setq org-confirm-babel-evaluate nil)

;; R Markdown for .Rmd files
;; (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown-mode))

;; Disable GUI stuff
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Kill emacs instead of just closing (for server mode)
;; (global-set-key "\C-x\C-c" 'kill-emacs)

(global-set-key "\C-x\C-b" 'buffer-menu)

;; Set yes-or-no to always be y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; Secondary file to load for things not included in the git repository/local customizations
(load-file "~/.emacs.d/.emacs.local")
