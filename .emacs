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
     (output-pdf "Evince")
     (output-html "xdg-open"))))
 '(cdlatex-paired-parens "$[{(")
 '(doc-view-continuous t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(org-latex-remove-logfiles t)
 '(package-selected-packages
   (quote
    (doom-modeline yasnippet-snippets bison-mode lex dockerfile-mode cdlatex magic-latex-buffer flycheck-golangci-lint go-mode go-snippets hindent smooth-scrolling ac-haskell-process flycheck-haskell haskell-mode haskell-snippets haskell-tab-indent hasky-stack evil-surround browse-kill-ring kill-ring-search ess focus helm indent-guide rainbow-delimiters smex sublimity workgroups2 beacon rainbow-mode matlab-mode org-edit-latex org-trello ox-asciidoc ox-gfm ox-html5slide ox-hugo ox-ioslide ox-minutes ox-pandoc mediawiki ox-mediawiki ac-clang ac-html ac-math aggressive-indent solarized-theme monokai-theme latex-extra latex-math-preview latex-pretty-symbols latex-preview-pane org-ac org-beautify-theme company-auctex flyspell-popup tuareg markdown-preview-mode org evil markdown-preview-eww markdown-mode+ markdown-mode auctex)))
 '(undo-tree-auto-save-history t))
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

;; Local elisp lib dir
(add-to-list 'load-path "~/.emacs.d/local")

;; Enabling evil mode (Vi controls)
(require 'evil)
(evil-mode 1)

;; Evil mode fine undo for granularity of undo
(setq evil-want-fine-undo t)
;; Enable surround mode for evil, easier manipulation of brackets
(global-evil-surround-mode 1)

;; Org-mode repo
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Org-mode keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Fix reference to free variable when using workgroups2 to restore open files
(defvar latex-mode)
(defvar latex-extra-mode)

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
(global-set-key (kbd "C-z") 'undo) ; Ctrl+z undo
(global-set-key (kbd "C-y") 'redo) ; Ctrl+y redo

;; Change spellchecker to aspell
(setq-default ispell-program-name "aspell")
(setq ispell-list-command "--list")

;; Enable flyspell by default in the following modes
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Enable flyspell-popup to automatically popup
(add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)
(defun turn-on-outline-minor-mode ()
(outline-minor-mode 1))

;; Outline mode in LaTeX
(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
(setq outline-minor-mode-prefix "\C-c \C-o")

;; RefTeX
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

(load-theme 'solarized-dark t)

;; Company mode
(company-mode 1)

;; Auto-load yasnippet
(require 'yasnippet)
(yas-global-mode t)
;; Allow nested snippet triggers
(setq yas-triggers-in-field t)

;;
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


;; Tuareg
(add-hook 'tuareg-interactive-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<up>") 'comint-previous-input)
	    )
	  )

;; Haskell, modes to enable by default
(add-hook 'haskell-mode-hook 'hindent-mode)
(add-hook 'haskell-mode-hook 'flycheck-mode)

;; Golang
(add-hook 'go-mode-hook 'flycheck-mode)

;; Commenting
;; C-x C-/ comments
(global-set-key [3 67108911] (quote comment-line))

;; Fonts
;; (add-to-list 'default-frame-alist
;;              '(font . "Fira Code-12"))

(global-aggressive-indent-mode 1)
(ac-config-default)
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
;; (require 'merlin)
(require 'helm-config)

(require 'focus)
(focus-mode 1)

(require 'indent-guide)
(indent-guide-global-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
					; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (require 'sublimity-map) ;; experimental
;; (require 'sublimity-attractive)

(sublimity-mode 1)

;; (require 'workgroups2)
;; Change some settings
;; (workgroups-mode 1)        ; put this one at the bottom of .emacs

(beacon-mode 1)

(require 'rainbow-mode)
(rainbow-mode 1)

;; Shortcut to browse kill ring
(global-set-key (kbd "C-x p") 'browse-kill-ring)
;; Change smerge command prefix, for dealing with git
(setq smerge-command-prefix "\C-cv")

;; Add ox-hugo support, export org files to Hugo compatible md
(with-eval-after-load 'ox
  (require 'ox-hugo))

;; Add auto-fill to text-mode by default
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Delete whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Load tutch-mode from local elisp dir
(load "tutch-mode")
(load "beluga-mode")

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

;; Smooth scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
;; (setq redisplay-dont-pause t
;;       scroll-margin 1
;;       scroll-step 1
;;       scroll-conservatively 10000
;;       scroll-preserve-screen-position 1)

;; Open new frames instead of windows
;; (setq pop-up-frames t)

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

(doom-modeline-mode 1)

;; Secondary file to load for things not included in the git repository/local customizations
(load-file "~/.emacs.local")
