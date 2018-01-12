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
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Atril")
     (output-html "xdg-open"))))
 '(doc-view-continuous t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(org-latex-remove-logfiles t)
 '(package-selected-packages
   (quote
    (matlab-mode org-edit-latex org-trello ox-asciidoc ox-gfm ox-html5slide ox-hugo ox-ioslide ox-minutes ox-pandoc mediawiki ox-mediawiki ac-clang ac-html ac-math aggressive-indent solarized-theme monokai-theme latex-extra latex-math-preview latex-pretty-symbols latex-preview-pane org-ac org-beautify-theme company-auctex flyspell-popup tuareg markdown-preview-mode org evil markdown-preview-eww markdown-mode+ markdown-mode auctex)))
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

;; Enabling evil mode (Vi controls)
    (require 'evil)
    (evil-mode 1)

;; Evil mode fine undo for granularity of undo
(setq evil-want-fine-undo t)

;; Org-mode repo
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Org-mode keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

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

;;
(add-hook 'org-mode-hook 'org-ac)

;; More LaTeX stuff
;; Don't override font-map in latex-extra-mode
;; Disable remap of C-c C-f from latex-extra-mode
(defvar latex/override-font-map nil)
(add-hook 'LaTeX-mode-hook 'latex-extra-mode)
(latex-preview-pane-enable)
;; Enable preview pane mode by default
;; (add-hook 'LaTeX-mode-hook
;; 	  (lambda ()
;; 	    add-hook 'latex-preview-pane-mode 'make-it-local
;; 	    )
;; 	  )
;; (add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)
(add-hook 'LaTeX-mode-hook 'company-mode)

;; Tuareg
(add-hook 'tuareg-interactive-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<up>") 'comint-previous-input)
	    )
	  )

;; Commenting
;; C-x C-/ comments
(global-set-key [3 67108911] (quote comment-line))

;; Fonts
(add-to-list 'default-frame-alist
	     '(font . "Fira Code-12"))

(setq org-agenda-files (list "~/Dropbox/1.Org"))
(global-aggressive-indent-mode 1)
(ac-config-default)
;; C
;; (setq-default c-basic-offset 4 c-default-style "linux")
;; (setq-default tab-width 4 indent-tabs-mode t)
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
(require 'merlin)
