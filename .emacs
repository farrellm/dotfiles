;;; .emacs --- Summary
;;; Commentary:

;; elpa packages I use
;;  ac-nrepl
;;  auctex
;;  auto-complete
;;  cider
;;  clojure-mode
;;  lua-mode
;;  rainbow-delimiters
;;  smartparens
;;  zenburn-theme
;; installed manually
;;  ess

;;; Code:

;; basic UI stuff
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(when window-system
  ;; (set-face-attribute 'default nil :height 80)
  (set-face-attribute 'default nil :font "Droid Sans Mono-9")
  (blink-cursor-mode 0)
  (setq-default cursor-type 'bar)

  ;; zenburn theme
  (eval-after-load "zenburn-theme-autoloads" '(load-theme 'zenburn)))

;; whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; don't ask about symlinks to files in version control
(setq vc-follow-symlinks t)

;; paren highlight
(show-paren-mode t)
(set-face-foreground 'show-paren-match "white")
(eval-after-load "rainbow-delimiters-autoloads"
  '(global-rainbow-delimiters-mode))

;; backup files
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; auto-revert buffers
(global-auto-revert-mode t)

;; It's annoying to have to type 'yes' to answer questions. One letter is enough for confirmation.
(fset 'yes-or-no-p 'y-or-n-p)

;; Ido, which is short for "Interactively Do Things," is an amazingly efficient way to select files and switch buffers.
(ido-mode t)

;; terminal mouse support
(xterm-mouse-mode t)

;; Auctex sync with Evince
(require 'dbus)

;; latex
(defvar TeX-save-query nil) ;;autosave before compiling

(defvar TeX-auto-save t)
(defvar TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(defvar TeX-PDF-mode t)

;; smartparens
(eval-after-load "smartparens-autoloads"
  '(progn
     (require 'smartparens-config)
     (smartparens-global-mode t)
     (smartparens-global-strict-mode t)

     ;; highlights matching pairs
     (show-smartparens-global-mode t)

     ;; keybinding management
     (sp-use-smartparens-bindings)

     ;; pair management
     (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

     ;; markdown-mode
     (sp-with-modes '(markdown-mode gfm-mode rst-mode)
       (sp-local-pair "*" "*" :bind "C-*")
       (sp-local-tag "2" "**" "**")
       (sp-local-tag "s" "```scheme" "```")
       (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

     ;; tex-mode latex-mode
     (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
       (sp-local-tag "i" "\"<" "\">"))

     ;; html-mode
     (sp-with-modes '(html-mode sgml-mode)
       (sp-local-pair "<" ">"))

     ;; lisp modes
     (sp-with-modes sp--lisp-modes
       (sp-local-pair "(" nil :bind "C-("))

     (require 'smartparens-latex)
     ))

(defun sp-forward-up-sexp ()
  "jump to end of current or outer sexp"
  (interactive)
  (evil-emacs-state)
  (forward-char)
  (sp-up-sexp)
  (evil-normal-state))

;; auto-complete
(defvar ac-ignore-case nil)

(eval-after-load 'auto-complete
  '(progn
     (add-to-list 'ac-modes 'R-mode)
     (add-to-list 'ac-modes 'latex-mode)
     (ac-flyspell-workaround)
     (setq ac-ignore-case nil)))
(eval-after-load 'auto-complete-config '(ac-config-default))

(add-hook 'after-init-hook
	  (lambda ()
	    (require 'auto-complete)
	    (require 'auto-complete-config)
	    (require 'ac-math)))

(defun ac-latex-mode-setup ()    ; add ac-sources to default ac-sources
  (interactive)
  (add-to-list 'ac-sources 'ac-source-math-latex)
  (add-to-list 'ac-sources 'ac-source-latex-commands))
(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; clojure
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)

;; Configure cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(defvar nrepl-hide-special-buffers t)
(defvar nrepl-popup-stacktraces-in-repl t)
(defvar nrepl-history-file "~/.emacs.d/nrepl-history")
(add-hook 'nrepl-mode-hook 'subword-mode)

;; Auto completion for cider
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; clojure
(defvar clojure-defun-indents
      '(match translate rotate scale mirror extrude-linear extude-rotate))

;; haskell
(defvar haskell-font-lock-symbols t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; ESS
(add-to-list 'load-path "/home/mfarrell/.emacs.d/ESS/lisp")
(require 'ess-site)
;; (add-hook 'after-init-hook
;;   (require 'ess-pkg)
;;   (add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode)))

(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

(defun th-evince-sync (file linecol &rest ignored)
  (let* ((fname (un-urliy file))
         (buf (find-buffer-visiting fname))
         (line (car linecol))
         (col (cadr linecol)))
    (if (null buf)
        (message "[Synctex]: %s is not opened..." fname)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
        (move-to-column col)))))

(defvar *dbus-evince-signal* nil)

(defun enable-evince-sync ()
  (require 'dbus)
  (when (and
         (eq window-system 'x)
         (fboundp 'dbus-register-signal))
    (unless *dbus-evince-signal*
      (setf *dbus-evince-signal*
            (dbus-register-signal
             :session nil "/org/gnome/evince/Window/0"
             "org.gnome.evince.Window" "SyncSource"
             'th-evince-sync)))))

(add-hook 'LaTeX-mode-hook 'enable-evince-sync)

;; functions for key-bindings
(defun prev-window ()
  "previous window"
  (interactive)
  (other-window -1))

(defun smart-comment ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (sp-comment)))

;; key bindings
(global-set-key (kbd "C-x p") 'prev-window)
(global-set-key (kbd "C-x x") 'execute-extended-command)
(global-set-key (kbd "M-\e \e") 'keyboard-quit)
(global-set-key (kbd "C-X C-b") 'ibuffer)
(global-set-key (kbd "C-X g") 'goto-line)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
;(global-set-key (kbd "C-;") 'smart-comment)


;; ELPA
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-safe-themes (quote ("16248150e4336572ff4aa21321015d37c3744a9eb243fbd1e934b594ff9cf394" "11d069fbfb0510e2b32a5787e26b762898c7e480364cbc0779fe841662e4cf5d" default)))
 '(ess-swv-pdflatex-commands (quote ("pdflatex" "texi2pdf" "make")))
 '(ess-swv-processor (quote knitr))
 '(fci-rule-color "#383838")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
