;; elpa packages I use
;;  ac-nrepl
;;  auctex
;;  auto-complete
;;  clojure-mode
;;  color-theme
;;  color-theme-sol...
;;  ess
;;  evil
;;  golden-ratio
;;  lua-mode
;;  nrepl
;;  nrepl-ritz
;;  paredit
;;  popup
;;  rainbow-delimiters
;;  undo-tree
;;  zenburn-theme

(defun delete-from-list (lst obj)
  "delete obj from a lst"
  (eval `(setq ,lst (delete obj (eval lst)))))

(defun stop-ac-and-normal ()
  (interactive)
  (ac-stop)
  (evil-normal-state))

;; after packages are loaded
(defun after-packages ()
  "extra initialization to run after elpa"
  ;; evil mode
  (evil-mode)

  (add-to-list 'evil-emacs-state-modes 'minibuffer-inactive-mode)
  (add-to-list 'evil-insert-state-modes 'inferior-ess-mode)
  (add-to-list 'evil-motion-state-modes 'package-menu-mode)
  (add-to-list 'evil-motion-state-modes 'customize-mode)

  (delete-from-list 'evil-emacs-state-modes 'customize-mode)
  (delete-from-list 'evil-emacs-state-modes 'ibuffer-mode)
  (delete-from-list 'evil-emacs-state-modes 'package-menu-mode)

  ;; evil re-maps
  (evil-global-set-key 'visual (kbd ";") 'comment-or-uncomment-region)
  (evil-global-set-key 'insert (kbd "C-j") 'other-window)
  (evil-global-set-key 'insert (kbd "C-k") 'prev-window)

  (define-key evil-ex-map [escape] 'abort-recursive-edit)

  ;; parens
  (global-rainbow-delimiters-mode)

  ;; nrepl
  (require 'nrepl)

  ;; Configure nrepl.el
  (setq nrepl-hide-special-buffers t)
  (setq nrepl-popup-stacktraces-in-repl t)
  (setq nrepl-history-file "~/.emacs.d/nrepl-history")
  
  ;; Some default eldoc facilities
  (add-hook 'nrepl-connected-hook
	    (defun pnh-clojure-mode-eldoc-hook ()
	      (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
	      (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
	      (nrepl-enable-on-existing-clojure-buffers)))
  
  ;; Repl mode hook
  (add-hook 'nrepl-mode-hook 'subword-mode)

  ;; Ritz middleware
  (require 'nrepl-ritz) ;; after (require 'nrepl)
 
  (define-key nrepl-interaction-mode-map (kbd "C-c C-j") 'nrepl-javadoc)
  (define-key nrepl-mode-map (kbd "C-c C-j") 'nrepl-javadoc)
  (define-key nrepl-interaction-mode-map (kbd "C-c C-a") 'nrepl-apropos)
  (define-key nrepl-mode-map (kbd "C-c C-a") 'nrepl-apropos)

  ;; auto-complete
  (require 'auto-complete)
  (add-to-list 'ac-modes 'R-mode)
  (add-to-list 'ac-modes 'latex-mode)

  (require 'auto-complete-config)
  (ac-config-default)

  ;; Auto completion for NREPL
  (require 'ac-nrepl)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'nrepl-mode))
  (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
  ;; (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

  (setq ac-use-menu-map t)
  (define-key ac-completing-map "\e" 'stop-ac-and-normal)
  (define-key ac-menu-map "\e" 'keyboard-quit)
  (define-key ac-menu-map (kbd "C-j") 'ac-next)
  (define-key ac-menu-map (kbd "C-k") 'ac-previous)
  (define-key ac-menu-map (kbd "\r") 'ac-complete)

  ;; golden-ratio
  (golden-ratio-enable)

  ;; clojure
  (setq clojure-defun-indents
	'(match translate rotate scale mirror extrude-linear extude-rotate))

  ;; color theme
  ;; (load-theme 'solarized-dark)
  ;; (load-theme 'solarized-light)
  (load-theme 'zenburn)
  )

(add-hook 'after-init-hook 'after-packages)

;; basic UI stuff
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(when window-system
  ;; (set-face-attribute 'default nil :height 80)
  (set-face-attribute 'default nil :font "Droid Sans Mono-9")
  (blink-cursor-mode 0))

;; don't ask about symlinks to files in version control
(setq vc-follow-symlinks t)

;; paren highlight 
(show-paren-mode t)
(set-face-foreground 'show-paren-match "white")

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


;; latex
(add-hook 'LaTeX-mode-hook (lambda () (TeX-global-PDF-mode)))
(setq TeX-save-query nil) ;;autosave before compiling

;; Evil
(setq evil-emacs-state-cursor "#dc322f")
(setq evil-normal-state-cursor '("#dcdccc"))
(setq evil-motion-state-cursor '("#dcdccc"))
(setq evil-insert-state-cursor '(bar "#dcdccc"))
(setq evil-operator-state-cursor '(evil-half-cursor "#dcdccc"))
(setq evil-replace-state-cursor '(hbar "#dcdccc"))
(setq evil-visual-state-cursor '(hollow "#dcdccc"))

;; ESS
;; (add-to-list 'load-path "/home/mfarrell/.emacs.d/elpa/ess-20130225.1754/lisp")
(add-to-list 'load-path "/home/mfarrell/.emacs.d/ESS/lisp")
(require 'ess-site)

;; Auctex sync with Evince
(require 'dbus)

(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

(defun th-evince-sync (file linecol &rest ignored)
  (let* ((fname (un-urlify file))
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

;; Predictive - my laptop is too slow :(
;; (add-to-list 'load-path "~/.emacs.d/predictive")
;; (add-to-list 'load-path "~/.emacs.d/predictive/latex")
;; (add-to-list 'load-path "~/.emacs.d/predictive/texinfo")
;; (add-to-list 'load-path "~/.emacs.d/predictive/html")
;; (require 'predictive)

;; fix keybindings for iBuffer
(eval-after-load 'ibuffer
  '(progn
     ;; use the standard ibuffer bindings as a base
     (set-keymap-parent
      (evil-get-auxiliary-keymap ibuffer-mode-map 'normal t)
      (assq-delete-all 'menu-bar (copy-keymap ibuffer-mode-map)))
     (evil-define-key 'normal ibuffer-mode-map "j" 'ibuffer-forward-line)
     (evil-define-key 'normal ibuffer-mode-map "k" 'ibuffer-backward-line)
     (evil-define-key 'normal ibuffer-mode-map "J" 'ibuffer-jump-to-buffer) ; "j"
     (evil-define-key 'normal ibuffer-mode-map (kbd "DEL") 'ibuffer-unmark-backward)))

;; Paredit
(load-file "/home/mfarrell/.emacs.d/ts-paredit.elc")

;; functions for key-bindings
(defun prev-window ()
  "previous window"
  (interactive)
  (other-window -1))

(defun delete-window-or-frame ()
  "close frame or quit if last frame"
  (interactive)
  (if (= 1 (count-windows))
      (delete-frame)
    (delete-window)))

;; key bindings
(global-set-key (kbd "C-x p") 'prev-window)
(global-set-key (kbd "C-x x") 'execute-extended-command)
(global-set-key (kbd "M-\e \e") 'keyboard-quit)
(global-set-key (kbd "C-X C-b") 'ibuffer)
(global-set-key (kbd "C-c f") 'font-lock-fontify-buffer)

;;; minor mode for key overrides
(defvar keys-minor-mode-map (make-keymap) "keys-minor-mode keymap.")

(define-key keys-minor-mode-map (kbd "C-j") 'other-window)
(define-key keys-minor-mode-map (kbd "C-k") 'prev-window)
(define-key keys-minor-mode-map (kbd "C--") 'split-window-below)
(define-key keys-minor-mode-map (kbd "C-_") 'split-window-below)
(define-key keys-minor-mode-map (kbd "C-\\") 'split-window-right)
(define-key keys-minor-mode-map (kbd "C-q") 'delete-window-or-frame)

(define-minor-mode keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " keys" 'keys-minor-mode-map)

(keys-minor-mode 1)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'keys-minor-mode))
      (let ((mykeys (assq 'keys-minor-mode minor-mode-map-alist)))
	(assq-delete-all 'keys-minor-mode minor-mode-map-alist)
	(add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;; ELPA
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("9f443833deb3412a34d2d2c912247349d4bd1b09e0f5eaba11a3ea7872892000" "36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
