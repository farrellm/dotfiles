(defun delete-from-list (lst obj)
  "delete obj from a lst"
  (eval `(setq ,lst (delete obj (eval lst)))))


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

  ;; parens
  (global-rainbow-delimiters-mode)

  ;; auto-complete
  (require 'auto-complete)
  (require 'auto-complete-config)
  (ac-config-default)

  ;; golden-ratio
  ;; (require 'golden-ratio)
  (golden-ratio-enable)

  ;; color theme
  ;; (load-theme 'solarized-dark)
  (load-theme 'solarized-light)
  )

(add-hook 'after-init-hook 'after-packages)
	  

;; basic UI stuff
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)

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

;; Evil
(setq evil-visual-state-cursor 'hollow)
(setq evil-emacs-state-cursor "#dc322f")

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

;;; minor mode for key overrides
(defvar keys-minor-mode-map (make-keymap) "keys-minor-mode keymap.")

(define-key keys-minor-mode-map (kbd "C-j") 'other-window)
(define-key keys-minor-mode-map (kbd "C-k") 'prev-window)
(define-key keys-minor-mode-map (kbd "C--") 'split-window-below)
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
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
