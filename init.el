(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(load-theme 'base16-gruvbox-dark-soft t)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;; GUI deshittening
(blink-cursor-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(global-hl-line-mode 1)
(nyan-mode 1)

(use-package package
  :ensure nil
  :config
  (package-initialize)
  :custom
  (package-native-compile t)
  (package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
		      ("melpa" . "https://melpa.org/packages/"))))

(unless package-archive-contents
  (package-refresh-contents))

(use-package org
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-todo-keywords
	'((sequence "TODO" "PROG" "WAIT" "|"  "DONE" "CNCL" "VOID")))
  (setq org-todo-keyword-faces
      '(("TODO" . "red")
	("PROG" . "magenta")
	("WAIT" . "orange")
	("DONE" . "green")
	("CNCL" . "olive drab")
	("VOID" . "dim gray")))
  (setq org-image-actual-width nil)
  (custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
   '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:strike-through t)))))
  )

(use-package base16-theme)

(use-package ace-jump-mode
  :bind ("C-<tab>" . ace-jump-mode)
  )

(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/org-roam"))
  (org-roam-db-autosync-mode)
  )

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02)
  )

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  )

(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save nil)
  :hook (
	 (rust-mode-hook . cargo-minor-mode)
	 (prog-mode-hook . display-line-numbers-mode))
  :custom
  (rustic-cargo-use-last-stored-arguments t))

(global-unset-key (kbd "C-z"))
(setq visible-bell t)
(setq make-backup-files nil)
(define-key global-map (kbd "C-x C-o") (kbd "C-x o"))  ;; more convenient to type; switches to other file when editing side by side
(define-key global-map (kbd "C-o") (kbd "C-e RET"))  ;; starts a new line without having to go to the end of the current one, same as vim O
(define-key global-map (kbd "C-S-o") (kbd "C-p C-e RET"))  ;; same thing but new line is before the current one
(define-key global-map (kbd "M-j") (kbd "C-u M-^"))  ;; append next line to current one, the default does the opposite

(latex-preview-pane-enable)

;; taken from: dfeichs emacs course and config
(defvar autosave-dir
  (concat "~/.emacs-autosaves/"))
(make-directory autosave-dir t)
(setq auto-save-filename-transforms `((".*" ,autosave-dir t)))

(defvar backup-dir "~/.emacs-doc-backups/")
(setq backup-directory-alist (list (cons ".*" backup-dir)))

;; custom commands
(defun extend-selection (&optional firstp)
  (interactive)
  (if firstp
      (progn
        (beginning-of-line)
        (set-mark-command nil)
        (next-line (or n 1))
        (exchange-point-and-mark))
    (progn
      (save-excursion
        (exchange-point-and-mark)
        (next-line)
        (exchange-point-and-mark)))
    (message "(Type l to extend selection)")
    (set-transient-map
     '(keymap (?l . extend-selection)))))

(defun mark-lines (&optional n)
  (interactive "p")
  (extend-selection t)
  (when (called-interactively-p)
   (set-transient-map
    '(keymap (?l . extend-selection)))))

(define-key global-map (kbd "C-x l") 'mark-lines)

(defun swap-buffers-with-next-window ()
  (interactive)
  (let* ((a (current-buffer))
         (b (window-buffer (next-window))))
    (switch-to-buffer b nil t)
    (save-selected-window
      (other-window 1)
      (switch-to-buffer a nil t))))

(define-key global-map (kbd "C-x 7") 'swap-buffers-with-next-window)



;; taken from https://gist.github.com/dfeich/1bcb09b0e2fc5f55fecec8f498862683
(defun ssh-term (&optional path name)
  "Opens an ansi terminal at PATH. If no PATH is given, it uses
the value of `default-directory'. PATH may be a tramp remote path.
The ansi-term buffer is named based on `name' "
  (interactive)
  (unless path (setq path default-directory))
  (unless name (setq name "ansi-term"))
  (ansi-term "/bin/bash" name)
  (let ((path (replace-regexp-in-string "^file:" "" path))
	(cd-str
	 "fn=%s; if test ! -d $fn; then fn=$(dirname $fn); fi; cd $fn;")
	(bufname (concat "*" name "*" )))
    (if (tramp-tramp-file-p path)
	(let ((tstruct (tramp-dissect-file-name path)))
	  (cond
	   ((equal (tramp-file-name-method tstruct) "ssh")
	    (process-send-string bufname (format
					  (concat  "ssh -t %s '"
						   cd-str
						   "exec bash'; exec bash; clear\n")
					  (tramp-file-name-host tstruct)
					  (tramp-file-name-localname tstruct))))
	   (t (error "not implemented for method %s"
		     (tramp-file-name-method tstruct)))))
      (process-send-string bufname (format (concat cd-str " exec bash;clear\n")
					   path)))))


(setq-default explicit-shell-file-name "/bin/fish")

;; Org tuff

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(git-gutter-fringe org-roam use-package-ensure-system-package latex-preview-pane lsp-mode rust-mode cargo-mode magit ace-jump-mode base16-theme nyan-mode ##)))

;; More annoying than useful
;;(find-file "~/org/ideas.org")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:strike-through t)))))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)
