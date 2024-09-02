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

(defalias 'yes-or-no-p 'y-or-n-p)						 
(setq initial-major-mode 'fundamental-mode)
(setq auto-revert-interval 1)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)
(savehist-mode)
;(windowmove-default-keybindings 'control)
(setq sentence-end-double-space nil)
(when (display-graphic-p)
  (context-menu-mode))

(add-hook 'after-init-hook 'help-quick)
(setq enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setq completion-cycle-threshold 1)                  ; TAB cycles candidates
(setq completions-detailed t)                        ; Show annotations
(setq tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setq completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setq completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setq completions-max-height 20)                     ; This is arbitrary
(setq completions-detailed t)
(setq completions-format 'one-column)
(setq completions-group t)
(setq completion-auto-select 'second-tab)
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)
(setq line-number-mode t)                        ; Show current line in modeline
(setq column-number-mode t)                      ; Show column as well

(setq x-underline-at-descent-line nil)           ; Prettier underlines
(setq switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setq-default show-trailing-whitespace t)
(pixel-scroll-precision-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode)

(use-package json-mode)
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

(use-package vterm)

(use-package smartparens
  :bind (("<localleader>(" . sp-wrap-round)
	 ("<localleader>{" . sp-wrap-curly)
	 ("<localleader>[" . sp-wrap-square)
	 ("<localleader>DEL" . sp-splice-sexp-killing-backward)))

(setq save-interprogram-paste-before-kill t)
(use-package org
   :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
         (org-mode . flyspell-mode))    ; spell checking!
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;  (use-package os-csl)
  (add-to-list 'org-export-backends 'md)
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)
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
  (setq org-tag-alist '(
                      ;; locale
                      (:startgroup)
                      ("home" . ?h)
                      ("work" . ?w)
                      ("school" . ?s)
                      (:endgroup)
                      (:newline)
                      ;; scale
                      (:startgroup)
                      ("one-shot" . ?o)
                      ("project" . ?j)sw
                      ("tiny" . ?t)
                      (:endgroup)
                      ;; misc
                      ("meta")
                      ("review")
                      ("reading")))
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

(defun today-org (directory)
  "Create an .org file in the specified DIRECTORY named with the current date in ISO format."
  (interactive "DDirectory: ")
  (let* ((current-date (format-time-string "%Y-%m-%d"))
         (filename (concat current-date ".org"))
         (filepath (expand-file-name filename directory)))
    (if (file-exists-p filepath)
        (message "File already exists: %s" filepath)
      (write-region "" nil filepath)
      (find-file filepath)
      (message "Created file: %s" filepath))))

(defun clear-kill-ring ()
  "Clear the kill ring."
  (interactive)
  (setq kill-ring nil)
  (message "Kill ring cleared."))

(defun reload-emacs-conf ()
  "Reload the emacs config"
  (interactive)
  (load-file "~/.emacs.d_exwm/init.el")
  (message "Reloaded emacs config"))
  
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

;;Taken from https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

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
            (let ((user (tramp-file-name-user tstruct))
                  (host (tramp-file-name-host tstruct))
                  (localname (tramp-file-name-localname tstruct)))
              (process-send-string bufname (format
                                            (concat  "ssh -t %s@%s '"
                                                     cd-str
                                                     "exec bash'; exec bash; clear\n")
                                            user host localname))))
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
   '(os-csl smartparens elgot json-mode yaml-mode fancy-battery symon vterm git-gutter-fringe org-roam use-package-ensure-system-package latex-preview-pane lsp-mode rust-mode cargo-mode magit ace-jump-mode base16-theme nyan-mode ##)))

;; More annoying than useful
;;(find-file "~/org/ideas.org")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:strike-through t)))))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)
