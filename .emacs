(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wheatgrass)))
 '(delete-selection-mode t)
 '(inhibit-startup-screen t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/LabFBT/notes/nanopore.org" "~/Dropbox/Apps/MobileOrg/notes.org")))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.ugent.be")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 157 :width normal)))))

;; Package management
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; M-x [package-]list-packages to browse and install packages

;;; CVN package dependencies
(defvar local-packages '(
			 projectile
			 auto-complete
			 epc
			 jedi
			 python-mode
			 ein ;iPython notebook
			 bbdb
			 multiple-cursors
			 powerline
			 ))

;;; Automatically install dependencies
(defun uninstalled-packages (packages)
  (delq nil
	(mapcar (lambda (p) (if (package-installed-p p nil) nil p)) packages)))

(let ((need-to-install (uninstalled-packages local-packages)))
  (when need-to-install
    (progn
      (package-refresh-contents)
      (dolist (p need-to-install)
	(package-install p)))))

;; Session management
(desktop-save-mode 1)

;; Tramp for remote access
(setq tramp-default-method "ssh")

;; Latex
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; (add-hook 'latex-mode-hook 'visual-line-mode)
(global-visual-line-mode 1)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Spelling
(setq ispell-dictionary "english")

;; Ido
(require 'ido)
(ido-mode t)

;; Powerline (requires package-initialize)
(require 'powerline)
(powerline-default-theme)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Git
;;(add-to-list 'load-path ".../git/contrib/emacs")
(require 'git)
;;(require 'git-blame)

;; Org
(add-to-list 'auto-mode-alist '("\\.HOW_TO\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time)
(setq org-directory "~/Dropbox/Apps/MobileOrg")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Org code evaluation
(setq org-confirm-babel-evaluate nil) ;without confirmation
(require 'ob-sh)                      ;required for sh
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (sh . t)))
(setq org-babel-sh-command "bash")

;; Projectile
(require 'projectile)
(projectile-global-mode)

;; Auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; Jedi config
(defvar jedi-config:use-system-python nil
  "Will use system python and active environment for Jedi server.
May be necessary for some GUI environments (e.g., Mac OS X)")

(defvar jedi-config:with-virtualenv nil
  "Set to non-nil to point to a particular virtualenv.")

(defvar jedi-config:vcs-root-sentinel ".git")

(defvar jedi-config:python-module-sentinel "__init__.py")

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell (get-shell-output "$SHELL --login -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(require 'jedi) ;for first time use run: M-x jedi:install-server
(add-to-list 'ac-sources 'ac-source-jedi-direct)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Custom cvn
;; C-c <letter> and F5-F9 reserved for user
(global-set-key (kbd "C-/") 'completion-at-point)
(setq cvn "Christophe Van Neste")
;;; execute statement
(fset 'execute-line-in-other-frame-term
   [?\C-k ?\C-y ?\C-u ?\C-  ?\C-x kp-5 ?o ?\C-c ?\C-j escape ?  ?\C-y return ?\C-x kp-5 ?o down])
(global-set-key (kbd "C-c e") 'execute-line-in-other-frame-term)
(global-set-key '[(f5)] 'execute-line-in-other-frame-term)
;;; execute region
(fset 'execute-region-in-other-frame-term
      [?\M-w ?\C-x kp-5 ?o ?% ?p ?a ?s ?t ?e return ?\C-c ?\C-j ?\C-x kp-5 ?o])
(global-set-key '[(f6)] 'execute-region-in-other-frame-term)
;;; execute org code block
(fset 'execute-code-block-in-other-frame-term
   [?\C-c ?\' ?\M-< ?\C-  ?\M-> ?\M-w ?\C-x kp-5 ?o ?% ?p ?a ?s ?t ?e return ?\C-c ?\C-j ?\C-x kp-5 ?o ?\M-< ?\C-c ?\'])
(global-set-key '[(f7)] 'execute-code-block-in-other-frame-term)
