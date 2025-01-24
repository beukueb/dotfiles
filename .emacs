;; cl warning
;; Remove when $ grep 'require' .emacs.d/elpa/*/*.el | grep 'cl)'
;; does not return any packages
(setq byte-compile-warnings '(cl-functions))

;; Package management
(require 'package)
(add-to-list 'package-archives
	     '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; M-x [package-]list-packages to browse and install packages

;;; use-package setup
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;;; non-use-package dependencies
(defvar local-packages '(
			 ; to put package configs in separate sections
			 auto-complete
			 ; python
			 epc
			 python-mode
			 ein ;iPython notebook
			 ; R
			 ess
			 ess-R-data-view
			 yaml-mode
			 ; go
			 go-mode
			 go-complete
			 go-autocomplete
			 ; autopair
			 ; contact management
			 bbdb
			 ; utilities
			 multiple-cursors
			 powerline
			 exec-path-from-shell ;for OS X
			 async
			 ; org
			 org-brain
			 polymode ; org-brain optional dependency
			 org-noter
			 org-tree-slide
			 pdf-tools
			 csv-mode
			 gptel
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


;; Operating system specific setup ;;

;; macOS mods
( when (memq system-type '(darwin))
  (defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to
match that used by the user's shell. Mac GUI started programs
do not have access to regular env variables."
  (interactive)
  (let ((path-from-shell (get-shell-output "$SHELL --login -i -c 'echo -n $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
  (setenv "PYTHONIOENCODING" "utf-8")
  (setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))
  
  (setq ns-right-alternate-modifier nil)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
)


;; Windows mods
( when (memq system-type '(cygwin windows-nt))
  (set-language-environment "utf-8")
  (setq-default visible-bell t)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq exec-path (append '("/usr/bin" "/usr/local/bin")
                          exec-path))
  (setenv "PATH" "/usr/local/bin:/usr/bin:/bin")
  ;(defun eshell-path-windows-hook ()
  ;  "Addpath on eshell mode creation"
  ;  (eshell/addpath "/bin/")
  ;  (eshell/addpath "-b" "/usr/bin/")
  ;  (eshell/addpath "-b" "/usr/local/bin/"))
  ;(add-hook 'eshell-mode-hook 'eshell-path-windows-hook)
  ;; https://www.emacswiki.org/emacs/AnsiColor
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on) ; to avoid strange codes
  (add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  ;;(require 'fakecygpty) ;; windows-nt only
  ;;(fakecygpty-activate)
  (setq shell-file-name "/bin/bash")
  (defun run-bash ()
      (interactive)
      (let ((shell-file-name "/bin/bash"))
        (shell "*bash*")))
  (defun run-cmdexe ()
      (interactive)
      (let ((shell-file-name "cmd.exe"))
        (shell "*cmd.exe*")))
  (defun run-powershell ()
  "Run powershell"
  (interactive)
  (async-shell-command "C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -Command -"
               nil
               nil))
)


;; Automatic Emacs customization
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wheatgrass))
 '(package-selected-packages
   '(docker-tramp ## org-tree-slide yasnippet yaml-mode use-package python-mode projectile powerline pdf-tools org-noter org-brain multiple-cursors jedi go-complete go-autocomplete exec-path-from-shell ess-R-data-view ein csv-mode bbdb async))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "wheat" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 139 :width normal :foundry "outline" :family "DejaVu Sans Mono")))))


;; General configuration ;;

;; Snippets
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'yaml-mode-hook 'yas-minor-mode))


;; Session management
(desktop-save-mode 1)
(setq desktop-restore-forces-onscreen nil)


;; Tramp for remote access
(setq tramp-default-method "ssh")


;; Latex
;;(load "auctex.el" nil t t) ;; WINTODO
;;(load "preview-latex.el" nil t t)

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
;;(require 'git)
;;(require 'git-blame)


;; Org
(add-to-list 'auto-mode-alist '("\\.HOW_TO\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time)
(setq org-directory "~/SynologyDrive/Apps/MobileOrg")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Org code evaluation
(setq org-confirm-babel-evaluate nil) ;without confirmation
(require 'ob-shell)                      ;required for sh
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (R . t)
   (lilypond . t)))
(setq org-babel-sh-command "bash")
;; (setq org-babel-python-command "ipython --no-banner --classic --no-confirm-exit")
(setq org-babel-python-command "python")
(setq org-src-window-setup 'current-window)
(require 'org-tempo) ;for <s tab expansions

;; Org beamer export
(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

;; Org opening links
(setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
				   (vm-imap . vm-visit-imap-folder-other-frame)
				   (gnus . org-gnus-no-new-news)
				   (file . find-file)
				   (wl . wl-other-frame)))
      )

;;; org-brain
;; org-brain configuration
(use-package org-brain :ensure t
  :init
  (setq org-brain-path "/cygdrive/c/Users/christophe/SynologyDrive/MindMap")
  (setq org-capture-templates
      '()) ; see https://orgmode.org/manual/Capture-templates.html
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil))

;; Allows you to edit entries directly from org-brain-visualize
(use-package polymode
  :config
  (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))

;; org-noter
;; hook so entries have id and are displayed in org-brain
(add-hook 'org-noter-insert-heading-hook #'org-id-get-create)
;; function to open noter from brain
(defun org-brain-open-org-noter (entry)
    "Open `org-noter' on the ENTRY.
If run interactively, get ENTRY from context."
    (interactive (list (org-brain-entry-at-pt)))
    (org-with-point-at (org-brain-entry-marker entry)
      (org-noter)))
;; Linking to C-c n (undefined in org-brain keymap)
(define-key org-brain-visualize-mode-map
  (kbd "\C-c n") #'org-brain-open-org-noter)


;; Projectile
(use-package projectile
  :config
  (projectile-global-mode))

;; Auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; Python/jedi config
(use-package python-mode
  :mode ("\\.ipy\\'" . python-mode))

(use-package jedi ;for first time use run: M-x jedi:install-server
  :config
  (add-to-list 'ac-sources 'ac-source-jedi-direct)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  
  (defvar jedi-config:use-system-python nil
    "Will use system python and active environment for Jedi server.
    May be necessary for some GUI environments (e.g., Mac OS X)
    virtualenv should be installed for the server-command python")
  
  ;(setq jedi:server-command
  ;      (list "/usr/local/bin/python3" jedi:server-script))
  
  ;(defvar jedi-config:with-virtualenv nil
  ;  "Set to non-nil to point to a particular virtualenv.")
  
  (defvar jedi-config:vcs-root-sentinel ".git")
  
  (defvar jedi-config:python-module-sentinel "__init__.py"))


(use-package ein
  :config
  (require 'request)
  (setq request-curl "/usr/bin/curl")
  (setq ein:output-area-inlined-images t))


;;; Custom cvn
(setq-default major-mode 'text-mode)

;; Backup directory
(setq backup-directory-alist `(("." . "~/.emacs.d/autosaves")))

;; Paste-replace selection
(delete-selection-mode 1)
(put 'dired-find-alternate-file 'disabled nil)

;; Switch frames
(global-set-key (kbd "M-s-<tab>") 'other-frame)
;; C-c <letter> and F5-F9 reserved for user
(global-set-key (kbd "C-/") 'completion-at-point)
(setq cvn "Christophe Van Neste")

;;; execute statement
(fset 'execute-line-in-other-frame-term
   [?\C-k ?\C-y ?\C-u ?\C-  ?\C-x kp-5 ?o down ?\C-c ?\C-j escape ?  ?\C-y return ?\C-c ?\C-k ?\C-c kp-5 ?o down])
(global-set-key (kbd "C-c e") 'execute-line-in-other-frame-term)
(global-set-key '[(M-f5)] 'execute-line-in-other-frame-term)
(fset 'execute-line-in-other-window-term
      [?\C-k ?\C-y ?\C-u ?\C-  ?\C-x ?o ?\C-c ?\C-j escape ?  ?\C-y return ?\C-c ?\C-k ?\C-c ?o down])
(global-set-key '[(f5)] 'execute-line-in-other-window-term)

;;; execute region
(fset 'execute-region-in-other-frame-term
      [?\M-w ?\C-x kp-5 ?o ?% ?p ?a ?s ?t ?e return ?\C-c kp-5 ?o])
(global-set-key '[(M-f6)] 'execute-region-in-other-frame-term)
(fset 'execute-region-in-other-window-term
      [?\M-w ?\C-x ?o ?% ?p ?a ?s ?t ?e return ?\C-c ?o])
(global-set-key '[(f6)] 'execute-region-in-other-window-term)

;;; copy region to /tmp/pythonpaste.ipy and run in console
(fset 'runpy [?\M-w ?\C-x ?\C-f ?\C-f ?/ ?t ?m ?p ?/ ?p ?y ?t ?h
   ?o ?n ?p ?a ?s ?t ?e ?. ?i ?p ?y return ?\C- ?\M-> backspace
   ?\C-y ?\C-x ?\C-s ?\C-x ?k return ?\C-x ?5 ?o ?% ?r ?u
   ?n ? ?- ?i ?  ?/ ?t ?m ?p ?/ ?p ?y ?t ?h ?o ?n ?p ?a ?s ?t ?e ?. ?i
   ?p ?y return ?\C-c ?5 ?o])
(global-set-key '[(f7)] 'runpy)

;;; execute org code block
(fset 'execute-code-block-in-other-frame-term
   [?\C-c ?\' ?\M-< ?\C-  ?\M-> ?\M-w ?\C-x kp-5 ?o ?% ?p ?a ?s ?t ?e return ?\C-c ?\C-j ?\C-x kp-5 ?o ?\M-< ?\C-c ?\'])
;;(global-set-key '[(f7)] 'execute-code-block-in-other-frame-term)
;; term char mode keybindings
(fset 'paste-in-char-term
      [?\C-e ?\C-c ?\C-j ?\s-v ?\C-c ?\C-k])
(add-hook 'term-load-hook
	  (lambda ()
	    (define-key term-raw-map (kbd "s-v") 'paste-in-char-term)))

;; csv-mode
(customize-set-variable 'csv-separators '("," ";"))

;; term functions
(defun term-copy ()
  "Copy in term char mode"
  (interactive)
  (term-line-mode)
  (kill-ring-save (region-beginning) (region-end)) 
  (term-char-mode)
  )
(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c M-w") 'term-copy))
(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

;; CVN keybindings
;; C-z is unset to liberate it for personal keybindings
;; the frame can still be suspended by double C-z
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z C-z") 'suspend-frame)

;; Docker (currently problematic on Windows)
;; (use-package docker
;;   :ensure t
;;   :bind ("C-c d" . docker))

;; store API key in ~/.authinfo[.gpg] line with format: 
;; machine ai.google.dev login apikey password ****
(setq
 gptel-model 'Gemini:gemini-1.5-flash
 gptel-backend (gptel-make-gemini "Gemini"
                 :key (plist-get (car (auth-source-search :host "ai.google.dev")) :secret)
                 :stream t))
(gptel-make-gpt4all "GPT4All"           ;Name of your choosing
 :protocol "http"
 :host "localhost:4891"                 ;Where it's running, enable in GPT4ALL settings
 :models '(llama-3.2-1b.instruct))      ;Available models
