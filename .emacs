;;;; Ben Schmidt's .emacs config
;; 12/4/2019

;;;; MELPA:
;; Customized to try using HTTPS and failback to HTTP - From Melpa.org install instructions:
;; (require 'package)
;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                     (not (gnutls-available-p))))
;;        (proto (if no-ssl "http" "https")))
;;   (when no-ssl
;;     (warn "\
;; Your version of Emacs does not support SSL connections,
;; which is unsafe because it allows man-in-the-middle attacks.
;; There are two things you can do about this warning:
;; 1. Install an Emacs version that does support SSL and be safe.
;; 2. Remove this warning from your init file so you won't see it again."))
;;   ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
;;   (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;   ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;;   (when (< emacs-major-version 24)
;;     ;; For important compatibility libraries like cl-lib
;;     (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
;; ;; Added by Package.el.  This must come before configurations of
;; ;; installed packages.  Don't delete this line.  If you don't want it,
;; ;; just comment it out by adding a semicolon to the start of the line.
;; ;; You may delete these explanatory comments.
;; (package-initialize)

;;;; straight.el: next-generation, purely functional package manager for the Emacs hacker.
;; replaces the Melpa config & package.el (use-package package) and (package-initialize) above.
;; https://github.com/raxod502/straight.el
;; straight.el bootstrap code:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; Set use-package to install missing packages using straight.el:
(setq straight-use-package-by-default t)

;;;; Install use-package with Straight:
(straight-use-package 'use-package)
(require 'use-package)
;; Now that straight and use-package are loaded we can install & load packages like:
;; (use-package foo)
;; See: https://github.com/jwiegley/use-package#package-installation
;; We can set keybindings & variables inside the package declaration like this:
;; (use-package ace-jump-mode
;;   :bind ("C-." . ace-jump-mode)
;;   :init (setq var-foo t))
;; Because the variable var-foo is set in the :init block, it is set before the package loads.

;;;; Packages:
(use-package which-key
  :init (setq which-key-idle-delay 0.001))
(which-key-mode 1)

;;; Org Mode:
;; Org Agenda & Clock:
(setq org-agenda-files (directory-files-recursively "~/org/" "^[^.#]+.org$"))
(setq org-agenda-skip-deadline-prewarning-if-scheduled t )
(setq org-log-note-clock-out t) ;; Prompt for a note when clocking out.
;; Org Fontify code in code blocks:
(setq org-src-fontify-natively t)
(setq org-return-follows-link t) ;; Use return on a link in an editable buffer will follow the link instead of inserting a new line.

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))
;; https://github.com/sabof/org-bullets
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; (setq org-bullets-bullet-list '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸")))
;; (setq org-bullets-bullet-list '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸")))

(use-package org-roam
      :after org
      :hook 
      (after-init . org-roam-mode)
      :straight (:host github :repo "jethrokuan/org-roam" :branch "develop")
      :custom ((org-roam-directory "~/zettels/"))
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph)
	       ("C-c n t" . org-roam-today))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))
(setq org-roam-buffer-width 0.2)
(setq org-roam-link-title-format "ƶ:%s")

(use-package neotree)

(use-package magit
  :custom
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package deft
  :after org
  :bind ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(use-package helm
  :straight (:host github :repo "emacs-helm/helm" :branch "master")
  :bind (("M-x" . helm-M-x)
	 ("C-x r b" . helm-filtered-bookmarks)
	 ("C-x C-f" . helm-find-files)
	 )
  )
;;(global-set-key (kbd "M-x") #'helm-M-x)
;;(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;;(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)
;;(use-package helm-projectile)

(use-package powershell)

(use-package pdf-tools)

;;;; Configuration:
;; Themes:
(use-package vscode-dark-plus-theme
  :straight (:host github :repo "ianpan870102/vscode-dark-plus-emacs-theme")
  :custom
  (add-to-list 'custom-theme-load-path "~/.emacs.d/straight/repos/vscode-dark-plus-emacs-theme/")
  (load-theme 'vscode-dark-plus t))

;; Change all prompts to y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Open Links using windows browser:
;; https://adam.kruszewski.name/2017/09/emacs-in-wsl-and-opening-links/
;; Another option here: https://www.reddit.com/r/bashonubuntuonwindows/comments/70i8aa/making_emacs_on_wsl_open_links_in_windows_web/
(defun my--browse-url (url &optional _new-window)
;; new-window ignored
"Opens link via powershell.exe"
(interactive (browse-url-interactive-arg "URL: "))
(let ((quotedUrl (format "start '%s'" url)))
(apply 'call-process "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe" nil
0 nil
(list "-Command" quotedUrl))))
(setq-default browse-url-browser-function 'my--browse-url)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(add-to-list (quote custom-theme-load-path) t)
 '(ansi-color-names-vector
   ["#282a36" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(custom-enabled-themes (quote (vscode-dark-plus-GrayBackground)))
 '(custom-safe-themes
   (quote
    ("55cf8d1507ce65a9c84cafd25c901faa94f6cc958e7dd5b345ab6efedfeed0e2" "7e88785f415a689c11610ec8e940d3e994eb25a9973474f7894fd5ae234ce18a" "15da14568df794266cd571db282a5776fc07065c661224225d309b389a26731a" "3082bb04a78ecf18f20f8220fb67aba8a02b43c289f48d72e530250e55e335a2" "ca849ae0c889eb918785cdc75452b1e11a00848a5128a95a23872e0119ccc8f4" default)))
 '(deft-default-extension "org" t)
 '(deft-directory "~/zettels/")
 '(deft-recursive t)
 '(deft-use-filter-string-for-filename t)
 '(fci-rule-color "#6272a4")
 '(global-set-key "g" t)
 '(helm-completion-style (quote emacs))
 '(jdee-db-active-breakpoint-face-colors (cons "#1E2029" "#bd93f9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1E2029" "#50fa7b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1E2029" "#565761"))
 '(load-theme (quote vscode-dark-plus) t)
 '(menu-bar-mode nil)
 '(objed-cursor-color "#ff5555")
 '(org-roam-directory "~/zettels/")
 '(org-roam-mode t nil (org-roam))
 '(package-selected-packages
   (quote
    (undo-tree doom-themes powershell magit org-bullets which-key)))
 '(pdf-view-midnight-colors (cons "#f8f8f2" "#282a36"))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-full-name "Ben Schmidt")
 '(user-mail-address "benschmidt@benschmidt.tech")
 '(vc-annotate-background "#282a36")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50fa7b")
    (cons 40 "#85fa80")
    (cons 60 "#bbf986")
    (cons 80 "#f1fa8c")
    (cons 100 "#f5e381")
    (cons 120 "#face76")
    (cons 140 "#ffb86c")
    (cons 160 "#ffa38a")
    (cons 180 "#ff8ea8")
    (cons 200 "#ff79c6")
    (cons 220 "#ff6da0")
    (cons 240 "#ff617a")
    (cons 260 "#ff5555")
    (cons 280 "#d45558")
    (cons 300 "#aa565a")
    (cons 320 "#80565d")
    (cons 340 "#6272a4")
    (cons 360 "#6272a4")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(org-level-1 ((t (:inherit outline-1 :box nil :height 1.35))))
 '(org-level-2 ((t (:inherit outline-2 :box nil :height 1.25))))
 '(org-level-3 ((t (:inherit outline-3 :box nil :height 1.15))))
 '(org-level-4 ((t (:inherit outline-4 :box nil :height 1.1)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
