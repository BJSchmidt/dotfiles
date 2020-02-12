;; MELPA:
;; Customized to try using HTTPS and failback to HTTP - From Melpa.org install instructions:
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Packages:
(require 'which-key)
(setq which-key-idle-delay 0.001)
(which-key-mode)
;; https://github.com/sabof/org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Change all prompts to y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Fontify code in code blocks
(setq org-src-fontify-natively t)

;; Key Bindings:
(global-set-key (kbd "C-x g") 'magit-status)

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

;; Org Mode Settings:
(setq org-agenda-files (directory-files-recursively "~/org/" "^[^.#]+.org$"))
(setq org-agenda-skip-deadline-prewarning-if-scheduled t )
(setq org-log-note-clock-out t) ;; Prompt for a note when clocking out.


(load-theme 'vscode-dark-plus t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#282a36" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(custom-enabled-themes (quote (vscode-dark-plus-GrayBackground)))
 '(custom-safe-themes
   (quote
    ("7e88785f415a689c11610ec8e940d3e994eb25a9973474f7894fd5ae234ce18a" "15da14568df794266cd571db282a5776fc07065c661224225d309b389a26731a" "3082bb04a78ecf18f20f8220fb67aba8a02b43c289f48d72e530250e55e335a2" "ca849ae0c889eb918785cdc75452b1e11a00848a5128a95a23872e0119ccc8f4" default)))
 '(fci-rule-color "#6272a4")
 '(jdee-db-active-breakpoint-face-colors (cons "#1E2029" "#bd93f9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1E2029" "#50fa7b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1E2029" "#565761"))
 '(menu-bar-mode nil)
 '(objed-cursor-color "#ff5555")
 '(org-agenda-files
   (quote
    ("/home/higman/org/personal/Microsoft/Windows_Server.org" "/home/higman/org/personal/Programming/Programming_Ideas.org" "/home/higman/org/personal/emacs/OrgMode_Notes.org" "/home/higman/org/personal/emacs/Spacemacs_Notes.org" "/home/higman/org/personal/BlogIdeas.org" "/home/higman/org/personal/Bookmarks.org" "/home/higman/org/personal/DevOps.org" "/home/higman/org/personal/Entertainment.org" "/home/higman/org/personal/Inbox.org" "/home/higman/org/personal/Networking.org" "/home/higman/org/personal/Personal.org" "/home/higman/org/personal/Pottery.org" "/home/higman/org/personal/Powershell.org" "/home/higman/org/personal/Professional.org" "/home/higman/org/personal/Shopping.org" "/home/higman/org/personal/misc.org" "/home/higman/org/personal/recipes.org" "/home/higman/org/personal/unraid.org" "/home/higman/org/ticketmaster/ticketmaster.org" "/home/higman/org/Bookmarks.org")))
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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(org-level-1 ((t (:inherit outline-1 :box nil :height 1.35))))
 '(org-level-2 ((t (:inherit outline-2 :box nil :height 1.25))))
 '(org-level-3 ((t (:inherit outline-3 :box nil :height 1.15))))
 '(org-level-4 ((t (:inherit outline-4 :box nil :height 1.1)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
