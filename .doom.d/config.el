;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ben Schmidt"
      user-mail-address "benschmidt@benschmidt.tech")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(setq which-key-idle-delay 0.001)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/zettels/")

;; Org Fontify code in code blocks:
(setq org-src-fontify-natively t)
(setq org-return-follows-link t) ;; Use return on a link in an editable buffer will follow the link instead of inserting a new line.

;; Org-Roam
(setq org-roam-directory "~/zettels/")
(setq org-roam-buffer-width 0.2)
(setq org-roam-link-title-format "ƶ:%s")
;;(add-hook 'org-roam-backlinks-mode-hook (lambda () (flyspell-mode -1))) ; disable flyspell in org-roam-backlinks buffers


;;(use-package pdf-tools) ; Requires some install external.
;;;; See: https://github.com/politza/pdf-tools


;;;; Transclude files:
;; https://stackoverflow.com/questions/15328515/iso-transclusion-in-emacs-org-mode
;; auto-populate with C-c C-x C-u.
;; Skip the min and max args to include the entire file.
;; Note that you can bind org-update-all-dblocks to a hook, so that this range is updated whenever you visit the file or save.
(defun org-dblock-write:transclusion (params)
  (progn
    (with-temp-buffer
      (insert-file-contents (plist-get params :filename))
      (let ((range-start (or (plist-get params :min) (line-number-at-pos (point-min))))
            (range-end (or (plist-get params :max) (line-number-at-pos (point-max)))))
        (copy-region-as-kill (line-beginning-position range-start)
                             (line-end-position range-end))))
    (yank)))

;;Example:
;;#+BEGIN: transclusion :filename "~/testfile.org" :min 2 :max 4
;;#+END:

;; transcludePS does the same thing as the transclusion dynamic block above, but wraps the transcluded content in a powershell src block:
;; auto-populate with C-c C-x C-u.
(defun org-dblock-write:transcludePS (params)
  (progn
    (insert "#+begin_src powershell")
    (newline)
    (with-temp-buffer
      (insert-file-contents (plist-get params :filename))
      (let ((range-start (or (plist-get params :min) (line-number-at-pos (point-min))))
            (range-end (or (plist-get params :max) (line-number-at-pos (point-max)))))
        (copy-region-as-kill (line-beginning-position range-start)
                             (line-end-position range-end))))
    (yank)
    (newline)
    (insert "#+end_src")))


;; WSL2 Specific configuration:
(when (string-match "-[Mm]icrosoft" operating-system-release)
  ;; WSL: WSL1 has "-Microsoft", WSL2 has "-microsoft-standard"

  ;; Open Links using windows browser:
  ;; https://adam.kruszewski.name/2017/09/emacs-in-wsl-and-opening-links/
  ;; Another option here: https://www.reddit.com/r/bashonubuntuonwindows/comments/70i8aa/making_emacs_on_wsl_open_links_in_windows_web/
  (defun my--browse-url (url &optional _new-window)
    ;; new-window ignored
    "Opens link via pwsh.exe"
    (interactive (browse-url-interactive-arg "URL: "))
    (let ((quotedUrl (format "start '%s'" url)))
      (apply 'call-process "/mnt/c/Program Files/PowerShell/7/pwsh.exe" nil
	     0 nil
	     (list "-Command" quotedUrl))))
  ; (apply 'call-process "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe" nil
  (setq-default browse-url-browser-function 'my--browse-url)
)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.