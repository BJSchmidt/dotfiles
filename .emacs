(setq user-full-name "Ben Schmidt")
(setq user-mail-address "benschmidt@benschmidt.tech")

;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 120)

;; Make frame transparency overridable
(defvar efs/frame-transparency '(90 . 90))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;(setq inhibit-startup-message t)

;; Visual Settings
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode t)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode t)
(setq visible-bell t)       ; Enable Visible Bell
(column-number-mode)        ; Enable Column Numbers in the modeline

;; Line Numbers
(setq display-line-numbers-type 'visual) ; Display Relative Line Numbers
(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode t)       ; Enable Display Line Numbers globally
(dolist (mode '(term-mode-hook             ; Disable line numbers for some modes
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Change all prompts to y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Fonts
(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-variable-font-size :weight 'regular)

;;Theme
(use-package doom-themes
  :init (load-theme 'doom-one t))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.00001))    ; Which-key shows up faster with a very small decimal value like 0.00001 instead of 0 or 0.0.

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package undo-fu)
(setq evil-undo-system 'undo-fu)

;;;;Evil Mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  ;(setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
 
  (evil-global-set-key 'motion "z=" 'flyspell-correct-word)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq user-config-file "~/repos/dotfiles/emacs.org")

(defun open-user-config-file () (interactive) (find-file user-config-file))

;; Use general to define your own leader key & menu (a la spacemacs or doom emacs)
;; Note efs/leader-keys is added onto later with a hydra for text scaling.
(use-package general
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (efs/leader-keys
    "t"  '(:ignore t :which-key "Toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")

    "g"  '(:ignore t :which-key "Git")
    "gg" '(magit-status :which-key "Magit Status")

    "n" '(:ignore t :which-key "Notes")
    "nf" '(org-roam-find-file :which-key "Org-Roam Find File")
    "ni" '(org-roam-insert :which-key "Org-Roam Insert")
    "nr" '(org-roam-buffer-toggle-display :which-key "Org-Roam Buffer")

    "b" '(:ignore t :which-key "Buffers")
    "bb" '(ivy-switch-buffer :which-key "Switch Buffer")
    "bk" '(kill-this-buffer :which-key "Kill Buffer")
    "bd" '(kill-this-buffer :which-key "Kill Buffer")
    "bn" '(evil-new-buffer :which-key "New Buffer")

    "f" '(:ignore t :which-key "Files")
    "ff" '(find-file :which-key "Find Files")
    "fc" '(open-user-config-file :which-key "Config File")

    "w" '(:ignore t :which-key "Windows")
    "ww" '(evil-window-next :wk "Evil Window Next")
    "wW" '(evil-window-prev :wk "Evil Window Prev")
    "wh" '(evil-window-left :wk "Evil Window Left")
    "wj" '(evil-window-down :wk "Evil Window Down")
    "wh" '(evil-window-up :wk "Evil Window Up")
    "wk" '(evil-window-right :wk "Evil Window Right")
    "wo" '(other window :wk "Other Window")

    "wc" '(delete-window :wk "Delete Window")
    "wd" '(delete-window :wk "Delete Window")
    "wk" '(delete-window :wk "Delete Window")
    "w0" '(delete-other-windows :wk "Delete Other Windows")
    "ww" '(evil-window-next :wk "Evil Window Next")

    )
)
    ;; More ideas
    ;; Switch to repl buffer in powershell mode
;; (general-define-key :keymaps 'evil-insert-state-map
;;                     (general-chord "jk") 'evil-normal-state
;;                     (general-chord "kj") 'evil-normal-state)

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(efs/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;;; Org Mode:
(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-hide nil      :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :bind (("C-c l" . org-store-link)
         ("C-c C-l" . org-insert-link))
  :config
  ;(setq org-ellipsis " ▾")
  (setq org-ellipsis " ➤")
  ;(setq org-ellipsis " ➢")
  ;(setq org-ellipsis " ➣")
  ;(setq org-ellipsis " ᐅ")
  ;(setq org-ellipsis " ᐳ")
  ;(setq org-ellipsis " >")
 ; 
  ;(setq org-ellipsis " »")
  ;(setq org-ellipsis " ›")
  ;(setq org-ellipsis " ❯")
  ;(setq org-ellipsis " ❱")
  ;(setq org-ellipsis " ⇁")
  ;(setq org-ellipsis " ⇀")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (efs/org-font-setup))

;; Org Agenda & Clock:
(setq org-agenda-files (directory-files-recursively "~/zettels/" "^[^.#]+.org$"))
(setq org-agenda-skip-deadline-prewarning-if-scheduled t )
(setq org-log-note-clock-out t) ;; Prompt for a note when clocking out.

;; Org Fontify code in code blocks:
(setq org-src-fontify-natively t)
(setq org-return-follows-link t) ;; Use return on a link in an editable buffer will follow the link instead of inserting a new line.

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom (org-bullets-bullet-list '("\u200b")))
  ; Some other org bullet candidates:
  ; org-bullets-bullet-list '("●" "◉" "○")
    ;; Set Bullets to a zero width space:
    ; (setq org-bullets-bullet-list '("\u200b"))
    ; https://zhangda.wordpress.com/2016/02/15/configurations-for-beautifying-emacs-org-mode/

(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      ; :straight (:host github :repo "jethrokuan/org-roam" :branch "develop")
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
(add-hook 'org-roam-backlinks-mode-hook (lambda () (flyspell-mode -1))) ; disable flyspell in org-roam-backlinks buffers

(use-package org-noter)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)))


(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("ps" . "src powershell"))

;; Automatically tangle our Emacs.org config file when we save it
  ;;(defun efs/org-babel-tangle-config ()
    ;;(when (string-equal (file-name-directory (buffer-file-name))
                        ;;(expand-file-name user-emacs-directory))
      ;;;; Dynamic scoping to the rescue
      ;;(let ((org-confirm-babel-evaluate nil))
        ;;(org-babel-tangle))))
;;
  ;;(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(setq org-export-with-section-numbers nil)
(setq org-export-preserve-breaks t)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package powershell)
; for LSP Mode, run M-x lsp-install-server pwsh-ls

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (global-set-key (kbd "C-x g") 'magit-status))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package deft
  :after org
  :bind ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

;enable flyspell for all text mode buffers:
(add-hook 'text-mode-hook 'flyspell-mode)

;; WSL2 Specific configuration:
(when (string-match "-[Mm]icrosoft" operating-system-release)
  ;; WSL: WSL1 has "-Microsoft", WSL2 has "-microsoft-standard"
  
  ;; Open Links using windows browser:
  ;; https://adam.kruszewski.name/2017/09/emacs-in-wsl-and-opening-links/
  ;; Another option here: https://www.reddit.com/r/bashonubuntuonwindows/comments/70i8aa/making_emacs_on_wsl_open_links_in_windows_web/
  (defun my--browse-url (url &optional _new-window)
    ;; new-window ignored
    "Opens link via powershell.exe"
    (interactive (browse-url-interactive-arg "URL: "))
    (let ((quotedUrl (format "start '%s'" url)))
      (apply 'call-process "/mnt/c/Program Files/PowerShell/7/pwsh.exe" nil
	     0 nil
	     (list "-Command" quotedUrl))))
  ; (apply 'call-process "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe" nil
  (setq-default browse-url-browser-function 'my--browse-url)
)
