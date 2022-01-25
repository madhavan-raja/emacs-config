(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)
(setq straight-use-package-by-default t)

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
(setq package-enable-at-startup nil)

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

(setq frame-title-format "%b - Emacs")
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq ring-bell-function 'ignore)

(setq-default
  delete-by-moving-to-trash t ;; Move to trash instead of deleting
  require-final-newline t ;; Auto create a newline at end of file
  custom-safe-themes t ;; Don't ask if theme is safe
  warning-minimum-level :emergency ;; Emacs, honestly, I want you to shut up
  disabled-command-function nil ;; Yes I want to use that command
  vc-follow-symlinks) ;; Follow those damn symlinks!

(defalias 'yes-or-no-p 'y-or-n-p) ;; Y or N instead of yes or no

(setq scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  smooth-scroll-margin 1)

(setq make-backup-files nil)

(setq ring-bell-function 'ignore)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package emacs
  :custom
  (redisplay-dont-pause            t) ;; Fully redraw the display before it processes queued input events.
  (next-screen-context-lines       2) ;; Number of lines of continuity to retain when scrolling by full screens
  (scroll-conservatively       10000) ;; only 'jump' when moving this far off the screen
  (scroll-step                     1) ;; Keyboard scroll one line at a time
  (mouse-wheel-progressive-speed nil) ;; Don't accelerate scrolling
  (mouse-wheel-follow-mouse        t) ;; Scroll window under mouse
  (fast-but-imprecise-scrolling    t) ;; No (less) lag while scrolling lots.
  (auto-window-vscroll           nil)) ;; Cursor move faster

(use-package good-scroll
  :hook (after-init . good-scroll-mode))

(setq custom-file (expand-file-name ".custom" user-emacs-directory))

(use-package doom-themes
  :config
  (load-theme 'doom-dark+ t)
  (doom-themes-neotree-config))

(custom-set-faces `(default ((t (:background "#0E0E0E")))))

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-splash-screen nil
  inhibit-startup-echo-area-message t
  inhibit-startup-message t)

(defun org/prettify-set ()
  (interactive)
  (setq prettify-symbols-alist
      '(("#+begin_example" . "")
        ("#+BEGIN_EXAMPLE" . "")
        ("#+end_example" . "")
        ("#+END_EXAMPLE" . "")
        ("#+results:" . "")
        ("#+RESULTS:" . "")
        ("#+begin_quote" . "❝")
        ("#+BEGIN_QUOTE" . "❝")
        ("#+end_quote" . "❞")
        ("#+END_QUOTE" . "❞")
        ("[ ]" . "☐")
        ("[-]" . "◯")
        ("[X]" . "☑"))))
(add-hook 'org-mode-hook 'org/prettify-set)

(defun prog/prettify-set ()
  (interactive)
  (setq prettify-symbols-alist
      '(("lambda" . "λ")
        ("->" . "→")
        ("<-" . "←")
        ("<=" . "≤")
        (">=" . "≥")
        ("!=" . "≠")
        ("~=" . "≃")
        ("=~" . "≃"))))
(add-hook 'prog-mode-hook 'prog/prettify-set)

(global-prettify-symbols-mode)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(dolist (mode '(org-mode-hook
  term-mode-hook
  eshell-mode-hook
  neotree-mode-hook
  elfeed-show-mode-hook
  circe-channel-mode-hook
  circe-chat-mode-hook
  doc-view-mode-hook
  xwidget-webkit-mode-hook
  woman-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

(save-place-mode) ;; Save location
(global-visual-line-mode) ;; Wrap lines
(global-auto-revert-mode) ;; Revert buffers
; (recentf-mode) ;; Recent files
; (add-hook 'org-mode-hook 'flyspell-mode) ;; Spell checker

(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font-12"))
(set-fontset-font t 'symbol "Twitter Color Emoji")

(setq-default indent-tabs-mode nil
    tab-width 2)
(setq indent-line-function 'insert-tab)

(use-package all-the-icons)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    ;; General
    ".f" 'consult-isearch
    ".q" 'delete-frame
    ".e" 'eval-region
    ".s" 'straight-use-package
    ;; Configs
    "ce" 'edit-emacs-configuration
    ;; Undo
    "uv" 'undo-tree-visualize
    "uu" 'undo-tree-undo
    "ur" 'undo-tree-redo
    "uc" 'consult-yank-pop
    ;; Words
    "wt" 'mw-thesaurus-lookup-dwim
    "wd" 'dictionary-lookup-definition
    "we" 'emoji-insert
    ;; Files
    "fr" 'consult-recent-file
    "fb" 'consult-bookmark
    "ff" 'find-file
    ;; Bufffers
    "bv" 'split-window-right
    "bh" 'split-window-below
    "bd" 'kill-current-buffer
    "bb" 'consult-buffer
    "bx" 'switch-to-scratch
    ;; Projectile
    "pa" 'projectile-add-known-project
    "pf" 'consult-projectile
    "pp" 'projectile-switch-project
    "pg" 'projectile-grep
    "pm" 'projectile-commander
    "pc" 'projectile-compile-project
    ;; Org Mode
    "oc" 'org-edit-special
    "ol" 'org-latex-preview
    "ot" 'org-ctrl-c-ctrl-c
    "oi" 'org-toggle-inline-images
    "oa" 'org-agenda
    "os" 'org-schedule
    ; Export
    "oep" 'org-latex-export-to-pdf
    "oeh" 'org-html-export-to-html
    "oem" 'org-man-export-to-man
    "oeu" 'org-publish-project
    ; Roam
    "orf" 'org-roam-node-find
    "ori" 'org-roam-node-insert
    "oru" 'org-roam-db-sync
    "oro" 'orui-open
    ; Babel
    "obs" 'org-babel-execute-src-block
    "obb" 'org-babel-execute-buffer
    "obl" 'org-babel-load-file
    ;; Help
    "hh" 'help
    "hk" 'describe-key
    "hv" 'describe-variable
    "hf" 'describe-function
    "hs" 'describe-symbol
    "hm" 'describe-mode
    ;; Magit
    "gi" 'magit-init
    "gc" 'magit-commit
    "gp" 'magit-push
    "gC" 'magit-clone
    "gs" 'magit-status))

(define-key evil-normal-state-map (kbd "M-s") 'save-buffer)
(define-key evil-normal-state-map (kbd "M-q") 'delete-window)
(define-key evil-normal-state-map (kbd "M-w") 'kill-current-buffer)

(define-key evil-normal-state-map (kbd "<C-tab>") 'consult-buffer)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "M-j") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "M-k") 'evil-scroll-up)

(define-key evil-normal-state-map "u" 'undo-tree-undo)
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)

(define-key evil-normal-state-map (kbd "M-t") 'neotree-toggle)
(define-key evil-normal-state-map (kbd "M-m") 'minimap-mode)
(define-key evil-normal-state-map (kbd "<C-return>") 'shr-browse-url)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(define-key evil-normal-state-map (kbd "C-=") 'text-scale-increase)
(define-key evil-normal-state-map (kbd "C--") 'text-scale-decrease)
(define-key evil-normal-state-map (kbd "C-0") 'text-scale-adjust)

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(defun my/c-c ()
  (interactive)
  (setq unread-command-events (listify-key-sequence (kbd "C-c"))))

(defun my/c-k ()
  (interactive)
  (setq unread-command-events (listify-key-sequence (kbd "C-k"))))

(evil-define-key 'normal global-map (kbd ",c") 'my/c-c)
(evil-define-key 'normal global-map (kbd ",x") 'my/c-k)

(set-default 'evil-normal-state-cursor 'box)
(set-default 'evil-insert-state-cursor 'bar)
(set-default 'evil-visual-state-cursor 'hbar)
(set-default 'evil-motion-state-cursor 'box)
(set-default 'evil-replace-state-cursor 'box)
(set-default 'evil-operator-state-cursor 'hbar)
(set-cursor-color "#B37AAE")
(setq-default cursor-type 'bar)

(setq evil-cross-lines t
      evil-move-beyond-eol t
      evil-symbol-word-search t
      evil-want-Y-yank-to-eol t
      evil-cross-lines t)

(use-package which-key
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.1))

(use-package consult)

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

(use-package vertico
  :init (vertico-mode 1)
  :config
  (setq vertico-resize nil
        vertico-count 15
        vertico-cycle t))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))

(use-package vertico-posframe
  :init (vertico-posframe-mode))

(setq vertico-posframe-parameters
     '((left-fringe . 5)
       (right-fringe . 5)))

(set-face-attribute 'vertico-posframe-border nil :background "#0E0E0E")

(setq vertico-posframe-border-width 3
    vertico-posframe-width 50
    vertico-posframe-height 15
    vertico-posframe-font "Iosevka Nerd Font-14")

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (special-mode . centaur-tabs-local-mode))
(setq centaur-tabs-height 32
  centaur-tabs-gray-out-icons 'buffer
  centaur-tabs-set-modified-marker t
  centaur-tabs-set-icons t)

(use-package neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(add-hook 'neotree-mode-hook
         (lambda ()
           (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
           (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
           (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
           (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
           (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
           (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
           (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))
(setq neo-window-fixed-size nil)

'(neo-dir-link-face ((t (:foreground "deep sky blue" :slant normal :weight bold :height 120 :family "Fira Code"))))
'(neo-file-link-face ((t (:foreground "White" :weight normal :height 120 :family "Fira Code"))))

(use-package magit
  :defer t)

(use-package smartparens
  :config (smartparens-global-mode)
  (show-smartparens-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-height 30
    doom-modeline-bar-width 3
    doom-modeline-buffer-encoding 'nondefault
    doom-modeline-major-mode-icon t
    doom-modeline-icon t))

(doom-modeline-def-modeline 'main
    '(bar modals buffer-info-simple remote-host " " major-mode workspace-name)
    '(buffer-position matches process checker lsp debug vcs))

(custom-set-faces
 '(mode-line ((t (:family "Iosevka Nerd Font" :height 120)))))

(use-package hide-mode-line
  :hook
  (special-mode . hide-mode-line-mode)
  (term-mode . hide-mode-line-mode)
  (neotree-mode . hide-mode-line-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode))
  (setq undo-tree-auto-save-history t)
  ; (setq undo-tree-history-directory-alist '(("." . "./undo")))

(use-package format-all
  :init (format-all-mode))

(use-package lsp-mode
  :init
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  ;        (lua-mode . lsp)
  ;        ;;(python-mode . lsp)
  ;        (sh-mode . lsp)
  ;        (lisp-mode . lsp)
  ;        (css-mode . lsp)
  ;        (html-mode . lsp)
  ;        (json-mode . lsp)
  ;        (markdown-mode . lsp)
  ;        (latex-mode . lsp)
  ;        (go-mode . lsp)
  ;        (text-mode . lsp)
  ;        (org-mode . lsp)
  :commands lsp
  :config
  (setq lsp-enable-symbol-highlighting nil
      lsp-ui-doc-enable t
      lsp-lens-enable nil
      lsp-headerline-breadcrumb-enable nil
      lsp-ui-sideline-enable nil
      lsp-ui-sideline-enable t
      lsp-modeline-code-actions-enable t
      lsp-ui-sideline-enable t
      lsp-ui-doc-border nil
      lsp-eldoc-enable-hover t
      lsp-log-io nil
      lsp-enable-file-watchers nil))

(use-package lsp-grammarly)

(use-package lsp-ui :commands lsp-ui-mode)

(setq lsp-enable-symbol-highlighting nil)

(use-package go-mode)
(use-package json-mode)
(use-package lua-mode)
(use-package nix-mode)
;;(use-package lsp-jedi
;;  :hook (python-mode . lsp-jedi)) ;; Doesn't work atm

(use-package lsp-ivy)

(use-package company
  :config (global-company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-select-next)))

(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

(setq-default company-backends '(company-capf
                                 company-yasnippet
                                 company-keywords
                                 compny-files
                                 company-ispell))

(setq company-idle-delay 0.1
      company-minimum-prefix-length 1
      company-selection-wrap-around t
      company-require-match 'never
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case t
      company-dabbrev-other-buffers nil
      company-tooltip-limit 5
      company-tooltip-minimum-width 50)

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-scrollbar nil))


; (use-package yasnippet
;   :hook (prog-mode . yas-global-mode))

; (use-package yasnippet-snippets
;   :defer t)

; (use-package flycheck
;   :ensure t
;   :init (global-flycheck-mode))

(use-package projectile
  :config (projectile-mode 1))

(use-package dictionary)

(use-package mw-thesaurus)

(use-package org-download)

(add-hook 'org-mode-hook 'org-indent-mode)

(add-hook 'org-mode-hook 'org-toggle-pretty-entities)

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-face-name "Inconsolata-8")
  (setq org-bullets-bullet-list
        '("◉" "○" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-export-backends '(latex md html man))

(require 'org)
(require 'ox-latex)
(require 'ox-man)
(require 'ox-publish)

(setq org-publish-use-timestamps-flag nil
    org-export-with-toc nil
    org-export-with-broken-links t)

(setq org-publish-project-alist
      '(
        ("Blog"
         :base-directory "~/Blog/"
         :base-extension "org" "png" "jpg" "css"
         :publishing-directory "~/Blog/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-preamble t
         )
        ("Garden"
         :base-directory "~/Org/"
         :base-extension "org" "css" "png"
         :publishing-directory "~/Garden/"
         :recursive nil
         :publishing-function org-html-publish-to-html
         :auto-preamble t
         )
      ))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted) 

(use-package htmlize)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-src-fontify-natively t)

(setq org-export-with-section-numbers nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (shell . t)
   (latex . t)))
(setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"./style.css\"/>"
  org-html-doctype "html5")

(setq org-ellipsis " ⤵")

(setq org-agenda-files '("~/RoamNotes"))

(setq org-todo-keywords
     '((sequence "TODO" "WAITING" "PAUSED" "ALMOST" "OPTIONAL" "IMPORTANT" "DONE")))
(setq org-todo-keyword-faces
  '(("TODO"      . (:foreground "#FF8080" :weight bold))
    ("WAITING"   . (:foreground "#FFFE80" :weight bold))
    ("PAUSED"    . (:foreground "#D5D5D5" :weight bold))
    ("ALMOST"    . (:foreground "#80D1FF" :weight bold))
    ("OPTIONAL"  . (:foreground "#C780FF" :weight bold))
    ("IMPORTANT" . (:foreground "#80FFE4" :weight bold))
    ("DONE"      . (:foreground "#97D59B" :weight bold))))

(setq org-roam-v2-ack t)

(use-package org-roam
  :custom
  (org-roam-directory "~/RoamNotes/")
  :config
  (org-roam-setup))

; (use-package org-roam-ui
;   :straight
;   (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;   :after org-roam
;   :hook (after-init . org-roam-ui-mode)
;   :config
;   (setq org-roam-ui-sync-theme t
;         org-roam-ui-follow t
;         org-roam-ui-update-on-save t
;         org-roam-ui-open-on-start 'nil))

(defun edit-emacs-configuration ()
  "Open the init file."
  (interactive)
  (find-file "~/.emacs.d/README.org"))

(use-package org-present)
