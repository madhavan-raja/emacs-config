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

(setq custom-file (expand-file-name ".custom" user-emacs-directory))

(use-package doom-themes
  :config
  (load-theme 'doom-dark+ t)
  (doom-themes-neotree-config))

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

(setq text-scale-mode-step 1.1)

(set-face-attribute 'default nil :family "Iosevka" :weight 'regular :height 120)
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :weight 'regular :height 1.0)
(set-face-attribute 'variable-pitch nil :font "Times New Roman" :height 120)

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
  :diminish evil-org-mode
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme))))

(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

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
    "ce" (lambda () (interactive) (find-file "~/.emacs.d/README.org"))
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

(use-package vertico
  :ensure t
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous))
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (maarginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

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

'(neo-dir-link-face ((t (:foreground "deep sky blue" :slant normal :weight bold :height 100 :family "Fira Code"))))
'(neo-file-link-face ((t (:foreground "White" :weight normal :height 120 :family "Fira Code"))))

(use-package magit
  :defer t)

(use-package smartparens
  :config (smartparens-global-mode)
  (show-smartparens-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(setq display-time-default-load-average nil)

(line-number-mode)
(column-number-mode)
(display-time-mode -1)
(size-indication-mode -1)

(use-package doom-modeline
  :init (doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-height 30
        doom-modeline-enable-word-count t
        doom-modeline-buffer-encoding nil
        doom-modeline-icon t
        doom-modeline-modal-icon nil
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-bar-width 3))

(custom-set-faces
 '(mode-line ((t (:family "Iosevka" :height 120)))))

(use-package hide-mode-line
  :hook
  (special-mode . hide-mode-line-mode)
  (term-mode . hide-mode-line-mode)
  (neotree-mode . hide-mode-line-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))

(use-package format-all
  :init (format-all-mode))

(use-package lsp-mode
  :init
  :hook (prog-mode . lsp-mode)
         ; (lua-mode . lsp)
         ; (python-mode . lsp)
         ; (sh-mode . lsp)
         ; (lisp-mode . lsp)
         ; (css-mode . lsp)
         ; (html-mode . lsp)
         ; (json-mode . lsp)
         ; (markdown-mode . lsp)
         ; (latex-mode . lsp)
         ; (go-mode . lsp)
         ; (text-mode . lsp)
         ; (org-mode . lsp-mode))
  :commands lsp
  :config
  (setq lsp-enable-symbol-highlighting nil
      lsp-enable-which-key-integration t
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

; (use-package lsp-grammarly)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-treemacs
  :after lsp)

(setq lsp-enable-symbol-highlighting nil)

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

; (use-package flycheck
;   :ensure t
;   :init (global-flycheck-mode))

(use-package projectile
  :config (projectile-mode 1))

(use-package org-download)

(add-hook 'org-mode-hook 'org-indent-mode)

; (add-hook 'org-mode-hook 'org-toggle-pretty-entities)

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
        '("•"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-ellipsis " ⤵ ")

(use-package toc-org)

(use-package svg-tag-mode
  :init
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                  nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ count total) nil
                                      :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag value nil
                             :stroke 0 :margin 0)) :ascent 'center)))

  (setq svg-tag-tags
    `(
      ;; Org tags

      (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
      (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

      ;; Task priority

      ("\\[#[A-Z]\\]" . ( (lambda (tag)
                            (svg-tag-make tag :face 'org-priority 
                                          :beg 2 :end -1 :margin 0))))

      ;; Progress

      ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                          (svg-progress-percent (substring tag 1 -2)))))
      ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                        (svg-progress-count (substring tag 1 -1)))))

      ;; TODO / DONE
      ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
      ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))
      ("IN PROGRESS" . ((lambda (tag) (svg-tag-make "IN PROGRESS" :face 'org-done :margin 0))))
      ("CANCELLED" . ((lambda (tag) (svg-tag-make "CANCELLED" :face 'org-done :margin 0))))

      ;; Citation of the form [cite:@Knuth:1984] 

      ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                        (svg-tag-make tag
                                                      :inverse t
                                                      :beg 7 :end -1
                                                      :crop-right t))))
      ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                              (svg-tag-make tag
                                                            :end -1
                                                            :crop-left t))))

      ;; Active date (without day name, with or without time)

      (,(format "\\(<%s>\\)" date-re) .
       ((lambda (tag)
          (svg-tag-make tag :beg 1 :end -1 :margin 0))))
      (,(format "\\(<%s *\\)%s>" date-re time-re) .
       ((lambda (tag)
          (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
      (,(format "<%s *\\(%s>\\)" date-re time-re) .
       ((lambda (tag)
          (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

      ;; Inactive date (without day name, with or without time)

       (,(format "\\(\\[%s\\]\\)" date-re) .
        ((lambda (tag)
           (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
       (,(format "\\(\\[%s *\\)%s\\]" date-re time-re) .
        ((lambda (tag)
           (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
       (,(format "\\[%s *\\(%s\\]\\)" date-re time-re) .
        ((lambda (tag)
           (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))
  :hook org-mode)

(setq org-todo-keywords
     '((sequence "TODO" "IN PROGRESS" "CANCELLED" "DONE")))

(global-set-key (kbd "S-<f9>") 'compile)
(global-set-key (kbd "<f9>") 'recompile)
