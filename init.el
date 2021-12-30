(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq frame-title-format "%b - Emacs")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-message t)

(setq make-backup-files nil)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq
  split-width-threshold 0
  split-height-threshold nil)

(use-package smooth-scrolling)
(smooth-scrolling-mode nil)
(setq smooth-scroll-margin 5)

(use-package emacs
  :custom
  (redisplay-dont-pause            t) ;; Fully redraw the display before it processes queued input events.
  (next-screen-context-lines       2) ;; Number of lines of continuity to retain when scrolling by full screens
  (scroll-conservatively       10000) ;; only 'jump' when moving this far off the screen
  (scroll-step                     1) ;; Keyboard scroll one line at a time
  (mouse-wheel-progressive-speed nil) ;; Don't accelerate scrolling
  (mouse-wheel-follow-mouse        t) ;; Scroll window under mouse
  (fast-but-imprecise-scrolling    t) ;; No (less) lag while scrolling lots.
  (auto-window-vscroll           nil) ;; Cursor move faster
  )

(use-package good-scroll
  :hook (after-init . good-scroll-mode))

(defun aborn/backward-kill-word ()
  "Customize/Smart backward-kill-word."
  (interactive)
  (let* ((cp (point))
         (backword)
         (end)
         (space-pos)
         (backword-char (if (bobp)
                            ""
                          (buffer-substring cp (- cp 1)))))
    (if (equal (length backword-char) (string-width backword-char))
        (progn
          (save-excursion
            (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
          (setq ab/debug backword)
          (save-excursion
            (when (and backword
                       (s-contains? " " backword))
              (setq space-pos (ignore-errors (search-backward " ")))))
          (save-excursion
            (let* ((pos (ignore-errors (search-backward-regexp "\n")))
                   (substr (when pos (buffer-substring pos cp))))
              (when (or (and substr (s-blank? (s-trim substr)))
                        (s-contains? "\n" backword))
                (setq end pos))))
          (if end
              (kill-region cp end)
            (if space-pos
                (kill-region cp space-pos)
              (backward-kill-word 1))))
      (kill-region cp (- cp 1)))))

(global-set-key  [C-backspace]
		 'aborn/backward-kill-word)

(defun edit-configuration ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c c") 'edit-configuration)

(setq custom-file (expand-file-name ".custom" user-emacs-directory))

(use-package doom-themes
  :config (load-theme 'doom-dark+ t))

(custom-set-faces `(default ((t (:background "#0E0E0E")))))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(scroll-bar-mode -1)

(tool-bar-mode -1)

(tooltip-mode -1)

(menu-bar-mode -1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(add-hook 'prog-mode-hook 'electric-pair-mode)

(add-hook 'prog-mode-hook 'show-paren-mode)
(setq show-paren-delay 0)

(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#f23")

(defun set-font-faces ()
  (set-face-attribute 'default nil :font "Fira Code-11")
  (set-face-attribute 'fringe nil :background nil))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
                (with-selected-frame frame
                  (set-font-faces))))
    (set-font-faces))

(use-package all-the-icons)

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
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g")
    'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h")
    'evil-delete-backward-char-and-join))

(evil-global-set-key 'motion "j" 'evil-next-line)
(evil-global-set-key 'motion "k" 'evil-previous-line)
(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(add-hook 'org-mode-hook 'org-indent-mode)

(add-hook 'org-mode-hook 'org-toggle-pretty-entities)

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-face-name "Inconsolata-12")
  (setq org-bullets-bullet-list
        '("◉" "○" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-src-fontify-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (latex . t)))

(setq org-ellipsis " ⤵")

(setq org-agenda-files '("~/RoamNotes"))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/RoamNotes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package magit)

(use-package treemacs
  :init
  (global-set-key (kbd "C-c C-n") 'treemacs))
