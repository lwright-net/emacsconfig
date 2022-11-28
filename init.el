(setq inhibit-startup-message t)

;;turn off some extra crap
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;;some space please
(set-fringe-mode 10)

;;instead of a ding
(setq visible-bell t)

;;pretty please
;;(load-theme 'doom-one)
;;set later after doom themes are installed
(set-face-attribute 'default nil :font "Ubuntu Mono" :height 110)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 110)

;;make escape quit a prompt
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;set line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;;and turn off for some modes
(dolist (mode '(;;org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;get some packages up in here
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
;;update if out of date
(unless package-archive-contents
  (package-refresh-contents))

;;setup use-package package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; setup ivy
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

(use-package ivy-rich)
(ivy-rich-mode 1)

;;setup doom-modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;;setup doom-themes
(use-package doom-themes
  :ensure t
  :init
(load-theme 'doom-vibrant t))

;;setup all-the-icons
(use-package all-the-icons
  :ensure t)

;;setup rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;setup which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;;setup counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))
(global-set-key (kbd "M-b") 'counsel-switch-buffer)

;;setup helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;setup general (defines key binds)
(use-package general
  :config
  (general-create-definer lw/leader-keys
     :keymaps '(normal insert visual emacs)
     :prefix "SPC"
     :global-prefix "C-SPC")
  (lw/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")

   "d" '(:ignore d :which-key "dired")
   "dd" '(dired :which-key "open dired")

   "o" '(:ignore o :which-key "org")
   "oc" '(org-capture :which-key "capture a thought")
   "oa" '(org-agenda :which-key "agenda")
   "X" '(org-capture :which-key "capture a thought")

   "g" '(:ignore g :which-key "git")
   "gg" '(magit-status :which-key "git status")

   "." '(counsel-find-file :which-key "open file")

   "w" '(:ignore w :which-key "window")
   "ws" '(split-window-below :which-key "split window")
   "wv" '(split-window-right :which-key "vertical split window")
   "wc" '(delete-window :which-key "close window")
   "wk" '(windmove-up :which-key "move window above")
   "wj" '(windmove-down :which-key "move window below")
   "wh" '(windmove-left :which-key "move window left")
   "wl" '(windmove-right :which-key "move window right")

   "b" '(:ignore b :which-key "buffer")
   "b k" '(kill-current-buffer :which-key "kill buffer")
   "b i" '(ibuffer :which-key "interactive buffer")
   "b B" '(counsel-switch-buffer :which-key "switch buffer")))


;;be EVIL
(defun lw/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :hook (evil-mode . lw/evil-hook)
  :config
  ;;(evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
(evil-mode 1)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package magit)

(defun lw/org-mode-setup ()
    (org-indent-mode))

(use-package org
  :hook (org-mode . lw/org-mode-setup)
  :config
  (setq org-directory '("~/org")
        org-agenda-files '("~/org/todo.org" "~/org/notes.org" "~/org/journal.org")
        org-default-notes-file '("~/org/notes.org")
        org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
           "* TODO %?\n %i\n %a")
          ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
           "* NOTE %?\n %i\n %a")
          ("i" "Idea" entry (file+headline "~/org/notes.org" "Ideas")
           "* IDEA %?\n %i\n %a")
          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n %i\n %a")))
        org-ellipsis " ↓"
        org-hide-emphasis-markers t
        org-startup-indented t
        (add-to-list 'org-modules 'org-tempo t)
        (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
        (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
        (add-to-list 'org-structure-template-alist '("py" . "src python"))
        (org-babel-do-load-languages
         'org-babel-load-languages '((python . t))))


(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("→" "↘" "↝" "⇒")))

(with-eval-after-load 'org-faces
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.05)))
    (set-face-attribute (car face) nil :font "Pink Chicken Bold" :weight 'bold :height (cdr face))))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file)

(use-package all-the-icons-dired)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "~/.emacs.d/emacs-logo-green.png"))
