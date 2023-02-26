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
(setq display-line-numbers 'relative)
;;and turn off for some modes
(dolist (mode '(;;org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;turn of wrapping
(setq truncate-lines t)
;;turn on auto revert
(auto-revert-mode)
;;keep my folders clean
(setq make-backup-files nil)

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
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))

;;(use-package ivy-rich)
;;(ivy-rich-mode 1)

(use-package all-the-icons-ivy-rich
  :ensure t
  :config (all-the-icons-ivy-rich-mode 1))

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

   "q" '(:ignore q :which-key "quit")
   "qq" '(save-buffers-kill-terminal :which-key "quit emacs")

   "d" '(:ignore d :which-key "dired")
   "dd" '(dired :which-key "open dired")

   "o" '(:ignore o :which-key "org")
   "oc" '(org-capture :which-key "capture a thought")
   "oa" '(org-agenda :which-key "agenda")
   "ot" '(org-todo :which-key "toggle todo item")
   "X" '(org-capture :which-key "capture a thought")

   "m" '(:ignore m :which-key "mail")
   "mc" '(mu4e-compose-new :which-key "new email")

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
  (define-key evil-normal-state-map (kbd "/") 'swiper)
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
          org-log-done 'note
          org-capture-templates
          '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
             "\n* TODO %?\n %i\n %a")
            ("d" "Todo with time" entry (file+olp+datetree "~/org/todo.org" "Scheduled Tasks")
             "\n* TODO %?\n %i\n SCHEDULED: %^t\n%a")
            ("n" "Notes")
            ("nn" "Note" entry (file+headline "~/org/notes.org" "Notes")
             "\n* %?\n %i\n")
            ("nc" "Note for CMD and PS" entry (file+headline "~/org/notes.org" "Useful ~CMD~ and ~PS~ commands")
             "\n* %?\n %i\n")
            ("nr" "Note for Registry hacks" entry (file+headline "~/org/notes.org" "Registry hacks/tricks")
             "\n* %?\n %i\n")
            ("i" "Idea" entry (file+headline "~/org/notes.org" "Ideas")
             "\n* IDEA %?\n %i\n %a")
            ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
             "\n* %?\nEntered on %U\n %i\n %a"))
          org-ellipsis " ↓"
          org-hide-emphasis-markers t
          org-startup-indented t)
          (add-to-list 'org-modules 'org-tempo t)
          (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
          (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
          (add-to-list 'org-structure-template-alist '("py" . "src python"))
          (add-to-list 'org-structure-template-alist '("ps" . "src powershell"))
          (org-babel-do-load-languages
           'org-babel-load-languages '((python . t)
                                       (shell . t)
                                       (haskell .t))))

(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-minted-options '(("breaklines" "true")
                               ("breakanywhere" "true")))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '(">" ">" ">" ">")))

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

(use-package sudo-edit
  :ensure t)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(use-package mu4e
  :ensure nil
  :config
  (setq mu4e-change-filenames-when-moving t)

  (setq mu4e-update-interval (* 5 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/mail")

  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "somenull"
          :vars '((user-mail-address . "lane@somenull.com")
                  (user-full-name . "Lane Wright")
                  (mu4e-compose-signature . "- Lane\nWannabe H@x0rM@n")
                  (smtpmail-smtp-server . "somenull.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type . ssl)
                  (mu4e-drafts-folder . "/drafts")
                  (mu4e-sent-folder . "/sent")
                  (mu4e-trash-folder . "/trash"))))))

(setq message-send-mail-function 'smtpmail-send-it)

(use-package org-mime
  :ensure t)

(setq org-mime-export-options '(:section-numbers nil
                                :with-author nil
                                :with-toc nil))

(add-hook 'message-send-hook 'org-mime-htmlize)
