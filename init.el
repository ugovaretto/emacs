(setq inhibit-startup-message t)
(if (boundp 'menu-bar-mode) (menu-bar-mode 0) nil)
(if (boundp 'tool-bar-mode) (tool-bar-mode 0) nil)
(if (boundp 'scroll-bar-mode) (scroll-bar-mode 0) nil)
(setq-default indent-tabs-mode nil)
(column-number-mode 0)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(windmove-default-keybindings)
(setq column-number-mode t)

;;;;;;;;;; PACKAGES ;;;;;;;;;;
;; first, declare repositories
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))


;; Init the package facility
(require 'package)
(package-initialize)
;; (package-refresh-contents) ;; this line is commented
;; since refreshing packages is time-consuming and should be done on demand

;; Declare packages
(setq my-packages
      '(;;projectile
        expand-region
        helm
        magit
        markdown-mode
        paredit
        wrap-region
        yaml-mode
        json-mode
        elpy
        eglot
        material-theme
        flycheck
        py-autopep8
        blacken
        ein
        doom-modeline
        ;;doom-themes
        auto-virtualenv
        rustic
        company
        modern-cpp-font-lock
        clang-format
        selectrum
        selectrum-prescient
        rg
        mark-multiple
        zenburn-theme
        whitespace
        elixir-mode
        minions
        moody
        zig-mode))

;; Iterate on packages and install missing ones
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;;;;;;;;; MULTIPLE CURSORS ;;;;;;;;;;
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;;;;;;;;;; MARK MULTIPLE ;;;;;;;;;;
(use-package mark-multiple
  :ensure t
  :bind ("C-c q" . 'mark-next-like-this))

;;;;;;;;;; WRAP REGION ;;;;;;;;;;
(wrap-region-mode t)

;;;;;;;;;; EXPAND REGION ;;;;;;;;;;
(use-package expand-region
  :ensure t
  :bind ("C-q" . er/expand-region))

;;;;;;;;;; FUZZY FINDING ;;;;;;;;;;
(require 'rg)
(rg-enable-menu)

;;;;;;;;;; MINIBUFFER COMPLETION ;;;;;;;;;;
(use-package selectrum
  :ensure t
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :ensure t
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

;;;;;;;;;; PROJECT ;;;;;;;;;;
(require 'project)
(setq project-vc-extra-root-markers '(".project.el" ".projectile" ))

;; ;;;;;;;;;; PROJECTILE ;;;;;;;;;;
;; (require 'projectile)
;; (projectile-mode +1)
;; ;; Recommended keymap prefix on Windows/Linux
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; (setq projectile-completion-system 'auto)

;;;;;;;;;; LANGUAGE SERVER ;;;;;;;;;;
(require 'eglot)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs
             `(python-mode
               . ,(eglot-alternatives '("pylsp"
                                        "jedi-language-server"
                                        ("pyright-langserver" "--stdio")))))

;;;;;;;;;; CMAKE ;;;;;;;;;;
(use-package cmake-mode)


;;;;;;;;;; PYTHON ;;;;;;;;;;
(elpy-enable)
;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Use IPython for REPL
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")
(require 'auto-virtualenv)
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)

;; Enable autopep8
;;(require 'py-autopep8)
;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;;;;;;;;; RUST ;;;;;;;;;;
(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

;;;;;;;;;; MODELINE ;;;;;;;;;;
(require 'minions)
(keymap-global-set "C-." #'minions-minor-modes-menu)
(require 'doom-modeline)
(doom-modeline-mode 1)

;;;;;;;;;; ELIXIR ;;;;;;;;;;
(use-package elixir-mode
  :ensure t)
(defun install-elixir-ls ()
  (let ((cur-dir (pwd)) (path (concat user-emacs-directory "elixir-ls")))
    (shell-command
     (concat "git clone https://github.com/elixir-lsp/elixir-ls.git " path))
    (cd path)
    (shell-command "mix deps.get")
    (shell-command "mix elixir_ls.release2"))
    (cd cur-dir)
 )

(if (not (file-exists-p (concat user-emacs-directory "elixir-ls")))
    (install-elixir-ls) ())

(defconst elixir-ls-path (concat user-emacs-directory "elixir-ls/release/language_server.sh"))
(use-package
 eglot
 :ensure nil
 :config (add-to-list 'eglot-server-programs `(elixir-mode ,elixir-ls-path)))

(add-hook 'elixir-mode-hook 'eglot-ensure)

;;;;;;;;;; C++ ;;;;;;;;;;
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)
(require 'clang-format)
(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)
(setq clang-format-style "file")

;;;;;;;;;; ZIG ;;;;;;;;;;
(use-package
 eglot
 :ensure nil
 :config (add-to-list 'eglot-server-programs '(zig-mode "zls")))
(add-hook 'zig-mode-hook 'eglot-ensure)

;;;;;;;;;; AUTOCOMPLETION ;;;;;;;;;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;; THEME ;;;;;;;;;;
(load-theme 'zenburn t)

;;;;;;;;;; LINES > 80 COLUMNS ;;;;;;;;;;
(setq-default display-fill-column-indicator-column 79)
(add-hook 'c++-mode-hook
          (lambda () (display-fill-column-indicator-mode)))
(add-hook 'c++-mode-hook (lambda () (set-face-attribute
                                     'fill-column-indicator nil
                                     :background 'unspecified
                                     :foreground "slategray"
                                     :stipple '(7 1 " "))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zenburn-theme yaml-mode wrap-region which-key selectrum-prescient rustic rg rainbow-mode py-autopep8 projectile paredit multiple-cursors modern-cpp-font-lock material-theme mark-multiple magit lsp-ui lsp-treemacs json-mode helm-lsp expand-region elpy ein doom-themes doom-modeline cmake-mode clang-format ccls blacken auto-virtualenv)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
