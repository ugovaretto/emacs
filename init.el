(setq inhibit-startup-message t) 
(menu-bar-mode 0)
;; (tool-bar-mode 0)
;; (scroll-bar-mode 0)
(column-number-mode 0)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(windmove-default-keybindings)
(wrap-region-mode t)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;;; <packages
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
      '(projectile
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
        doom-themes
        auto-virtualenv
        rustic
        company
        modern-cpp-font-lock
        clang-format
        selectrum
        selectrum-prescient
	rg
	mark-multiple))

;; Iterate on packages and install missing ones
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))
;;;; packages>
;; (load-theme 'material t)

;;;; <multiple cursors
;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(use-package mark-multiple
  :ensure t
  :bind ("C-c q" . 'mark-next-like-this))
;;;; multiple cursors>


;;;; <expand region
(use-package expand-region
  :ensure t
  :bind ("C-q" . er/expand-region))
;;;; expand region>

;;;; <fuzzy finding
(require 'rg)
;; (rg-enable-default-bindings)
(rg-enable-menu)
;;;; fuzzy finding>

;;;; <minibuffer completion
;; (use-package ido
;;   :config
;;   (setq ido-enable-flex-matching t)
;;   (ido-mode 1))
;;;; minibuffer completion>
(use-package selectrum
  :ensure t
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :ensure t
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))
;;;; <projectile
(require 'projectile)
(projectile-mode +1)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'auto)
;;;; projectile>


;;;; <language server
(require 'eglot)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs
	     `(python-mode
	       . ,(eglot-alternatives '("pylsp"
					"jedi-language-server"
					("pyright-langserver" "--stdio"))))
       '((c++-mode c-mode) "clangd"))
;;;; language server>

;;;; <cmake
(use-package cmake-mode)
;;;; cmake>

;;;; <Python 
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

;; Enable autopep8
;;(require 'py-autopep8)
;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;;; <Rust
;;; (require  'rustic)
;;(setq :mode ("\\.rs$" . rustic-mode))
;;(rustic-lsp-client 'eglot)
;;(rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t))
;;;; Rust>

(require 'auto-virtualenv)
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
;;;; Python>

(require 'doom-modeline)
(doom-modeline-mode 1)


;;;; <c++
;; (require 'eglot)
;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)
;; (add-to-list 'eglot-server-programs
;;     '((c-mode c++-mode)
;;           . ("clangd"
;;                 "-j=8"
;;                 "--log=error"
;;                 "--malloc-trim"
;;                 "--background-index"
;;                 "--clang-tidy"
;;                 "--cross-file-rename"
;;                 "--completion-style=detailed"
;;                 "--pch-storage=memory"
;;                 "--header-insertion=never"
;;                 "--header-insertion-decorators=0")))
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
;;;; c++>

;;;; <autocompletion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;;;; autocompletion>

;;;; <theme
(use-package  doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-monokai-ristretto t))
;;;; theme>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-mode mark-multiple cmake-mode lsp-mode elpy json-mode yaml-mode wrap-region paredit markdown-mode magit helm expand-region projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
