(setq inhibit-startup-message t) 
(menu-bar-mode 0)
;; (tool-bar-mode 0)
;; (scroll-bar-mode 0)
(column-number-mode 0)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(windmove-default-keybindings)
(wrap-region-mode t)

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
        rustic))

;; Iterate on packages and install missing ones
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))
;;;; packages>
;; (load-theme 'material t)

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
(require  'rustic)
;;(setq :mode ("\\.rs$" . rustic-mode))
;;(rustic-lsp-client 'eglot)
;;(rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
;;;; Rust>

(require 'auto-virtualenv)
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
;;;; Python>

(require 'doom-modeline)
(doom-modeline-mode 1)

;;;; <language server
(require 'eglot)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs
	     `(python-mode
	       . ,(eglot-alternatives '("pylsp"
					"jedi-language-server"
					("pyright-langserver" "--stdio")))))
;;;; language server>

;;;; <theme
(use-package  doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-monokai-pro t))
;;;; theme>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-mode elpy json-mode yaml-mode wrap-region paredit markdown-mode magit helm expand-region projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
