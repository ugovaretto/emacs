(setq lexical-binding t)
(setq inhibit-startup-message t)
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0) nil)
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0) nil)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0) nil)
(setq-default indent-tabs-mode nil)
(column-number-mode 0)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(windmove-default-keybindings)
(setq column-number-mode t)
(add-hook 'prog-mode-hook #'electric-pair-mode)
;; possibly helping in not getting sly stuck
(setq-default comint-process-echoes 'off)
;;;;;;;;;; PACKAGES ;;;;;;;;;;
;; first, declare repositories
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
       ;; ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/" )))


;; Init the package facility
(require 'package)
(package-initialize)


;; (package-refresh-contents)

;; (package-refresh-contents) ;; this line is commented
;; since refreshing packages is time-consuming and should be done on demand

;; Declare packages
(setq my-packages
      '(;;projectile
        ;;doom-themes
        ;;ac-slime
        ;;slime
        auto-virtualenv
        blacken
        clang-format
        company
        consult
        dired-preview
        doom-modeline
        eglot
        ein
        elixir-mode
        elpy
        embark
        embark-consult
        expand-region
        flycheck
        helm
        json-mode
        ligature
        magit
        marginalia
        mark-multiple
        markdown-mode
        material-theme
        minions
        modern-cpp-font-lock
        moody
        paredit
        py-autopep8
        rainbow-delimiters
        recentf
        rg
        rustic
        selectrum
        selectrum-prescient
        sly
        swift-mode
        vterm
        which-key
        whitespace
        wrap-region
        yaml-mode
        yasnippet
        zenburn-theme
        zig-mode))

;; Iterate on packages and install missing ones
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-c\C-r" 'recentf-open-files)

;;;;;;;;;; VTERM ;;;;;;;;;;
(use-package vterm
  :ensure t)

;;;;;;;;;; LIGATURES ;;;;;;;;;;
(use-package ligature
  :load-path "path-to-ligature-repo"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                          '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                            ;; =:= =!=
                            ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                            ;; ;; ;;;
                            (";" (rx (+ ";")))
                            ;; && &&&
                            ("&" (rx (+ "&")))
                            ;; !! !!! !. !: !!. != !== !~
                            ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                            ;; ?? ??? ?:  ?=  ?.
                            ("?" (rx (or ":" "=" "\." (+ "?"))))
                            ;; %% %%%
                            ("%" (rx (+ "%")))
                            ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                            ;; |->>-||-<<-| |- |== ||=||
                            ;; |==>>==<<==<=>==//==/=!==:===>
                            ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                            "-" "=" ))))
                            ;; \\ \\\ \/
                            ("\\" (rx (or "/" (+ "\\"))))
                            ;; ++ +++ ++++ +>
                            ("+" (rx (or ">" (+ "+"))))
                            ;; :: ::: :::: :> :< := :// ::=
                            (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                            ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                            "="))))
                            ;; .. ... .... .= .- .? ..= ..<
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                            ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                            ;; *> */ *)  ** *** ****
                            ("*" (rx (or ">" "/" ")" (+ "*"))))
                            ;; www wwww
                            ("w" (rx (+ "w")))
                            ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                            ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                            ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                            ;; << <<< <<<<
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                            ;; >> >>> >>>>
                            (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                            ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                            ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                         (+ "#"))))
                            ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ;; __ ___ ____ _|_ __|____|_
                            ("_" (rx (+ (or "_" "|"))))
                            ;; Fira code: 0xFF 0x12
                            ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                            ;; Fira code:
                            "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                            ;; The few not covered by the regexps.
                            "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;;;;;;;;; DIRED ;;;;;;;;;;
;; make the other dired window the default
;; target for operations after splitting with C-o
(setq dired-dwim-target t)

;;;;;;;;;; Customize Rainbow Delimiters. ;;;;;;;;;;
(require 'rainbow-delimiters)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
(set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray
(set-face-bold 'rainbow-delimiters-depth-1-face t)
(set-face-bold 'rainbow-delimiters-depth-2-face t)
(set-face-bold 'rainbow-delimiters-depth-3-face t)
(set-face-bold 'rainbow-delimiters-depth-4-face t)
(set-face-bold 'rainbow-delimiters-depth-5-face t)
(set-face-bold 'rainbow-delimiters-depth-6-face t)
(set-face-bold 'rainbow-delimiters-depth-7-face t)
(set-face-bold 'rainbow-delimiters-depth-8-face t)
(set-face-bold 'rainbow-delimiters-depth-9-face t)

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

;;;;;;;;;; RAINBOW DELIMITERS ;;;;;;;;;;
(use-package rainbow-delimiters
    :ensure t
    :hook ((prog-mode . rainbow-delimiters-mode)
           (lisp-mode . rainbow-delimiters-mode)
           (sly-mode . rainbow-delimiters-mode)
           ;;(slime-repl-mode . rainbow-delimiters-mode)
           ))

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
;;;;;;;;;; SNIPPETS ;;;;;;;;;;
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs `(,(concat user-emacs-directory "snippets")))
  (yas-global-mode 1))

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-S-e") 'yas-expand)

;;;;;;;;;; CMAKE ;;;;;;;;;;
(use-package cmake-mode
  :ensure t)


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
 :ensure t
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
 :ensure t
 :config (add-to-list 'eglot-server-programs '(zig-mode "zls")))
(add-hook 'zig-mode-hook 'eglot-ensure)

;;;;;;;;;; SWIFT ;;;;;;;;;;
(use-package
 eglot
 :ensure t
 :config (add-to-list 'eglot-server-programs '(swift-mode "sourcekit-lsp")))
(add-hook 'swift-mode-hook 'eglot-ensure)


;;;;;;;;;; AUTOCOMPLETION ;;;;;;;;;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;; THEME ;;;;;;;;;;
;; (load-theme 'zenburn t)
(load-theme 'misterioso t)

;;;;;;;;;; LISP ;;;;;;;;;;
;; ;; Follow instructions here:
;; ;; https://github.com/rabbibotton/clog?tab=readme-ov-file
;; ;; SLIME
;; (use-package slime
;;   :ensure t
;;   :config '(slime-fancy slime-quicklisp slime-asdf slime-mrepl slime-company)
;;   :hook ((slime-repl-mode . auto-complete-mode)))

;; (use-package slime-company
;;   :after (slime company)
;;   :config (setq slime-company-completion 'fuzzy
;;                 slime-company-after-completion 'slime-company-just-one-space))
;; ;; (require 'ac-slime)
;; ;; (add-hook 'slime-mode-hook 'set-up-slime-ac)
;; ;; (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;; ;; (eval-after-load "auto-complete"
;; ;;   '(add-to-list 'ac-modes 'slime-repl-mode))
;; ;; (setq ac-auto-show-menu 0.3)

;;SLY
(use-package sly
  :ensure t)
(setq sly-complete-symbol-function 'sly-flex-completions)

;; Allegro CL MacOS
;; (setq inferior-lisp-program "/Applications/AllegroCL64express.app/Contents/Resources/alisp")

;; SBCL
(setq inferior-lisp-program "sbcl")

;; ECL
;;(setq inferior-lisp-program "ecl")

;;;;;;;;;; LINES > 80 COLUMNS ;;;;;;;;;;
(setq-default display-fill-column-indicator-column 79)
(add-hook 'c++-mode-hook
          (lambda () (display-fill-column-indicator-mode)))
(add-hook 'c++-mode-hook (lambda () (set-face-attribute
                                     'fill-column-indicator nil
                                     :background 'unspecified
                                     :foreground "slategray"
                                     :stipple '(7 1 " "))))


;;;;;;;;;; COMMAND/KEYS COMPLETION ;;;;;;;;;;
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ellama ligature vterm zenburn-theme yaml-mode wrap-region which-key selectrum-prescient rustic rg rainbow-mode py-autopep8 projectile paredit multiple-cursors modern-cpp-font-lock material-theme mark-multiple magit lsp-ui lsp-treemacs json-mode helm-lsp expand-region elpy ein doom-themes doom-modeline cmake-mode clang-format ccls blacken auto-virtualenv)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
