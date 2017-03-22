;;; packages.el --- wamaral layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <wamaral@wamaral>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `wamaral-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `wamaral/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `wamaral/pre-init-PACKAGE' and/or
;;   `wamaral/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst wamaral-packages
  '(4clojure
    clojure-cheatsheet
    helm-cider
    slamhound
    dired+
    ;; dumb-jump
    tern-auto-complete
    editorconfig
    evil-smartparens
    projectile-direnv
    ;; groovy-mode
    highlight-chars
    ;; lispy
    ;; evil-lispy
    (parinfer-mode :location (recipe
                              :fetcher github
                              :repo "joodie/parinfer-mode"))
    (evil-ruby-block-object :location (recipe
                                       :fetcher github
                                       :repo "XuHaoJun/evil-ruby-block-object")))
  "The list of Lisp packages required by the wamaral layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun wamaral/init-4clojure ()
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
    "hc" 'clojure-cheatsheet))

(defun wamaral/init-clojure-cheatsheet ())
(defun wamaral/init-slamhound ())
(defun wamaral/init-highlight-chars ())

(defun wamaral/init-helm-cider ()
  (require 'helm-cider)
  (helm-cider-mode 1))

(defun wamaral/init-dired+ ()
  (require 'dired-x)
  (require 'dired+)
  (setq dired-recursive-deletes 'top))

; (defun wamaral/init-dumb-jump ()
;   (require 'dumb-jump)
;   (setq dumb-jump-default-project "~/dev")
;   ;; (setq dumb-jump-quiet t)
;   (define-key evil-normal-state-map (kbd "C-]") 'dumb-jump-go)
;   (define-key evil-normal-state-map (kbd "C-[") 'dumb-jump-back)
;   (define-key evil-motion-state-map (kbd "g C-]") 'dumb-jump-quick-look))

(defun wamaral/init-tern-auto-complete ()
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup))))

(defun wamaral/init-editorconfig ()
  (require 'editorconfig)
  (editorconfig-mode 1)
  (add-hook 'after-change-major-mode-hook 'editorconfig-apply 'append))

(defun wamaral/init-evil-smartparens ()
  (require 'evil-smartparens))

(defun wamaral/init-projectile-direnv ()
  ;; (add-hook 'projectile-mode-hook 'projectile-direnv-export-variables)
  (require 'projectile-direnv)
  (spacemacs/set-leader-keys-for-minor-mode 'projectile-mode
    "p." 'projectile-direnv-export-variables))

;; (defun wamaral/init-evil-lispy ()
;;   (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
;;   (add-hook 'clojure-mode-hook #'evil-lispy-mode))

(defun wamaral/init-parinfer-mode ()
  ;; (require 'parinfer-mode)
  ;; (add-hook 'clojure-mode-hook #'parinfer-mode)
  ;; (add-hook 'cider-mode-hook #'parinfer-mode)
  )

(defun wamaral/init-evil-ruby-block-object ()
  (add-hook 'ruby-mode
            (lambda () (require 'evil-ruby-block-object))))

;; (defun wamaral/init-groovy-mode ()
;;   (add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . groovy-mode)))

(defun wamaral/highlight-chars ()
  (require 'highlight-chars)
  (add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
  (add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace))

(defun wamaral/init-wamaral ()
  (add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . groovy-mode)))

;;; packages.el ends here
