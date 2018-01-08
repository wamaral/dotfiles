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
    flycheck-clojure
    helm-cider
    helm-flycheck
    dired+
    tern-auto-complete
    evil-smartparens
    evil-embrace
    projectile-direnv
    highlight-chars
    magithub
    (evil-little-word :location (recipe
                                 :fetcher github
                                 :repo "tarao/evil-plugins"
                                 :files ("evil-little-word.el")))
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

(defun wamaral/init-4clojure ())

(defun wamaral/init-flycheck-clojure ()
  (eval-after-load 'flycheck '(flycheck-clojure-setup))
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'clojure-mode-hook #'flycheck-mode))

(defun wamaral/init-highlight-chars ())

(defun wamaral/init-helm-cider ()
  (require 'helm-cider)
  (helm-cider-mode 1))

(defun wamaral/init-helm-flycheck ()
  (require 'helm-flycheck)
  (spacemacs/set-leader-keys "ee" 'helm-flycheck))

(defun wamaral/init-dired+ ()
  (require 'dired-x)
  (require 'dired+)
  (setq dired-recursive-deletes 'top))

(defun wamaral/init-tern-auto-complete ()
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup))))

(defun wamaral/init-evil-smartparens ()
  (require 'evil-smartparens))

(defun wamaral/init-evil-embrace ()
  (require 'evil-embrace)
  (evil-embrace-enable-evil-surround-integration))

(defun wamaral/init-projectile-direnv ()
  ;; (add-hook 'projectile-mode-hook 'projectile-direnv-export-variables)
  (require 'projectile-direnv)
  (spacemacs/set-leader-keys ;;'projectile-mode
    "p." 'projectile-direnv-export-variables))

(defun wamaral/init-evil-little-word ()
  (use-package evil-little-word
               :commands (evil-forward-little-word-begin
                           evil-backward-little-word-begin
                           evil-forward-little-word-end
                           evil-backward-little-word-end
                           evil-a-little-word
                           evil-inner-little-word)
               :init (progn
                       (define-key evil-motion-state-map (kbd "glw") 'evil-forward-little-word-begin)
                       (define-key evil-motion-state-map (kbd "glb") 'evil-backward-little-word-begin)
                       (define-key evil-motion-state-map (kbd "glW") 'evil-forward-little-word-end)
                       (define-key evil-motion-state-map (kbd "glB") 'evil-backward-little-word-end)
                       (define-key evil-outer-text-objects-map (kbd "lw") 'evil-a-little-word)
                       (define-key evil-inner-text-objects-map (kbd "lw") 'evil-inner-little-word))))

(defun wamaral/init-magithub ()
  (use-package magithub
               :after magit
               :ensure t
               :config (magithub-feature-autoinject t)))

(defun wamaral/init-evil-ruby-block-object ()
  (add-hook 'ruby-mode
            (lambda () (require 'evil-ruby-block-object))))

(defun wamaral/highlight-chars ()
  (require 'highlight-chars)
  (add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
  (add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace))

(defun wamaral/init-wamaral ()
  (add-to-list 'auto-mode-alist '("\\Jenkinsfile\\'" . groovy-mode)))

;;; packages.el ends here
