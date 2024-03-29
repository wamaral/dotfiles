;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;;; Code:
(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t)
     better-defaults
     (clojure :variables
              clojure-enable-sayid t
              clojure-enable-clj-refactor t)
     colors
     command-log
     copy-as-format
     csv
     docker
     elfeed
     elixir
     (elm :variables
          elm-sort-imports-on-save t)
     emacs-lisp
     emoji
     evil-commentary
     games
     git
     ;; github
     (go :variables
         go-use-gometalinter t
         go-tab-width 2)
     gtags
     (haskell :variables
              haskell-enable-hindent-style "gibiansky"
              ;; haskell-completion-backend 'intero
              haskell-enable-ghc-mod-support nil)
     html
     java
     (javascript :variables
                 javascript-disable-tern-port-files t)
     (latex :variables
            latex-enable-auto-fill t)
     lsp
     markdown
     (org :variables
          org-enable-reveal-js-support t
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-enable-org-journal-support t
          org-want-todo-bindings t
          org-projectile-file "TODO.org")
     pandoc
     plantuml
     purescript
     python
     ranger
     react
     rebox
     restclient
     ruby
     ruby-on-rails
     ;; (scala :variables
     ;;        scala-enable-eldoc t)
     scala
     search-engine
     semantic
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-term-shell "/bin/zsh"
            shell-default-height 30
            shell-default-position 'bottom)
     shell-scripts
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     spell-checking-enable-auto-dictionary t)
     sql
     (syntax-checking :variables
                      syntax-checking-enable-tooltips nil)
     systemd
     themes-megapack
     typescript
     (version-control :variables
                      version-control-global-margin t
                      version-control-diff-tool 'diff-hl)
     vimscript
     xkcd
     yaml
     wamaral
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '()

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(php-extras linum linum-relative eyebrowse neotree)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 50

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((projects . 10)
                                (recents . 15))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(sanityinc-tomorrow-bright
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Fira Mono"
                               :size 12.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key "-"

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols 'display-graphic-p

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers 'relative

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'current

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  ;; (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;; Fix dead keys
  (require 'iso-transl)

  ;; Persistent undo history
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,(concat spacemacs-cache-directory "undo"))))
  (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
    (make-directory (concat spacemacs-cache-directory "undo"))))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (setq powerline-default-separator nil)

  (spacemacs/toggle-menu-bar-on)

  (setq browse-url-browser-function 'browse-url-generic
        engine/browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome-unstable")

  ;; file formats
  (add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . groovy-mode))
  (add-to-list 'auto-mode-alist '("\\.aurora\\'" . python-mode))

  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  ;; (with-eval-after-load 'flycheck
  ;;   (flycheck-define-checker javascript-flow
  ;;     "A JavaScript syntax and style checker using Flow.
  ;;      See URL `http://flowtype.org/'."
  ;;     :command ("flow" source-original)
  ;;     :error-patterns
  ;;     ((error line-start (1+ nonl) ":" line ":" column ":" (message) line-end))
  ;;     :modes (js2-mode web-mode react-mode))
  ;;   (flycheck-add-next-checker 'javascript-eslint 'javascript-flow)
  ;;   (add-to-list 'flycheck-checkers 'javascript-flow 'append))

  ;; aggressive indent
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'cider-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode)

  ;; smartparens
  (turn-off-show-smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)

  ;; cleverparens
  ;; (spacemacs/toggle-evil-cleverparens-on)
  ;; (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  ;; (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  ;; (add-hook 'cider-mode-hook #'evil-cleverparens-mode)

  ;; auto complete
  (global-company-mode)
  (setq ac-auto-start 2)
  (setq ac-auto-show-menu 0.8)
  (setq ac-quick-help-delay 1)
  ;; (add-hook 'ruby-mode-hook
  ;;           (lambda ()
  ;;             (make-local-variable 'ac-stop-words)
  ;;             (add-to-list 'ac-stop-words "end")))

  ;; cider
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'subword-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  (cider-auto-test-mode 1)
  (setq cider-auto-mode t)
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-stacktrace-default-filters '(tooling dup java))
  (setq cider-pprint-fn 'puget)
  (setq cider-repl-use-pretty-printing t)

  ;; auto load all files under src/ after connecting
  (defun cider-auto-load-src-hook ()
    (cider-nrepl-sync-request:eval "(clojure.tools.namespace.repl/set-refresh-dirs \"src\")")
    (cider-ns-refresh))
  (add-hook 'cider-connected-hook 'cider-auto-load-src-hook)

  (with-eval-after-load 'clojure-mode
    (lambda ()
      (define-clojure-indent
       (alet 'defun)
       (mlet 'defun)
       (m/alet 'defun)
       (m/mlet 'defun)
       (for-all 'defun)
       (prop/for-all 'defun)
       (verify 'defun))))

  (defun midje-load-facts ()
    "Load current namespace (or their test namespace) facts"
    (interactive)
    (cider-ensure-connected)
    (when buffer-file-name
      (let* ((ns (cider-current-ns))
             (test-ns (if (projectile-test-file-p (buffer-file-name)) ns (format "%s-test" ns)))
             (cmd (format "(do (midje.repl/forget-facts *ns* '%s) (midje.repl/load-facts '%s))" test-ns test-ns)))
        (cider-interactive-eval cmd))))

  (defun midje-load-facts-current-buffer ()
    "Load current buffer facts"
    (interactive)
    (cider-ensure-connected)
    (when buffer-file-name
      (let* ((ns (cider-current-ns))
             (cmd (format "(do (midje.repl/forget-facts *ns* '%s) (midje.repl/load-facts '%s))" ns ns)))
        (cider-interactive-eval cmd))))

  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "tm" 'midje-load-facts)
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "tM" 'midje-load-facts-current-buffer)

  ;; Disable clojure core.typed checker
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc clojure-cider-typed))

  ;; Enable hlint
  ;; (with-eval-after-load 'intero
  ;;   (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

  ;; Don't move cursor back on esc in haskell repl
  (when (configuration-layer/package-usedp 'haskell)
    (add-hook 'haskell-interactive-mode-hook
              (lambda ()
                (setq-local evil-move-cursor-back nil))))

  (setq evil-shift-width 2)
  (setq evil-find-skip-newlines t)
  (setq-default evil-want-fine-undo nil)

  ;; magit
  (setq magit-repository-directories '("~/dev/"))

  ;; ranger
  (setq ranger-cleanup-on-disable t)
  ;; (setq ranger-cleanup-eagerly t)
  (setq ranger-show-dotfiles t)
  (setq ranger-max-preview-size 10)
  (setq ranger-dont-show-binary t)

  ;; tags
  ;; (add-hook 'prog-mode-hook 'ggtags-mode)
  ;; (define-key evil-normal-state-map (kbd "C-]") 'ggtags-find-tag-dwim)
  ;; (define-key evil-motion-state-map (kbd "g C-]") 'ggtags-grep)
  (add-hook 'evil-motion-state-minor-mode
            (lambda ()
              (define-key evil-normal-state-map (kbd "C-]") 'spacemacs/jump-to-definition)))
  ;; (define-key evil-normal-state-map (kbd "C-]") 'spacemacs/jump-to-definition)
  (define-key evil-motion-state-map (kbd "g C-]") 'helm-gtags-find-tag-from-here)
  (setq helm-gtags-display-style 'detail)
  (setq helm-gtags-auto-update t)
  (setq helm-gtags-ignore-case t)
  (setq tags-revert-without-query t)
  (setq tags-case-fold-search t)        ; t=case-insensitive, nil=case-sensitive
  (setq large-file-warning-threshold (* 50 1024 1024)) ; 50MB
  (setq dumb-jump-default-project "~/dev")

  ;; js indent
  (setq-default js2-basic-offset 2
                js-indent-level 2
                css-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-attr-indent-offset 2)

  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

  (setq web-mode-enable-auto-pairing t)

  ;; coffee mode
  (setq coffee-tab-width 2)
  (setq coffee-args-compile '("-c" "-m")) ;; generating sourcemap
  (add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)

  ;; json mode
  (setq json-reformat:indent-width 2)
  (setq json-reformat:pretty-string t)

  ;; eclim
  (setq eclim-eclipse-dirs "/usr/lib/eclipse"
        eclim-executable "/usr/lib/eclipse/eclim")

  (setq-default ruby-version-manager 'chruby)
  (setq ruby-insert-encoding-magic-comment nil)
  (setq enh-ruby-add-encoding-comment-on-save nil)

  (volatile-highlights-mode t)

  ;; (indent-guide-global-mode)
  ;; (setq indent-guide-recursive t)
  (spacemacs/toggle-indent-guide-globally-on)

  ;; (spacemacs/toggle-automatic-symbol-highlight-on)

  (global-flycheck-mode)

  ;; (set-face-background 'highlight-indentation-face "#e3e3d3")
  ;; (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
  ;; (add-hook 'font-lock-mode-hook 'highlight-indentation-current-column-mode)

  (spacemacs/toggle-whitespace-globally-on)
  ;; (global-whitespace-mode)
  (setq whitespace-style '(face trailing tabs empty))
  ;; (setq whitespace-tab 'border)
  ;; (setq whitespace-trailing 'ahs-edit-mode-face)
  ;; (setq whitespace-empty 'ahs-plugin-defalt-face)
  ;; (setq whitespace-display-mappings
  ;;       ;; all numbers are Unicode codepoint in decimal. try (insert-char 182) to see it
  ;;       '(;;(space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
  ;;         (newline-mark 10 [8629 10]) ; 10 LINE FEED
  ;;         (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
  ;;         ))

  (setq ensime-startup-notification nil)

  (defun scalafmt-file ()
    "Runs scalafmt for current buffer (spawns new JVM)"
    (interactive)
    (shell-command (concat "/usr/local/bin/scalafmt " (shell-quote-argument (buffer-file-name)))))
  (spacemacs/set-leader-keys-for-major-mode 'scala-mode "rF" 'scalafmt-file)

  (setq-default elfeed-feeds
                '("https://www.reddit.com/r/haskell/.rss"
                  "http://xkcd.com/rss.xml"))

  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))

  ;; goodbye!
  (global-unset-key (kbd "C-h h"))

  ;; smooth scroll
  (setq scroll-conservatively 101
        scroll-margin 5
        scroll-preserve-screen-position 't)

  ;; orgmode
  (setq org-plantuml-jar-path (expand-file-name "/opt/plantuml/plantuml.jar"))
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (with-eval-after-load 'org
    (lambda ()
      (org-projectile:prompt)
      (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
      (setq org-projectile:per-repo-filename "TODO.org")
      (setq-default org-capture-templates
                    '(("t" "Tasks")
                      ("tg" "General" entry (file+headline "" "Tasks")
                       "* TODO %?\n%i\n%U"
                       :empty-lines 1)
                      ("tl" "Location" entry (file+headline "" "Tasks")
                       "* TODO %?\n%i\n%U\n%a"
                       :empty-lines 1)
                      ("n" "Notes")
                      ("ng" "General" entry (file+headline "" "Notes")
                       "* %?\n%i\n%U"
                       :empty-lines 1)
                      ("nl" "Location" entry (file+headline "" "Notes")
                       "* %?\n%i\n%U\n%a"
                       :empty-lines 1)))
      ;; (add-to-list 'org-capture-templates (org-projectile:project-todo-entry))

      (setq org-plantuml-jar-path (expand-file-name "/opt/plantuml/plantuml.jar"))
      (setq org-confirm-babel-evaluate nil
            org-src-fontify-natively t
            org-src-tab-acts-natively t)

      (require 'ob-python)
      (require 'ob-clojure)
      (require 'ob-perl)
      (require 'ob-dot)
      (require 'ob-gnuplot)
      (require 'ob-lisp)
      (require 'ob-org)
      (require 'ob-calc)
      (require 'ob-js)
      (require 'ob-latex)
      (require 'ob-plantuml)
      (require 'ob-sh)
      (require 'ob-ditaa)
      (require 'ob-octave)
      (require 'ob-sed)
      (require 'ob-sql)

      (org-babel-do-load-languages
       'org-babel-load-languages
       '((http       . t)
         (sh         . t)
         (sed        . t)
         (sqlite     . t)
         (js         . t)
         (emacs-lisp . t)
         (clojure    . t)
         (perl       . t)
         (python     . t)
         (ruby       . t)
         (octave     . t)
         (gnuplot    . t)
         (lisp       . t)
         (org        . t)
         (calc       . t)
         (latex      . t)
         (plantuml   . t)
         (ditaa      . t)
         (dot        . t)))))

  ;; custom fns
  (defun touch-current-file ()
    "Updates mtime on the file for the current buffer"
    (interactive)
    (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
    (clear-visited-file-modtime))
  (spacemacs/set-leader-keys "ft" 'touch-current-file)

  (defun untabify-buffer ()
    "Remove tabs from whole buffer."
    (interactive)
    (save-excursion
      (untabify (point-min) (point-max))))

  (defun indent-whole-buffer ()
    "Indent whole buffer."
    (interactive)
    (save-excursion
      (indent-region (point-min) (point-max))))

  (defun cleanup-buffer ()
    "Remove trailing whitespace, indent and remove tabs from whole buffer."
    (interactive)
    (save-excursion
      (delete-trailing-whitespace)
      (indent-region (point-min) (point-max))
      (untabify (point-min) (point-max))))
  (spacemacs/set-leader-keys "bc" 'cleanup-buffer)

  (defun file-reopen-as-root ()
    "Reopens current file as root."
    (interactive)
    (when buffer-file-name
      (find-alternate-file
       (concat "/sudo:root@localhost:"
               buffer-file-name))))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(Info-fontify-angle-bracketed-flag nil)
   '(blink-cursor-mode nil)
   '(column-number-mode t)
   '(company-backends
     (quote
      (company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                    (company-dabbrev-code company-keywords)
                    company-oddmuse company-dabbrev)))
   '(company-idle-delay 0.4)
   '(company-show-numbers t)
   '(diff-hl-margin-mode t)
   '(diff-hl-side (quote right))
   '(evil-want-Y-yank-to-eol t)
   '(face-font-family-alternatives
     (quote
      (("Fira Mono" "Monospace" "courier" "fixed")
       ("courier" "CMU Typewriter Text" "fixed")
       ("Sans Serif" "helv" "helvetica" "arial" "fixed")
       ("helv" "helvetica" "arial" "fixed"))))
   '(fci-rule-color "#D0BF8F" t)
   '(flycheck-haskell-hlint-executable "stack hlint")
   '(font-use-system-font nil)
   '(ggtags-find-tag-hook (quote (recenter)))
   '(ggtags-global-output-format (quote ctags))
   '(ggtags-navigation-mode nil)
   '(ggtags-sort-by-nearness t)
   '(ggtags-use-idutils t)
   '(global-diff-hl-mode t)
   '(global-highlight-parentheses-mode t)
   '(haskell-check-command "/home/wamaral/.local/bin/hlint")
   '(haskell-compile-cabal-build-alt-command
     "/bin/zsh -c cd %s && cabal clean -s && cabal build --ghc-option=-ferror-spans")
   '(haskell-compile-cabal-build-command
     "/bin/zsh -c cd %s && cabal build --ghc-option=-ferror-spans")
   '(haskell-hasktags-path "/home/wamaral/.local/bin/hasktags")
   '(haskell-mode-stylish-haskell-path "/home/wamaral/.local/bin/stylish-haskell")
   '(haskell-process-path-ghci "/home/wamaral/bin/stack exec -- ghci")
   '(haskell-process-path-stack "/home/wamaral/bin/stack")
   '(haskell-process-suggest-haskell-docs-imports t)
   '(haskell-stylish-on-save t)
   '(helm-ag-use-agignore t)
   '(hindent-process-path "stack hindent")
   '(js-enabled-frameworks (quote (javascript extjs)))
   '(js2-include-node-externs t)
   '(js2-mode-show-parse-errors nil)
   '(js2-mode-show-strict-warnings nil)
   '(linum-relative-current-symbol "")
   '(linum-relative-format "%3s")
   '(menu-bar-mode t)
   '(org-agenda-files
     (quote
      ("~/org/calendars/default.org" "~/org/calendars/work.org" "~/org/notes.org")))
   '(org-default-notes-file "~/org/notes.org")
   '(org-export-async-init-file
     "/home/wamaral/.emacs.d/layers/+emacs/org/local/org-async-init.el" t)
   '(org-export-backends
     (quote
      (ascii html icalendar latex md odt confluence freemind)))
   '(org-modules
     (quote
      (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-mouse org-rmail org-w3m)))
   '(package-selected-packages
     (quote
      (insert-shebang exotica-theme apiwrap evil-lion org-category-capture ghub+ engine-mode window-purpose flycheck-clojure dante skewer-mode with-editor helm-flycheck evil-little-word evil-embrace embrace emojify emoji-cheat-sheet-plus elfeed-web elfeed-org elfeed-goodies ace-jump-mode elfeed company-emoji pandoc-mode ox-twbs ox-pandoc org-journal ghub org-brain impatient-mode evil-org highlight-chars evil-smartparens evil-ruby-block-object ob-elixir string-inflection restclient-helm rebecca-theme projectile-direnv plantuml-mode parinfer-mode pacmacs ox-reveal symon f browse-at-remote meghanada scala-mode elm-mode magithub solarized-theme faceup madhat2r-theme winum unfill sudoku sourcerer-theme go-guru slime-company minitest imenu-list pug-mode dired+ alchemist helm-gtags ggtags company-emacs-eclim ranger erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks eclim marshal evil-unimpaired caml ob-http auto-complete org-projectile github-search groovy-mode erlang editorconfig request pcache mwim ghc utop tuareg systemd rebox2 psci deferred purescript-mode psc-ide ocp-indent nginx-mode merlin flycheck-purescript flycheck-gometalinter docker tablist docker-tramp csv-mode company-auctex auctex-latexmk auctex company-ghci helm-cider paredit highlight gh inf-ruby typescript-mode slime racket-mode helm-dash geiser flycheck-elm anzu restclient ensime sbt-mode elixir-mode hlint-refactor flyspell-correct-helm flycheck-mix zeal-at-point xkcd vimrc-mode typit mmt tide dash-functional tern color-identifiers-mode ox-gfm darkokai-theme dumb-jump popup packed git-gutter git-commit nlinum iedit hydra flyspell-correct swiper ivy zenburn-theme which-key web-mode ujelly-theme spacemacs-theme spaceline powerline sass-mode ruby-test-mode rainbow-delimiters projectile-rails organic-green-theme org-plus-contrib open-junk-file move-text monokai-theme material-theme magit-gitflow leuven-theme js2-refactor ido-vertical-mode helm-projectile helm-ag gruvbox-theme gotham-theme google-translate fish-mode evil-surround evil-mc emmet-mode dracula-theme company-web color-theme-sanityinc-solarized clj-refactor multiple-cursors badwolf-theme anti-zenburn-theme aggressive-indent ace-link cider clojure-mode smartparens bind-map evil flycheck haskell-mode go-mode yasnippet company helm helm-core avy markdown-mode alert magit async projectile js2-mode dash s color-theme-sanityinc-tomorrow zonokai-theme zen-and-art-theme yaml-mode xterm-color ws-butler wolfram-mode window-numbering web-completion-data web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package undo-tree underwater-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org thrift tern-auto-complete tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stickyfunc-enhance stekene-theme stan-mode srefactor sql-indent spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smooth-scrolling smeargle slim-mode shm shell-pop seti-theme scss-mode scad-mode rvm ruby-tools rubocop rspec-mode robe reverse-theme restart-emacs rbenv rake rainbow-mode rainbow-identifiers railscasts-theme queue quelpa qml-mode purple-haze-theme professional-theme popwin planet-theme pkg-info phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastels-on-dark-theme paradox page-break-lines orgit org-repo-todo org-present org-pomodoro org-download org-bullets omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme multi-term monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme matlab-mode markdown-toc majapahit-theme magit-popup magit-gh-pulls macrostep lush-theme lorem-ipsum log4e livid-mode nlinum-relative link-hint light-soap-theme less-css-mode julia-mode json-mode js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme info+ inflections indent-guide hungry-delete htmlize hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flyspell helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet hc-zenburn-theme haskell-snippets haml-mode gruber-darker-theme grandshell-theme goto-chg golden-ratio go-eldoc gnuplot gntp github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md gandalf-theme flycheck-pos-tip flycheck-haskell flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator feature-mode farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-search-highlight-persist evil-numbers evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-commentary evil-cleverparens evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help emacs-eclim elisp-slime-nav edn dockerfile-mode django-theme diff-hl define-word darktooth-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-tern company-statistics company-shell company-quickhelp company-go company-ghc company-cabal command-log-mode column-enforce-mode colorsarenice-theme coffee-mode cmm-mode clues-theme clojure-snippets clojure-cheatsheet clean-aindent-mode cider-eval-sexp-fu chruby cherry-blossom-theme busybee-theme bundler buffer-move bubbleberry-theme birds-of-paradise-plus-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile arduino-mode apropospriate-theme ample-zen-theme ample-theme alect-themes afternoon-theme adaptive-wrap ace-window ace-jump-helm-line ac-ispell 4clojure)))
   '(plantuml-jar-path "/opt/plantuml/plantuml.jar")
   '(projectile-enable-idle-timer nil)
   '(projectile-globally-ignored-files (quote ("TAGS" "GPATH" "GRTAGS" "GTAGS" "tags")))
   '(projectile-idle-timer-hook (quote (helm-gtags-update-tags)))
   '(projectile-rails-global-mode t)
   '(projectile-switch-project-action (quote helm-projectile))
   '(projectile-use-git-grep t)
   '(projectile-verbose nil)
   '(psc-ide-add-import-on-completion t t)
   '(psc-ide-rebuild-on-save nil t)
   '(safe-local-variable-values
     (quote
      ((haskell-process-use-ghci . t)
       (haskell-indent-spaces . 4)
       (elixir-enable-compilation-checking . t)
       (elixir-enable-compilation-checking))))
   '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum-relative-current-face ((t (:inherit linum :foreground "#CAE682" :weight bold)))))
)
