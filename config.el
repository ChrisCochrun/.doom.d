;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Chris Cochrun"
      user-mail-address "chris@tfcconnection.org")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "VictorMono Nerd Font" :size 10.0 :weight 'semi-bold)
      doom-variable-pitch-font (font-spec :family "VictorMono Nerd Font" :size 11.0 :weight 'semi-bold))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-snazzy)
(setq doom-themes-treemacs-theme "doom-colors")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.



;; MY CHANGES
;; Author: Chris Cochrun
;; Email: chris@tfcconnection.org


;; Ensure qml is added to the completion engine company
(add-to-list 'company-backends 'company-qml)

;; Set Vterm to zsh
(setq vterm-shell "/bin/zsh")

;; Change default evil escape sequence to spacemacs style
(setq evil-escape-key-sequence "fd")


;; Make Emacs transparent
(set-frame-parameter (selected-frame) 'alpha '(70 70))
(add-to-list 'default-frame-alist '(alpha 70 70))

;; Org-Roam
(setq org-roam-directory "~/org")
(setq org-roam-buffer-width 0.25)
(setq org-roam-capture-templates
      '(("d" "default" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "${slug}"
         :head "#+TITLE: ${title}\n#+AUTHOR: Chris Cochrun\n#+CREATED: %<%D - %I:%M %p>\n\n* ")
        ("b" "bible" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "${slug}"
         :head "#+TITLE: ${title}\n#+AUTHOR: Chris Cochrun\n#+CREATED: %<%D - %I:%M %p>\n- tags %^G\n\n* ")))

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-label-truncate t
        org-roam-server-label-truncate-length 60
        org-roam-server-label-wrap-length 20))

;; Window movements TODO

;; Face edits TODO

;; QT/QML
(setq company-qml-extra-qmltypes-files '("/home/chris/.Felgo/Felgo/gcc_64/import/VPlayPlugins/vplayplugins.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/import/VPlayApps/vplayapps.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/import/VPlay/vplay.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/import/Felgo/felgo.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/qml"))

(setq deft-directory "~/org/")
