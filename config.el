;;; config.el -*- lexical-binding: t; -*-
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

(setq doom-font (font-spec :family "VictorMono Nerd Font" :size 12.0 :weight 'semi-bold)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 13.0 :weight 'semi-bold))

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

(setq-default delete-by-moving-to-trash t
              tab-width 4)

;; QT/QML
;; Ensure qml is added to the completion engine company
(add-to-list 'company-backends 'company-qml)

(setq company-qml-extra-qmltypes-files '("/home/chris/.Felgo/Felgo/gcc_64/import/VPlayPlugins/vplayplugins.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/import/VPlayApps/vplayapps.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/import/VPlay/vplay.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/import/Felgo/felgo.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/qml"))

;; gdscript
(require 'gdscript-mode)

(setq
 all-the-icons-scale-factor 0.8
 doom-modeline-height 30
 doom-modeline-major-mode-icon t
 doom-modeline-major-mode-color-icon t
 doom-modeline-mu4e t
 doom-modeline-bar-width 3)

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; org
(setq org-superstar-headline-bullets-list '("◉" "◈" "▸" "◎" "✬" "◇" "❉" "✙" "❖"))
(setq olivetti-body-width 0.6)
(setq org-imenu-depth 3)
(add-hook! org-mode (setq hl-line-mode nil))

(add-hook! 'org-mode-hook (lambda () (imenu-add-to-menubar "Imenu")))

;; (add-hook! org-mode (olivetti-mode t))
(add-hook! org-mode (org-autolist-mode t))

(map! :map org-mode-map
      :n "M-<tab>" 'org-show-subtree
      :n "C-M-o" 'turn-on-olivetti-mode)

;; (defun org-yt-follow-mpv (video-id)
;;   "Open youtube with VIDEO-ID."
;;   (async-shell-command (format "mpv %s" (concat "https://youtu.be/" video-id)))
;;   )

;; (map! :map org-mode-map
;;       :n "M-v" 'org-yt-follow-mpv)

(setq deft-directory "~/org/")

(setq org-agenda-files
   '("/home/chris/org/DMPREADME.org" "/home/chris/org/DMPTODO.org" "/home/chris/org/inbox.org" "/home/chris/org/notes.org" "/home/chris/org/repetition.org" "/home/chris/org/tasks.org" "/home/chris/org/tfc_plans.org" "/home/chris/org/ministry_team.org" "/home/chris/org/todo.org" "/home/chris/org/newsletter.org"))

(setq org-capture-templates
      '(("t" "Personal todo" entry
         (file+headline +org-capture-todo-file "Inbox")
         "* TODO %^{TODO name}\n%a\n%i%?" :prepend t)
        ("n" "Personal notes" entry
         (file+headline +org-capture-notes-file "Inbox")
         "* %u %?\n%i\n%a" :prepend t)
        ("j" "Journal" entry
         (file+olp+datetree +org-capture-journal-file)
         "* %U %?\n%i\n%a" :prepend t)
        ("p" "Templates for projects")
        ("pt" "Project-local todo" entry
         (file+headline +org-capture-project-todo-file "Inbox")
         "* TODO %?\n%i\n%a" :prepend t)
        ("pn" "Project-local notes" entry
         (file+headline +org-capture-project-notes-file "Inbox")
         "* %U %?\n%i\n%a" :prepend t)
        ("pc" "Project-local changelog" entry
         (file+headline +org-capture-project-changelog-file "Unreleased")
         "* %U %?\n%i\n%a" :prepend t)
        ("o" "Centralized templates for projects")
        ("ot" "Project todo" entry #'+org-capture-central-project-todo-file
         "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
        ("on" "Project notes" entry #'+org-capture-central-project-notes-file
         "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
        ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file
         "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))
)

;; org-super-agenda
(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Today"
                                         :time-grid t
                                         :scheduled today)
                                  (:name "Due Today"
                                         :deadline today)
                                  (:name "Important"
                                         :priority "A")
                                  (:name "Overdue"
                                         :time-grid t
                                         :scheduled today)
                                  (:name "Due soon"
                                         :deadline future)))
  :config
  (org-super-agenda-mode))
(setq org-super-agenda-header-map nil)

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

(use-package! org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-serve-files t
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(add-hook! org-roam-mode org-roam-server-mode t)

(setq +zen-text-scale 1.5)
;; (setq writeroom-global-effects writeroom-set-menu-bar-lines writeroom-set-tool-bar-lines writeroom-set-vertical-scroll-bars writeroom-set-bottom-divider-width)

;; elfeed
(map! :leader "o F" 'elfeed)

;; Make elfeed update when opened
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)


;; function to launch mpv from elfeed
(defun elfeed-v-mpv (url)
  "Watch a video from URL in MPV"
  (async-shell-command (format "mpv %s" url)))

(defun elfeed-view-mpv (&optional use-generic-p)
  "Youtube-feed link"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (elfeed-v-mpv it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; function to launch mpv from elfeed
(defun elfeed-a-mpv (url)
  "Watch a video from URL in MPV"
  (async-shell-command (format "mpv --no-audio-display %s" url)))

(defun elfeed-view-mpv-audio (&optional use-generic-p)
  "Youtube-feed link"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (elfeed-a-mpv it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; mapping keys to launch mpv
(map! :map elfeed-search-mode-map
      :n "v" 'elfeed-view-mpv
      :n "a" 'elfeed-view-mpv-audio)

;; Email

;; Add gmail
(set-email-account! "gmail"
  '((mu4e-sent-folder       . "/gmail/[Gmail].Sent Mail/")
    (smtpmail-smtp-user     . "ccochrun21@gmail.com")
    (user-mail-address      . "ccochrun21@gmail.com")    ;; only needed for mu < 1.4
    (mu4e-compose-signature . "---\nChris Cochrun"))
  nil)

;; Add my o365 account from work
(set-email-account! "office365"
  '((mu4e-sent-folder       . "/office/Sent Items")
    (mu4e-drafts-folder     . "/office/Drafts")
    (mu4e-trash-folder      . "/office/Deleted Items")
    (mu4e-refile-folder     . "/office/Archive")
    (smtpmail-smtp-user     . "chris@tfcconnection.org")
    (user-mail-address      . "chris@tfcconnection.org")    ;; only needed for mu < 1.4
    (mu4e-compose-signature . "---\nChris Cochrun"))
  t)

;; Add personal outlook account
(set-email-account! "office365"
  '((mu4e-sent-folder       . "/outlook/Sent")
    (mu4e-drafts-folder     . "/outlook/Drafts")
    (mu4e-trash-folder      . "/outlook/Deleted")
    (mu4e-refile-folder     . "/outlook/Archive")
    (smtpmail-smtp-user     . "chris.cochrun@outlook.com")
    (user-mail-address      . "chris.cochrun@outlook.com")    ;; only needed for mu < 1.4
    (mu4e-compose-signature . "---\nChris Cochrun"))
  t)

;; Add the ability to send email for o365
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.office365.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.office365.com" 587 "chris@tfcconnection.org" nil))
   smtpmail-default-smtp-server "smtp.office365.com"
   smtpmail-smtp-server "smtp.office365.com"
   smtpmail-smtp-service 587)

;; shortcuts in the jumplist by pressing "J" in the mu4e buffer
(setq   mu4e-maildir-shortcuts
    '((:maildir "/office/Archive"               :key ?a)
     (:maildir "/office/INBOX"                  :key ?i)
     (:maildir "/outlook/INBOX"                 :key ?l)
     (:maildir "/office/Junk Email"             :key ?j)
     (:maildir "/office/INBOX/Website Forms"    :key ?f)
     (:maildir "/gmail/INBOX"                   :key ?g)
     (:maildir "/office/sent"                   :key ?s)))

(add-hook! 'mu4e-view-mode-hook evil-normal-state)

(use-package! mu4e-views
  :after mu4e
  :defer nil
  :config
  (setq mu4e-views-completion-method 'ivy) ;; use ivy for completion
  (setq mu4e-views-default-view-method "html") ;; make xwidgets default
  (mu4e-views-mu4e-use-view-msg-method "html") ;; select the default
  (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window)) ;; when pressing n and p stay in the current window

(map! :map mu4e-headers-mode-map
      :n "H" #'mu4e-views-mu4e-select-view-msg-method)

;; Set Vterm to zsh
(setq vterm-shell "/bin/fish")

;; Change default evil escape sequence to spacemacs style
(setq evil-escape-key-sequence "fd")


;; Make Emacs transparent
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

;; Using counsel-linux-app for app launcher
(custom-set-variables '(counsel-linux-app-format-function #'counsel-linux-app-format-function-name-first))
(setq +ivy-buffer-preview t)

(setq tramp-terminal-type "tramp")
