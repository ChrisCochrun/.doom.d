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
      doom-variable-pitch-font (font-spec :family "NotoSans Nerd Font" :size 13.0 :weight 'light))

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

(setq +doom-dashboard-banner-dir "/home/chris/.doom.d/banner/")
(setq +doom-dashboard-banner-file "whitelionsmall.png")

;; org
(setq org-superstar-headline-bullets-list '("◉" "◈" "▸" "◎" "✬" "◇" "❉" "✙" "❖"))
(setq olivetti-body-width 0.6)
(setq org-imenu-depth 3)
(add-hook! org-mode (setq hl-line-mode nil))

(add-hook! 'org-mode-hook (lambda () (imenu-add-to-menubar "Imenu")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROJ(p)" "STRT(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "CNCL(c)")
 (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")))


;; (add-hook! org-mode (olivetti-mode t))
;; (add-hook! org-mode (org-autolist-mode t))
(add-hook! org-mode (toc-org-mode t))

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

(setq org-roam-dailies-capture-templates
      '(("d" "daily" plain #'org-roam-capture--get-point ""
        :immediate-finish t
        :file-name "%<%m-%d-%Y>"
        :head "#+TITLE: %<%m-%d-%Y>\n#+AUTHOR: Chris Cochrun mailto://chris@tfcconnection.org\n#+CREATED: %<%D - %I:%M %p>\n\n* HFL\n* Tasks\n* Family\n** How Do I Love Abbie?")
        ("b" "biblical daily" plain #'org-roam-capture--get-point ""
         :immediate-finish t
        :file-name "%<%m-%d-%Y>-bib"
        :head "#+TITLE: %<%m-%d-%Y> - Biblical\n#+AUTHOR: Chris Cochrun mailto://chris@tfcconnection.org")))

(use-package! org-roam-server
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

;; elfeed
(map! :leader "o F" 'elfeed)

;; Make elfeed update when opened
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

;; function to launch mpv from elfeed
(defun elfeed-v-mpv (url)
  "Watch a video from URL in MPV"
  (emms-add-url url))

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
  "Watch a video from URL in MPV
This creates a new mpv video from the url passed to it."
  (async-shell-command (format "mpv --prefetch-playlist=yes --no-audio-display --playlist-start=auto %s" url)))

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

(map! :leader "o M" 'emms)
(require 'emms-setup)
(emms-all)
(emms-default-players)

(map! :leader "P" 'emms-pause)

;; Add gmail
(set-email-account! "gmail"
  '((mu4e-sent-folder       . "/gmail/[Gmail].Sent Mail/")
    (smtpmail-smtp-user     . "ccochrun21@gmail.com")
    (user-mail-address      . "ccochrun21@gmail.com")    ;; only needed for mu < 1.4
    (mu4e-compose-signature . "---\nChris Cochrun"))
  nil)

;; Add personal outlook account
(set-email-account! "office365"
  '((mu4e-sent-folder       . "/outlook/Sent")
    (mu4e-drafts-folder     . "/outlook/Drafts")
    (mu4e-trash-folder      . "/outlook/Deleted")
    (mu4e-refile-folder     . "/outlook/Archive")
    (smtpmail-smtp-user     . "chris.cochrun@outlook.com")
    (user-mail-address      . "chris.cochrun@outlook.com")    ;; only needed for mu < 1.4
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
(setq mu4e-maildir-shortcuts
    '((:maildir "/office/Archive"               :key ?a)
     (:maildir "/office/INBOX"                  :key ?i)
     (:maildir "/outlook/INBOX"                 :key ?l)
     (:maildir "/office/Junk Email"             :key ?j)
     (:maildir "/office/INBOX/Website Forms"    :key ?f)
     (:maildir "/gmail/INBOX"                   :key ?g)
     (:maildir "/office/sent"                   :key ?s)))

(add-hook! 'mu4e-view-mode-hook evil-normal-state)

;; (add-to-list mu4e-headers-actions ("org capture message" . mu4e-org-store-and-capture))

(setq mu4e-bookmarks
      '((:name "Unread messages"
         :query "flag:unread AND NOT flag:trashed AND NOT maildir:\"/outlook/Junk\" AND NOT maildir:\"/office/Junk Email\" AND NOT maildir:\"/outlook/Deleted\" AND NOT maildir:\"/office/Deleted Items\""
         :key 117)
        (:name "Today's messages" :query "date:today..now" :key 116)
        (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key 119)
        (:name "Messages with images" :query "mime:image/*" :key 112)))

(mu4e-alert-set-default-style 'notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
(setq mu4e-alert-email-notification-types '(count))

(setq mu4e-alert-interesting-mail-query
      (concat
       "flag:unread"
       " AND NOT flag:trashed"
       " AND NOT maildir:"
       "\"/outlook/Junk\" AND NOT maildir:\"/office/Junk Email\" AND NOT maildir:\"/outlook/Deleted\" AND NOT maildir:\"/office/Deleted Items\""))

(use-package! calfw
  :config
  (defun my-open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "Cyan")  ; org-agenda source
      (cfw:ical-create-source "NV" "https://www.nvhuskies.org/vnews/display.vical" "Green")  ; School Calendar
      (cfw:ical-create-source "Outlook" "https://outlook.office365.com/owa/calendar/62a0d491bec4430e825822afd2fd1c01@tfcconnection.org/9acc5bc27ca24ce7a900c57284959f9d8242340735661296952/S-1-8-2197686000-2519837503-3687200543-3873966527/reachcalendar.ics" "Purple")  ; Outlook Calendar
      ))))

(map! :leader
      (:prefix ("a" . "Calendar")
       :desc "Open Calendar" "c" 'my-open-calendar))
(map! :map cfw:calendar-mode-map
      "SPC" 'doom/leader
      "q" 'kill-this-buffer
      "RET" 'cfw:show-details-command)
(map! :map cfw:details-mode-map
      :n "q" 'cfw:details-kill-buffer-command)

(setq password-cache t)
(setq password-cache-expiry 3600)

;;; Extra execution information
(defvar chris/eshell-status-p t
  "If non-nil, display status before prompt.")
(defvar chris/eshell-status--last-command-time nil)
(make-variable-buffer-local 'chris/eshell-status--last-command-time)
(defvar chris/eshell-status-min-duration-before-display 0
  "If a command takes more time than this, display its duration.")

(defun chris/eshell-status-display ()
  (if chris/eshell-status--last-command-time
    (let ((duration (time-subtract (current-time) chris/eshell-status--last-command-time)))
      (setq chris/eshell-status--last-command-time nil)
      (when (> (time-to-seconds duration) chris/eshell-status-min-duration-before-display)
        (format "  %.3fs %s"
                (time-to-seconds duration)
                (format-time-string "| %F %T" (current-time)))))
    (format "  0.000s")))

(defun chris/eshell-status-record ()
  (setq chris/eshell-status--last-command-time (current-time)))

(add-hook 'eshell-pre-command-hook 'chris/eshell-status-record)

(setq eshell-prompt-function
      (lambda nil
        (let ((path (abbreviate-file-name (eshell/pwd))))
          (concat
           (if (or (string= system-name "archdesktop") (string= system-name "chris-linuxlaptop"))
               nil
             (format
              (propertize "\n(%s@%s)" 'face '(:foreground "#606580"))
              (propertize (user-login-name) 'face '(:inherit compilation-warning))
              (propertize (system-name) 'face '(:inherit compilation-warning))))
           (if (and (require 'magit nil t) (or (magit-get-current-branch) (magit-get-current-tag)))
               (let* ((root (abbreviate-file-name (magit-rev-parse "--show-toplevel")))
                      (after-root (substring-no-properties path (min (length path) (1+ (length root))))))
                 (format
                  (propertize "\n[ %s | %s@%s ]" 'face font-lock-comment-face)
                  (propertize root 'face `(:inherit org-warning))
                  (propertize after-root 'face `(:inherit org-level-1))
                  (propertize (or (magit-get-current-branch) (magit-get-current-tag)) 'face `(:inherit org-macro))))
             (format
              (propertize "\n[%s]" 'face font-lock-comment-face)
              (propertize path 'face `(:inherit org-level-1))))
           (when chris/eshell-status-p
             (propertize (or (chris/eshell-status-display) "") 'face font-lock-comment-face))
           (propertize "\n" 'face '(:inherit org-todo :weight ultra-bold))
           " "))))

;;; If the prompt spans over multiple lines, the regexp should match
;;; last line only.
(setq-default eshell-prompt-regexp "^ ")

;; Set Vterm to zsh
(setq vterm-shell "/bin/fish")

;; Change default evil escape sequence to spacemacs style
(setq evil-escape-key-sequence "fd")


;; Make Emacs transparent
(set-frame-parameter (selected-frame) 'alpha '(75 75))
(add-to-list 'default-frame-alist '(alpha 75 75))

(add-to-list 'company-backends 'company-qml)

(setq company-qml-extra-qmltypes-files '("/home/chris/.Felgo/Felgo/gcc_64/import/VPlayPlugins/vplayplugins.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/import/VPlayApps/vplayapps.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/import/VPlay/vplay.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/import/Felgo/felgo.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/qml"))

;; Using counsel-linux-app for app launcher
(custom-set-variables '(counsel-linux-app-format-function #'counsel-linux-app-format-function-name-first))
;; (setq +ivy-buffer-preview t)

(set-frame-parameter nil 'fullscreen 'fullboth)

(display-time-mode t)
(setq display-time-interval 60)
(setq display-time-format "%a %b %e, %l:%M %p")
(display-battery-mode)

(require 'exwm)
(require 'exwm-config)
(exwm-config-example)
(exwm-enable)

(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist '(0 "DVI-D-0" 1 "HDMI-0"))
(add-hook! 'exwm-randr-screen-change-hook
  (lambda ()
    (start-process-shell-command
     "xrandr" nil "xrandr --output DVI-D-0 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-0 --mode 1600x900 --pos 1920x0 --rotate normal")))
(exwm-randr-enable)

(require 'exwm-systemtray)
(exwm-systemtray-enable)
(if (string= system-name "chris-linuxlaptop")
    (setq exwm-systemtray-height 38
          exwm-systemtray-icon-gap 12)
  (setq exwm-systemtray-height 18
        exwm-systemtray-icon-gap 6))

(setq exwm-workspace-number 8
      exwm-workspace-show-all-buffers t)


(defun chris/exwm-workspace-next ()
  "Move forward one workspace."
  (interactive)
  (if (< exwm-workspace-current-index (1- exwm-workspace-number))
      (exwm-workspace-switch (1+ exwm-workspace-current-index))
    (message "No next workspace.")))

(defun chris/exwm-workspace-prev ()
  "Move to the previous workspace."
  (interactive)
  (if (> exwm-workspace-current-index 0)
      (exwm-workspace-switch (1- exwm-workspace-current-index))
    (message "No previous workspace.")))

(defun chris/exwm-flameshot ()
  "Take a screenshot using flameshot"
  (interactive)
  (shell-command "flameshot gui"))

(defun chris/exwm-launch-dolphin ()
  "launch dolphin"
  (interactive)
  (start-process-shell-command "dolphin" none "dolphin"))

;; microphone commands
(if (string= system-name "archdesktop") (setq desktop-environment-volume-toggle-microphone-command "amixer -c 2 set Mic toggle"))

;;Global keybindings
(setq exwm-input-global-keys
          `(
            ;; 's-r': Reset (to line-mode).
            ([?\s-r] . exwm-reset)
            ;; 's-w': Switch workspace.
            ([?\s-w] . exwm-workspace-switch)
            ([?\s-j] . chris/exwm-workspace-prev)
            ([?\s-k] . chris/exwm-workspace-next)
            ;; Switch Buffer
            ([?\s-b] . +ivy/switch-buffer)
            ([?\s-m] . exwm-workspace-move-window)
            ;; close app
            ([?\s-c] . kill-this-buffer)
            ;; Launch Dolphin
            ([?\s-d] . chris/exwm-launch-dolphin)
            ;; Launch eshell
            ([s-return] . +eshell/toggle)
            ;; screenshot
            ([print] . chris/exwm-flameshot)
            ;; Audio
            ([XF86AudioRaiseVolume] . desktop-environment-volume-increment)
            ([XF86AudioLowerVolume] . desktop-environment-volume-decrement)
            ([XF86AudioMute] . desktop-environment-toggle-mute)
            ([XF86Launch8] . desktop-environment-toggle-microphone-mute)
            ;; Brightness
            ([XF86MonBrightnessUp] . desktop-environment-brightness-increment)
            ([XF86MonBrightnessDown] . desktop-environment-brightness-decrement)
            ;; 's-&': Launch application.
            ([?\s-r] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ([menu] . counsel-linux-app)
            ;; 's-N': Switch to certain workspace.
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))))

(setq exwm-floating-border-width 0)
(setq exwm-manage-configurations '(((or (string-match-p "libreoffice"
                                                   exwm-class-name)
                                   (string= exwm-class-name "MuseScore3")
                                   (string= exwm-class-name "Gimp")
                                   (string= exwm-class-name "mpv")
                                   (string= exwm-class-name "feh")
                                   (string= exwm-class-name "dolphin")
                                   (string= exwm-title "Event Tester"))
                               floating t
                               floating-mode-line nil
                               )))

(start-process-shell-command "xset" nil "xset r rate 220 90")
(start-process-shell-command "fehwall" nil "feh --bg-fill ~/Pictures/wallpapers/RoyalKing.png")
(start-process-shell-command "picom" nil "picom")
(start-process-shell-command "flameshot" nil "flameshot")
(start-process-shell-command "nextcloud" nil "nextcloud")
(start-process-shell-command "caffeine" nil "caffeine")
(start-process-shell-command "kdeconnect-indicator" nil "kdeconnect-indicator")

(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
(defun +ivy-posframe-display-exwm (str)
  (ivy-posframe--display str
   (lambda (info)
     (let* ((workarea (elt exwm-workspace--workareas exwm-workspace-current-index))
            (x (aref workarea 0))
            (y (aref workarea 1))

            (fw (aref workarea 2))
            (fh (aref workarea 3))

            (pw (plist-get info :posframe-width))
            (ph (plist-get info :posframe-height)))

       (cons (+ x (/ (- fw pw) 2)) (+ y (/ (- fh ph) 2)))))))

(setq ivy-posframe-display-functions-alist
      '((t . +ivy-posframe-display-exwm))

      ivy-posframe-parameters '((parent-frame nil)
                                (z-group . above)))

;; force set frame-position on every posframe display
(advice-add 'posframe--set-frame-position :before
            (lambda (&rest args)
              (setq-local posframe--last-posframe-pixel-position nil)))

(setq tramp-terminal-type "tramp")
