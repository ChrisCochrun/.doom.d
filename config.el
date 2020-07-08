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

(setq doom-font (font-spec :family "VictorMono Nerd Font" :size 12.0 :weight 'semi-bold)
      doom-variable-pitch-font (font-spec :family "VictorMono Nerd Font" :size 13.0 :weight 'semi-bold))

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

(setq
 all-the-icons-scale-factor 0.8
 doom-modeline-height 30
 doom-modeline-major-mode-icon t
 doom-modeline-major-mode-color-icon t
 doom-modeline-mu4e t
 doom-modeline-bar-width 3)

;; EXWM
;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-default)
;; (require 'exwm-randr)
;; (setq exwm-randr-workspace-monitor-plist '(0 "DVI-D-0" 1 "HDMI-0"))
;; (add-hook! 'exwm-randr-screen-change-hook
;;   (lambda ()
;;     (start-process-shell-command
;;      "xrandr" nil "xrandr --output DVI-D-0 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-0 --mode 1600x900 --pos 1920x0 --rotate normal")))
;; (exwm-randr-enable)
;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)
;; (setq exwm-systemtray-height 20)
;; (setq exwm-workspace-number 4)

;; Global keybindings
;; (setq exwm-input-global-keys
;;           `(
;;             ;; 's-r': Reset (to line-mode).
;;             ([?\s-r] . exwm-reset)
;;             ;; 's-w': Switch workspace.
;;             ([?\s-w] . exwm-workspace-switch)
;;             ;; 's-&': Launch application.
;;             ([?\s-&] . (lambda (command)
;;                          (interactive (list (read-shell-command "$ ")))
;;                          (start-process-shell-command command nil command)))
;;             ([?\s-r] . (start-process-shell-command "rofi" nil "/home/chris/.dotfiles/rofi/launchers-git/launcher.sh"))
;;             ;; 's-N': Switch to certain workspace.
;;             ,@(mapcar (lambda (i)
;;                         `(,(kbd (format "s-%d" i)) .
;;                           (lambda ()
;;                             (interactive)
;;                             (exwm-workspace-switch-create ,i))))
;;                       (number-sequence 0 9))))

;; (add-hook! exwm-init-hook (start-process-shell-command "nextcloud" nil "nextcloud"))
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

(use-package! org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(add-hook! org-roam-mode org-roam-server-mode t)
;; Window movements TODO

;; Face edits TODO

;; QT/QML
;; Ensure qml is added to the completion engine company
(add-to-list 'company-backends 'company-qml)

(setq company-qml-extra-qmltypes-files '("/home/chris/.Felgo/Felgo/gcc_64/import/VPlayPlugins/vplayplugins.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/import/VPlayApps/vplayapps.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/import/VPlay/vplay.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/import/Felgo/felgo.qmltypes"
                                         "/home/chris/.Felgo/Felgo/gcc_64/qml"))

;; org
(setq org-superstar-headline-bullets-list '("◉" "◈" "▸" "◎" "✬" "◇" "❉" "✙" "❖"))
(setq olivetti-body-width 0.6)
(add-hook! org-mode (setq hl-line-mode nil))

(add-hook! 'org-mode-hook (lambda () (imenu-add-to-menubar "Imenu")))

(add-hook! org-mode (olivetti-mode t))
(add-hook! org-mode (org-autolist-mode t))

;; (defun org-yt-follow-mpv (video-id)
;;   "Open youtube with VIDEO-ID."
;;   (async-shell-command (format "mpv %s" (concat "https://youtu.be/" video-id)))
;;   )

;; (map! :map org-mode-map
;;       :n "M-v" 'org-yt-follow-mpv)

(setq deft-directory "~/org/")

;; Email
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

;; Add gmail
(set-email-account! "gmail"
  '((mu4e-sent-folder       . "/gmail/[Gmail].Sent Mail/")
    (smtpmail-smtp-user     . "ccochrun21@gmail.com")
    (user-mail-address      . "ccochrun21@gmail.com")    ;; only needed for mu < 1.4
    (mu4e-compose-signature . "---\nChris Cochrun"))
  nil)

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
     (:maildir "/office/Junk Email"             :key ?j)
     (:maildir "/office/INBOX/Website Forms"    :key ?f)
     (:maildir "/gmail/INBOX"                   :key ?g)
     (:maildir "/office/sent"                   :key ?s)))

;; elfeed
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

;; mapping the v key to launch mpv
(map! :map elfeed-search-mode-map
      :n "v" 'elfeed-view-mpv)

;; Tramp for ZSH
;; Needed to allow me to remote to servers using zsh as the main shell
(setq tramp-terminal-type "tramp")

;; org-super-agenda
;; (use-package! org-super-agenda
;;   :init
;;   (setq org-super-agenda-groups '((:name "Today"
;;                                          :time-grid t
;;                                          :scheduled today)
;;                                   (:name "Due Today"
;;                                          :deadline today)
;;                                   (:name "Important"
;;                                          :priority "A")
;;                                   (:name "Overdue"
;;                                          :time-grid t
;;                                          :scheduled today)
;;                                   (:name "Due soon"
;;                                          :deadline future)))
;;   :config
;;   (org-super-agenda-mode)
;;   :after org-agenda)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/home/chris/org/newsletter.org" "/home/chris/org/2020-05-01.org" "/home/chris/org/2020-05-02.org" "/home/chris/org/2020-06-05.org" "/home/chris/org/2020-06-08.org" "/home/chris/org/2020-06-11.org" "/home/chris/org/2020-06-12.org" "/home/chris/org/2020-06-13.org" "/home/chris/org/2020-06-14.org" "/home/chris/org/2020-06-15.org" "/home/chris/org/2020-06-16.org" "/home/chris/org/2020-06-22.org" "/home/chris/org/2020-06-25.org" "/home/chris/org/2020-06-26.org" "/home/chris/org/2020-06-29.org" "/home/chris/org/20200501140836-april_2020.org" "/home/chris/org/20200501160749-newsletters.org" "/home/chris/org/20200503065300-2020_05_03.org" "/home/chris/org/20200503065920-sinful.org" "/home/chris/org/20200504053204-2020_05_04.org" "/home/chris/org/20200504171942-homework_for_life.org" "/home/chris/org/20200505061606-2020_05_05.org" "/home/chris/org/20200506063547-god_s_wrath.org" "/home/chris/org/20200506065200-celebration_before_god.org" "/home/chris/org/20200506070605-solomon.org" "/home/chris/org/20200506072644-2020_05_06.org" "/home/chris/org/20200507072922-for_your_name_s_sake.org" "/home/chris/org/20200507112804-2020_05_07.org" "/home/chris/org/20200508202941-2020_05_08.org" "/home/chris/org/20200508210627-stars_and_heavens.org" "/home/chris/org/20200514102114-prayers.org" "/home/chris/org/20200528210123-2020_05_28.org" "/home/chris/org/20200529164304-dmwf_presentation.org" "/home/chris/org/20200530205937-2020_05_30.org" "/home/chris/org/20200601134541-2020_06_01.org" "/home/chris/org/2020_05_14-20200514220919.org" "/home/chris/org/DMPREADME.org" "/home/chris/org/DMPTODO.org" "/home/chris/org/InductiveBibleStudy.org" "/home/chris/org/Summer Ideas.org" "/home/chris/org/archive.org" "/home/chris/org/dmapp.org" "/home/chris/org/elfeed.org" "/home/chris/org/esv.org" "/home/chris/org/fix_my_eyes-20200514141622.org" "/home/chris/org/fool.org" "/home/chris/org/inbox.org" "/home/chris/org/intro_to_the_hebrew_bible.org" "/home/chris/org/nasb.org" "/home/chris/org/notes.org" "/home/chris/org/repetition.org" "/home/chris/org/tasks.org" "/home/chris/org/todo.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
