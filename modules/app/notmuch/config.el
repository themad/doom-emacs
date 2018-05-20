;;; app/notmuch/config.el -*- lexical-binding: t; -*-

(def-package! notmuch
  :commands (notmuch
             notmuch-tree
             notmuch-tree-mode
             notmuch-search
             notmuch-search-mode
             notmuch-hello
             notmuch-hello-mode
             notmuch-show
             notmuch-show-mode
             notmuch-message-mode)
  :config
  (defun notmuch-show-delete-message-then-next ()
    "Deletes this message, then move on"
    (interactive)
    (notmuch-show-tag (list "+deleted"))
    (notmuch-show-archive-message-then-next-or-exit))
  (defun notmuch-show-kill-message-then-next ()
    "Adds the kill tag to this message, then move on"
    (interactive)
    (notmuch-show-tag (list "+kill"))
    (notmuch-show-archive-message-then-next-or-exit))
  (defun notmuch-search-kill-thread ()
    "Adds the kill tag to this message, then move on"
    (interactive)
    (notmuch-search-add-tag (list "+kill"))
    (notmuch-search-archive-thread))
  (defun notmuch-search-delete-thread ()
    "Adds the deleted tag to this message, then move on"
    (interactive)
    (notmuch-search-add-tag (list "+deleted"))
    (notmuch-search-archive-thread))

  (set! :evil-state 'notmuch-hello-mode 'normal)
  (set! :evil-state 'notmuch-show-mode 'normal)
  (set! :evil-state 'notmuch-search-mode 'normal)
  (set! :evil-state 'notmuch-tree-mode 'normal)
  (set! :evil-state 'notmuch-message-mode 'normal)
  (add-hook 'notmuch-message-mode-hook (lambda () (set (make-local-variable 'company-backends) '(notmuch-company (company-ispell :with company-yasnippet)))))
  (add-hook 'notmuch-tree-mode-hook (lambda () (setq-local line-spacing nil)))
  (remove-hook 'message-mode-hook #'turn-on-auto-fill)
  (remove-hook 'notmuch-message-mode-hook #'turn-on-auto-fill)

  (map!
         (:map notmuch-show-mode-map
          :nmv "o"     #'ace-link-notmuch-show
          :nmv "a"     #'notmuch-show-archive-message-then-next-or-next-thread
          :nmv "A"     #'notmuch-show-archive-thread-then-exit
          :nmv "d"     #'notmuch-show-delete-message-then-next
          :nmv "K"     #'notmuch-show-kill-thread-then-next
          :nmv "i"     #'open-message-with-mail-app-notmuch-show
          :nmv "I"     #'notmuch-show-view-all-mime-parts
          :nmv "q"     #'notmuch-bury-or-kill-this-buffer
          :nmv "s"     #'counsel-notmuch
          :nmv "t"     #'notmuch-tree-from-show-current-query
          :nmv "s-n"   #'notmuch-mua-new-mail
          :nmv "n"     #'notmuch-show-next-thread-show
          :nmv "v"     #'notmuch-show-view-raw-message
          :nmv "r"     #'notmuch-show-reply
          :nmv "<tab>" #'notmuch-show-toggle-visibility-headers
          :nmv "+"     #'notmuch-show-add-tag
          :nmv "-"     #'notmuch-show-remove-tag
          :nmv "R"     #'notmuch-show-reply-sender
          :nmv "p"     #'notmuch-show-previous-thread-show)
        (:map notmuch-hello-mode-map
          :nmv "J"   #'notmuch-jump-search
          :nmv "o"   #'ace-link-notmuch-hello
          :nmv "t"   #'notmuch-tree
          :nmv "k"   #'widget-backward
          :nmv "n"   #'notmuch-mua-new-mail
          :nmv "s-n" #'notmuch-mua-new-mail
          :nmv "j"   #'widget-forward
          :nmv "s"   #'counsel-notmuch
          :nmv "q"   #'+mail/quit
          :nmv "e"   #'notmuch-update
          :nmv "r"   #'notmuch-hello-update)
        (:map notmuch-search-mode-map
          :nmv "gg"  #'notmuch-search-first-thread
          :nmv "G"   #'notmuch-search-last-thread
          :nmv "j"   #'notmuch-search-next-thread
          :nmv "J"   #'notmuch-jump-search
          :nmv "k"   #'notmuch-search-previous-thread
          :nmv "t"   #'notmuch-tree-from-search-thread
          :nmv "RET" #'notmuch-search-show-thread
          :nmv "s-n" #'notmuch-mua-new-mail
          :nmv "T"   #'notmuch-tree-from-search-current-query
          :nmv ";"   #'notmuch-search-tag
          :nmv "d"   #'notmuch-search-delete-thread
          :nmv "a"   #'notmuch-search-archive-thread
          :nmv "q"   #'notmuch
          :nmv "R"   #'notmuch-search-reply-to-thread-sender
          :nmv "r"   #'notmuch-search-reply-to-thread
          :nmv "s"   #'counsel-notmuch
          :nmv "="   #'notmuch-search-refresh-view
          :nmv "x"   #'notmuch-search-spam)
          :nmv "+"   #'notmuch-search-add-tag
          :nmv "-"   #'notmuch-search-remove-tag
        (:map notmuch-tree-mode-map
          :nmv "j"   #'notmuch-tree-next-message
          :nmv "k"   #'notmuch-tree-prev-message
          :nmv "S"   #'notmuch-search-from-tree-current-query
          :nmv "s"   #'counsel-notmuch
          :nmv "t"   #'notmuch-tree
          :nmv ";"   #'notmuch-tree-tag
          :nmv "RET" #'notmuch-tree-show-message
          :nmv "q"   #'notmuch-tree-quit
          :nmv "s-n" #'notmuch-mua-new-mail
          :nmv "r"   #'notmuch-search-reply-to-thread-sender
          :nmv "a"   #'notmuch-tree-archive-message-then-next
          :nmv "A"   #'notmuch-tree-archive-thread
          :nmv "i"   #'open-message-with-mail-app-notmuch-tree
          :nmv "d"   #'notmuch-tree-delete
          :nmv "x"   #'notmuch-tree-spam)
          :nmv "+"   #'notmuch-tree-add-tag
          :nmv "-"   #'notmuch-tree-remove-tag
        (:map notmuch-message-mode-map
          :localleader
          :desc "Send and Exit"       :n doom-localleader-key #'notmuch-mua-send-and-exit
          :desc "Kill Message Buffer" :n "k" #'notmuch-mua-kill-buffer
          :desc "Save as Draft"       :n "s" #'message-dont-send
	  :desc "Attach file" :n "f" #'mml-attach-file))

  (setq message-kill-buffer-on-exit t)

;;;;; Add org-notmuch for referencing emails
  (when (featurep! :feature org)
    (require 'org-notmuch))

;;;;; Notmuch-avy
  (require 'avy)
  (defun ace-link--notmuch-hello-collect ()
    "Collect the positions of visible links in *notmuch-hello*."
    (let (candidates pt)
      (save-excursion
        (save-restriction
          (goto-char (point-min))
          (setq pt (point))
          (while (progn (widget-forward 1)
                        (> (point) pt))
            (setq pt (point))
            (when (get-char-property (point) 'button)
              (push (point) candidates)))))
      (nreverse candidates)))
  (defun ace-link--notmuch-hello-action (pt)
    (when (number-or-marker-p pt)
      (goto-char (1+ pt))
      (widget-button-press (point))))
  (defun ace-link-notmuch-hello ()
    "Open a visible link in *notmuch-hello*."
    (interactive)
    (let ((pt (avy-with ace-link-notmuch-hello
                (avy--process
                 (ace-link--notmuch-hello-collect)
                 #'avy--overlay-pre))))
      (ace-link--notmuch-hello-action pt)))
  (defun ace-link--notmuch-show-collect ()
    "Collect the positions of visible links in `notmuch-show' buffer."
    (let (candidates pt)
      (save-excursion
        (save-restriction
          (narrow-to-region
           (window-start)
           (window-end))
          (goto-char (point-min))
          (while (re-search-forward "https?://" nil t)
            (setq pt (- (point) (length (match-string 0))))
            (push pt candidates))))
      (nreverse candidates)))
  (defun ace-link--notmuch-show-action  (pt)
    (goto-char pt)
    (browse-url-at-point))
  (defun ace-link-notmuch-show ()
    "Open a visible link in `notmuch-show' buffer."
    (interactive)
    (let ((pt (avy-with ace-link-notmuch-show
                (avy--process
                 (ace-link--notmuch-show-collect)
                 #'avy--overlay-pre))))
      (ace-link--notmuch-show-action pt)))
  )

;;;; counsel-notmuch
(def-package! counsel-notmuch
  :commands counsel-notmuch
  :after notmuch)
;;;; org-mime
(def-package! org-mime
  :after (org notmuch)
  :config (setq org-mime-library 'mml))

(after! evil-snipe
  (push 'notmuch-tree-mode evil-snipe-disabled-modes)
  (push 'notmuch-hello-mode evil-snipe-disabled-modes)
  (push 'notmuch-search-mode evil-snipe-disabled-modes)
  (push 'notmuch-show-mode evil-snipe-disabled-modes))
