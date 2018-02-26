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
  (defun my-buffer-face-mode-notmuch-show ()
    "Sets a fixed width (monospace) font in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "Charter" :height 1.2))
    (buffer-face-mode)
    (setq-local line-spacing 0.5))
  (defun my-buffer-face-mode-notmuch ()
    "Sets a fixed width (monospace) font in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "SF Mono" :height 1.0))
    (buffer-face-mode)
    (setq-local line-spacing 0.2))
  (set! :evil-state 'notmuch-hello-mode 'normal)
  (set! :evil-state 'notmuch-show-mode 'normal)
  (set! :evil-state 'notmuch-search-mode 'normal)
  (set! :evil-state 'notmuch-tree-mode 'normal)
  (set! :evil-state 'notmuch-message-mode 'normal)
  (add-hook 'notmuch-show-mode-hook 'my-buffer-face-mode-notmuch-show)
  (add-hook 'notmuch-tree-mode-hook 'my-buffer-face-mode-notmuch)
  (add-hook 'notmuch-search-mode-hook 'my-buffer-face-mode-notmuch)
  (add-hook 'notmuch-message-mode-hook 'my-buffer-face-mode-notmuch)
  (add-hook 'notmuch-message-mode-hook (lambda () (set (make-local-variable 'company-backends) '(notmuch-company (company-ispell :with company-yasnippet)))))
  (add-hook 'notmuch-tree-mode-hook (lambda () (setq-local line-spacing nil)))
  (remove-hook 'message-mode-hook #'turn-on-auto-fill)
  (remove-hook 'notmuch-message-mode-hook #'turn-on-auto-fill)
  (push 'notmuch-tree-mode evil-snipe-disabled-modes)
  (push 'notmuch-hello-mode evil-snipe-disabled-modes)
  (push 'notmuch-search-mode evil-snipe-disabled-modes)
  (push 'notmuch-show-mode evil-snipe-disabled-modes)

;;;;; Add org-notmuch for referencing emails
  (require 'org-notmuch)

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
