;;; app/notmuch/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun =email ()
  "Activate (or switch to) `notmuch' in its workspace (`mail')."
  (interactive)
  (unless (featurep! :feature workspaces)
    (user-error ":feature workspaces is required, but disabled"))
  (+workspace-switch "mail" t)
  (if-let* ((buf (cl-find-if (lambda (it) (string-match-p "^\\*notmuch" (buffer-name (window-buffer it))))
                             (doom-visible-windows))))
      (select-window (get-buffer-window buf)) (call-interactively 'notmuch))
  (+workspace/display))

;;;###autoload
(defun +mail/quit ()
  (interactive)
  (doom-kill-matching-buffers "^\\*notmuch")
  (+workspace/delete "mail"))
