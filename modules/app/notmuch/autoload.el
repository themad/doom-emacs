;;; app/notmuch/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun notmuch ()
  "Opens a workspace dedicated to `notmuch.'"
  (interactive)

  (progn
    (+workspace/new "notmuch")
    (call-interactively #'notmuch)))
