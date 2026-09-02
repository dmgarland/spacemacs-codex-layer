;;; -*- lexical-binding: t; -*-

(defconst codex-packages
  '((projectile :location built-in)
    (vterm :location
           (recipe :fetcher github
                   :repo "akermu/emacs-libvterm"
                   :files ("*")))

    (ansi-term :location built-in)))

(defun codex/init-codex ()
  "Initialise the Codex layer."
  (use-package codex
    :defer t))

