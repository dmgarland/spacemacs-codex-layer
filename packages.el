(defconst codex-packages
  '((projectile :location built-in)
    (vterm :location (recipe :fetcher github :repo "akermu/emacs-libvterm"))
    (ansi-term :location built-in)))

(defun codex/init-codex ()
  "Initialise the Codex layer."
  (use-package codex
    :defer t))

