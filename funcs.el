;;; -*- lexical-binding: t -*-
;;; Codex layer functions — prefers vterm, falls back to ansi-term
(defconst codex-packages
  '((projectile :location built-in)))

(defvar codex--last-buffer nil
  "Holds the buffer of the last Codex terminal session.")

;;------------------------------------------------------------
;;  Internal utility
;;------------------------------------------------------------
(defun codex/vterm-available-p ()
  "Return non-nil if vterm is available and functional."
  (and (require 'vterm nil t)
       (fboundp 'vterm)
       (ignore-errors
         (with-temp-buffer
           (vterm-mode)
           t))))

(defun codex--display-in-side-window (buffer)
  "Display BUFFER in a side window at the right, not stealing focus."
  (display-buffer-in-side-window
   buffer
   '((side . right)
     (slot . 0)
     (window-height . 0.25)
     (window-parameters . ((no-delete-other-windows . t)
                           (no-other-window . t))))))

;;------------------------------------------------------------
;;  Core commands
;;------------------------------------------------------------
(defun codex/run-in-project-root ()
  "Run `codex .` in an interactive terminal inside Emacs.
Uses vterm if available, else falls back to ansi-term."
  (interactive)
  (let* ((root (or (projectile-project-root) default-directory))
         (proj (file-name-nondirectory (directory-file-name root)))
         (term-name (format "Codex: %s" proj))
         (default-directory root))
    (message "🜏 Launching Codex terminal in %s" root)
    (if (codex/vterm-available-p)
        ;; vterm branch
        (let ((buf (vterm term-name)))
          (setq codex--last-buffer buf)
          (with-current-buffer buf
            (vterm-send-string "codex")
            (vterm-send-return))
          (select-window (get-buffer-window buf))
          (when (bound-and-true-p evil-local-mode)
            (evil-insert-state)))
      ;; ansi-term fallback
      (let ((buf (ansi-term "/bin/bash" term-name)))
        (setq codex--last-buffer buf)
        (with-current-buffer buf
          (run-with-timer
           0.2 nil
           (lambda ()
             (when (get-buffer-process buf)
               (term-send-raw-string "codex .\n")))))
        (select-window (get-buffer-window buf))
        (when (bound-and-true-p evil-local-mode)
          (evil-insert-state))))))

(defun codex/get-buffer ()
  "Return the live Codex terminal buffer, or error if none."
  (when (and codex--last-buffer
             (not (get-buffer-process codex--last-buffer)))
    (setq codex--last-buffer nil))
  (or (and codex--last-buffer
           (buffer-live-p codex--last-buffer)
           (get-buffer-process codex--last-buffer)
           codex--last-buffer)
      (cl-find-if
       (lambda (buf)
         (with-current-buffer buf
           (and (string-match-p "Codex" (buffer-name buf))
                (or (derived-mode-p 'vterm-mode)
                    (derived-mode-p 'term-mode))
                (get-buffer-process buf))))
       (buffer-list))
      (user-error "No active Codex terminal session found (SPC a c ! first)")))

;;------------------------------------------------------------
;;  Sending helpers (work for both vterm and ansi-term)
;;------------------------------------------------------------
(defun codex--send-to-terminal (buf string)
  "Send STRING to Codex BUF using the appropriate terminal backend."
  (with-current-buffer buf
    (cond ((derived-mode-p 'vterm-mode)
           (vterm-send-string string)
           )
          ((derived-mode-p 'term-mode) (term-send-raw-string string ))
          (t (error "Buffer %s is not a recognised terminal" (buffer-name buf))))))

(defun codex/send-region-to-codex (beg end)
  "Send the selected region to Codex, focus window, enter insert mode."
  (interactive "r")
  (let* ((region (buffer-substring-no-properties beg end))
         (codex-buf (codex/get-buffer))
         (codex-win (get-buffer-window codex-buf t)))
    (codex--send-to-terminal codex-buf region)
    (when codex-win (select-window codex-win))
    (with-current-buffer codex-buf
      (if (bound-and-true-p evil-local-mode)
          (evil-insert-state)
        (when (derived-mode-p 'term-mode)
          (term-char-mode))))
    (message "🜏 Sent region (%d chars) to Codex and focused terminal." (length region))))

(defun codex/send-current-file-path ()
  "Send current file path to Codex, focus terminal, enter insert mode."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer isn’t visiting a file"))
  (let* ((path (expand-file-name buffer-file-name))
         (codex-buf (codex/get-buffer))
         (codex-win (get-buffer-window codex-buf t)))
    (codex--send-to-terminal codex-buf (format "in this file: %s" path))
    (when codex-win (select-window codex-win))
    (with-current-buffer codex-buf
      (if (bound-and-true-p evil-local-mode)
          (evil-insert-state)
        (when (derived-mode-p 'term-mode)
          (term-char-mode))))
    (message "🜏 Sent path to Codex and focused terminal.")))

(defun codex/send-current-file-and-line ()
  "Send current file path and line number to Codex, focus terminal, enter insert mode."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer isn’t visiting a file"))
  (let* ((path (expand-file-name buffer-file-name))
         (line (line-number-at-pos))
         (codex-buf (codex/get-buffer))
         (codex-win (get-buffer-window codex-buf t)))
    (codex--send-to-terminal codex-buf (format "in this file on line: %s:%d" path line))
    (when codex-win (select-window codex-win))
    (with-current-buffer codex-buf
      (if (bound-and-true-p evil-local-mode)
          (evil-insert-state)
        (when (derived-mode-p 'term-mode)
          (term-char-mode))))
    (message "🜏 Sent path+line to Codex and focused terminal.")))

;;------------------------------------------------------------
;;  Key behaviour inside Codex terminals
;;------------------------------------------------------------

(defun codex/term-allow-escape ()
  "Allow raw Escape to reach the Codex subprocess in ansi-term."
  (when (derived-mode-p 'term-mode)
    ;; send literal ESC instead of exiting evil state
    (define-key term-raw-map (kbd "<escape>")
                (lambda () (interactive) (term-send-raw-string "\e")))))

(add-hook 'term-mode-hook #'codex/term-allow-escape)

;; For vterm, if installed
(with-eval-after-load 'vterm
  (define-key vterm-mode-map [escape]
              (lambda () (interactive) (vterm-send-key "<escape>"))))
