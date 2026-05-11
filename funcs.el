;;; -*- lexical-binding: t -*-
;;; Codex layer functions — prefers vterm, falls back to ansi-term
(require 'cl-lib)
(require 'subr-x)

(defconst codex-packages
  '((projectile :location built-in)))

(defvar codex--last-buffer nil
  "Holds the buffer of the last Codex terminal session.")

(defcustom codex-file-context-large-file-size (* 256 1024)
  "File size in bytes after which the picker shows a large file badge."
  :type 'integer
  :group 'codex)

(defconst codex-file-context-ignored-directories
  '(".git" "node_modules" "dist" "build" "target" ".next" ".cache")
  "Directory names excluded from file context selection.")

(defconst codex-file-context-binary-extensions
  '("png" "jpg" "jpeg" "gif" "webp" "ico" "pdf" "zip" "gz" "tar" "jar"
    "war" "class" "dll" "exe" "so" "dylib" "o" "a" "elc" "pyc" "ttf"
    "otf" "woff" "woff2" "mp3" "mp4" "mov" "avi")
  "File extensions excluded from file context selection.")

(defvar codex--context-menu-history nil)
(defvar codex--file-context-history nil)
(defvar codex--minibuffer-back-triggered nil)
(defvar codex--minibuffer-delete-command nil)
(defconst codex--file-context-empty-state "<No workspace files match your search>"
  "Sentinel row shown when the picker has no file matches.")

(defvar-local codex--selected-context-files nil
  "Workspace-relative file paths already inserted into the current Codex buffer.")

(defvar-local codex--workspace-file-cache nil
  "Cached workspace file list for the current Codex buffer.")

(defvar-local codex--workspace-file-cache-root nil
  "Root directory used for `codex--workspace-file-cache'.")

(defvar codex-terminal-context-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "@") #'codex/context-menu-or-insert)
    map)
  "Keymap used to open the Codex context menu from terminal buffers.")

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

(define-minor-mode codex-terminal-context-mode
  "Enable `@' context selection inside Codex terminal buffers."
  :lighter " CodexCtx"
  :keymap codex-terminal-context-mode-map)

(defun codex/context-menu-option (label action)
  "Create a reusable context menu option with LABEL and ACTION."
  (cons label action))

(defconst codex--context-menu-options
  (list (codex/context-menu-option "Files" #'codex/file-context-picker))
  "Available providers for `codex/context-menu'.")

(defun codex--project-root (&optional directory)
  "Return the active project root for DIRECTORY or `default-directory'."
  (let ((default-directory (file-name-as-directory
                            (expand-file-name (or directory default-directory)))))
    (or (ignore-errors (projectile-project-root))
        default-directory)))

(defun codex--codex-terminal-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is a Codex terminal buffer."
  (with-current-buffer (or buffer (current-buffer))
    (and (or (derived-mode-p 'vterm-mode)
             (derived-mode-p 'term-mode))
         (string-match-p "Codex" (buffer-name)))))

(defun codex--terminal-send-self-insert (string)
  "Send STRING straight to the current terminal buffer."
  (codex--send-to-terminal (current-buffer) string))

(defun codex--context-file-absolute-path (root relative-path)
  "Expand RELATIVE-PATH against ROOT."
  (expand-file-name relative-path (file-name-as-directory root)))

(defun codex--workspace-file-ignored-p (relative-path)
  "Return non-nil when RELATIVE-PATH should be hidden from the picker."
  (let ((segments (split-string relative-path "/" t))
        (extension (downcase (or (file-name-extension relative-path) ""))))
    (or (string-prefix-p "../" relative-path)
        (member extension codex-file-context-binary-extensions)
        (string-match-p "\\(?:\\.min\\.[^.]+\\|\\.map\\)\\'" relative-path)
        (cl-some (lambda (segment)
                   (member segment codex-file-context-ignored-directories))
                 segments))))

(defun codex--git-workspace-files (root)
  "Return Git-tracked and unignored files for ROOT, or nil on failure."
  (let ((default-directory (file-name-as-directory root)))
    (when (and (executable-find "git")
               (zerop (call-process "git" nil nil nil "rev-parse" "--is-inside-work-tree")))
      (with-temp-buffer
        (when (zerop (call-process "git" nil t nil
                                   "ls-files" "--cached" "--others" "--exclude-standard"))
          (split-string (buffer-string) "\n" t))))))

(defun codex--filesystem-workspace-files (root)
  "Return regular files under ROOT."
  (mapcar (lambda (path)
            (file-relative-name path root))
          (directory-files-recursively root ".*" nil)))

(defun codex--collect-workspace-files (root)
  "Collect workspace files for ROOT, respecting ignore rules when possible."
  (let* ((root (file-name-as-directory root))
         (files (or (codex--git-workspace-files root)
                    (codex--filesystem-workspace-files root))))
    (cl-loop for relative-path in files
             for absolute-path = (codex--context-file-absolute-path root relative-path)
             unless (or (codex--workspace-file-ignored-p relative-path)
                        (not (file-regular-p absolute-path))
                        (not (file-in-directory-p absolute-path root)))
             collect relative-path)))

(defun codex--workspace-files (root)
  "Return cached workspace files for ROOT."
  (let ((root (file-name-as-directory root)))
    (if (and codex--workspace-file-cache
             (equal root codex--workspace-file-cache-root))
        codex--workspace-file-cache
      (message "🜏 Indexing workspace files...")
      (setq codex--workspace-file-cache-root root
            codex--workspace-file-cache
            (delete-dups (codex--collect-workspace-files root))))))

(defun codex--recent-workspace-files (root)
  "Return a hash table ranking recently opened or edited files within ROOT."
  (let ((table (make-hash-table :test 'equal))
        (rank 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and buffer-file-name
                   (file-in-directory-p buffer-file-name root))
          (puthash (file-relative-name buffer-file-name root)
                   rank
                   table)
          (setq rank (1+ rank)))))
    (when (bound-and-true-p recentf-list)
      (dolist (file recentf-list)
        (when (and (stringp file)
                   (file-in-directory-p file root)
                   (not (gethash (file-relative-name file root) table)))
          (puthash (file-relative-name file root) rank table)
          (setq rank (1+ rank)))))
    table))

(defun codex--active-workspace-file (root)
  "Return the most recent non-terminal file path within ROOT."
  (cl-loop for buffer in (buffer-list)
           for file = (buffer-local-value 'buffer-file-name buffer)
           when (and file
                     (file-in-directory-p file root)
                     (not (codex--codex-terminal-buffer-p buffer)))
           return file))

(defun codex--shared-path-prefix-length (left right)
  "Return the number of shared path segments between LEFT and RIGHT."
  (let ((left-segments (split-string (directory-file-name (or left "")) "/" t))
        (right-segments (split-string (directory-file-name (or right "")) "/" t))
        (matches 0))
    (while (and left-segments right-segments
                (string= (car left-segments) (car right-segments)))
      (setq matches (1+ matches)
            left-segments (cdr left-segments)
            right-segments (cdr right-segments)))
    matches))

(defun codex--nearby-file-score (relative-path active-file root)
  "Return a score for how near RELATIVE-PATH is to ACTIVE-FILE within ROOT."
  (if (not active-file)
      9999
    (let* ((active-relative (file-relative-name active-file root))
           (candidate-dir (file-name-directory relative-path))
           (active-dir (file-name-directory active-relative))
           (shared (codex--shared-path-prefix-length candidate-dir active-dir)))
      (- 999 shared))))

(defun codex--subsequence-score (needle haystack)
  "Return a fuzzy subsequence score for NEEDLE inside HAYSTACK, or nil."
  (catch 'missing
    (let ((start 0)
          (score 0))
      (dolist (char (string-to-list needle))
        (let ((position (cl-position char haystack :start start)))
          (if position
              (setq score (+ score position)
                    start (1+ position))
            (throw 'missing nil))))
      score)))

(defun codex--file-search-score (query relative-path recent-files active-file root)
  "Return a sortable score list for QUERY against RELATIVE-PATH."
  (let* ((query (downcase query))
         (basename (downcase (file-name-nondirectory relative-path)))
         (path (downcase relative-path))
         (exact-filename (if (and (not (string-empty-p query))
                                  (string= query basename))
                             0
                           1))
         (recent-rank (or (gethash relative-path recent-files) 9999))
         (nearby-rank (codex--nearby-file-score relative-path active-file root))
         (substring-rank (or (and (not (string-empty-p query))
                                  (string-match-p (regexp-quote query) path))
                             9999))
         (fuzzy-rank (or (if (string-empty-p query)
                             0
                           (codex--subsequence-score query path))
                         9999)))
    (list exact-filename recent-rank nearby-rank substring-rank fuzzy-rank
          (length relative-path) relative-path)))

(defun codex--file-search-match-p (query relative-path)
  "Return non-nil when QUERY matches RELATIVE-PATH."
  (or (string-empty-p query)
      (string-match-p (regexp-quote (downcase query))
                      (downcase relative-path))
      (codex--subsequence-score (downcase query) (downcase relative-path))))

(defun codex--sort-files-for-query (query files root)
  "Return FILES filtered and sorted for QUERY within ROOT."
  (let ((recent-files (codex--recent-workspace-files root))
        (active-file (codex--active-workspace-file root)))
    (sort (cl-remove-if-not
           (lambda (relative-path)
             (codex--file-search-match-p query relative-path))
           (copy-sequence files))
          (lambda (left right)
            (let ((left-score (codex--file-search-score query left recent-files active-file root))
                  (right-score (codex--file-search-score query right recent-files active-file root)))
              (cl-loop for left-part in left-score
                       for right-part in right-score
                       thereis (cond ((< left-part right-part) t)
                                     ((> left-part right-part) nil))))))))

(defun codex--selected-file-badge (relative-path)
  "Return a status badge for RELATIVE-PATH when one is needed."
  (let ((root (or codex--workspace-file-cache-root (codex--project-root))))
    (concat
     (when (member relative-path codex--selected-context-files)
       " [selected]")
     (let ((absolute-path (codex--context-file-absolute-path root relative-path)))
       (cond
        ((not (file-exists-p absolute-path))
         " [missing]")
        ((> (or (file-attribute-size (file-attributes absolute-path 'string)) 0)
            codex-file-context-large-file-size)
         " [large file]")
         (t ""))))))

(defun codex/file-context-token-valid-p (relative-path &optional root)
  "Return non-nil when RELATIVE-PATH still exists inside ROOT."
  (let* ((root (or root codex--workspace-file-cache-root (codex--project-root)))
         (absolute-path (codex--context-file-absolute-path root relative-path)))
    (and (file-in-directory-p absolute-path root)
         (file-exists-p absolute-path))))

(defun codex--file-context-annotation (candidate)
  "Return minibuffer annotation for file context CANDIDATE."
  (format "  %s%s"
          (or (file-name-directory candidate) "./")
          (codex--selected-file-badge candidate)))

(defun codex--minibuffer-handle-delete ()
  "Return to the previous menu when backspacing from an empty minibuffer."
  (interactive)
  (if (string-empty-p (minibuffer-contents-no-properties))
      (progn
        (setq codex--minibuffer-back-triggered t)
        (abort-recursive-edit))
    (call-interactively (or codex--minibuffer-delete-command
                            #'backward-delete-char-untabify))))

(defun codex--completing-read-with-back (prompt collection &optional history)
  "Like `completing-read', but empty backspace returns :back."
  (let ((codex--minibuffer-back-triggered nil)
        (codex--minibuffer-delete-command nil))
    (condition-case nil
        (minibuffer-with-setup-hook
            (lambda ()
              (setq codex--minibuffer-delete-command
                    (or (key-binding (kbd "DEL"))
                        #'backward-delete-char-untabify))
              (local-set-key (kbd "DEL") #'codex--minibuffer-handle-delete)
              (local-set-key (kbd "<backspace>") #'codex--minibuffer-handle-delete))
          (completing-read prompt collection nil t nil history))
      (quit (if codex--minibuffer-back-triggered :back nil)))))

(defun codex/file-context-token (relative-path)
  "Return the inline Codex file token for RELATIVE-PATH."
  (let ((safe-path (if (string-match-p "[^[:alnum:]/._-]" relative-path)
                       (format "\"%s\""
                               (replace-regexp-in-string
                                "\""
                                "\\\\\""
                                (replace-regexp-in-string "\\\\" "\\\\\\\\" relative-path t t)
                                t t))
                     relative-path)))
    (format "@%s" safe-path)))

(defun codex--insert-file-context-token (relative-path)
  "Insert RELATIVE-PATH into the current Codex terminal as a file token."
  (let ((token (codex/file-context-token relative-path)))
    (unless (member relative-path codex--selected-context-files)
      (push relative-path codex--selected-context-files))
    (codex--terminal-send-self-insert (concat token " "))
    (message "🜏 Added %s to the next Codex request." relative-path)))

(defun codex--file-picker-collection (files root)
  "Return a completion table for FILES in ROOT."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata
          (category . file)
          (annotation-function . codex--file-context-annotation))
      (complete-with-action action
                            (or (codex--sort-files-for-query string files root)
                                (list codex--file-context-empty-state))
                            string
                            predicate))))

(defun codex--read-file-context (root files)
  "Read a file selection from FILES under ROOT."
  (let ((completion-extra-properties
         '(:annotation-function codex--file-context-annotation)))
    (codex--completing-read-with-back
     "Files: "
     (codex--file-picker-collection files root)
     'codex--file-context-history)))

(defun codex/file-context-picker ()
  "Open a searchable workspace file picker and insert selected file tokens."
  (interactive)
  (let* ((root (codex--project-root))
         (files (codex--workspace-files root)))
    (unless files
      (user-error "No workspace files indexed yet. Wait for indexing and try again"))
    (catch 'done
      (while t
        (let ((selection (codex--read-file-context root files)))
          (cond
           ((eq selection :back)
            (throw 'done :back))
           ((or (null selection)
                (string-empty-p selection))
            (throw 'done nil))
           ((string= selection codex--file-context-empty-state)
            (message "🜏 No workspace files match that search."))
           ((member selection codex--selected-context-files)
            (message "🜏 %s is already attached to this prompt." selection))
           (t
            (codex--insert-file-context-token selection))))))))

(defun codex/context-menu ()
  "Open the Codex context picker anchored to the current terminal input."
  (interactive)
  (let* ((options (mapcar #'car codex--context-menu-options))
         (choice (completing-read "Context: " options nil t nil 'codex--context-menu-history)))
    (when-let ((action (cdr (assoc choice codex--context-menu-options))))
      (when (eq (funcall action) :back)
        (codex/context-menu)))))

(defun codex/context-menu-or-insert ()
  "Open the Codex context menu, or insert a literal @ outside Codex buffers."
  (interactive)
  (if (codex--codex-terminal-buffer-p)
      (condition-case nil
          (codex/context-menu)
        (quit (codex--terminal-send-self-insert "@")))
    (codex--terminal-send-self-insert "@")))

(defun codex--enable-terminal-context-menu ()
  "Enable Codex context menu bindings in the current terminal buffer."
  (setq-local codex--selected-context-files nil
              codex--workspace-file-cache nil
              codex--workspace-file-cache-root nil)
  (codex-terminal-context-mode 1))

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
            (codex--enable-terminal-context-menu)
            (vterm-send-string "codex")
            (vterm-send-return))
          (select-window (get-buffer-window buf))
          (when (bound-and-true-p evil-local-mode)
            (evil-insert-state)))
      ;; ansi-term fallback
      (let ((buf (ansi-term "/bin/bash" term-name)))
        (setq codex--last-buffer buf)
        (with-current-buffer buf
          (codex--enable-terminal-context-menu)
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
(add-hook 'term-mode-hook #'codex--enable-terminal-context-menu)

;; For vterm, if installed
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "@") #'codex/context-menu-or-insert)
  (define-key vterm-mode-map [escape]
              (lambda () (interactive) (vterm-send-key "<escape>"))))

(with-eval-after-load 'term
  (define-key term-raw-map (kbd "@") #'codex/context-menu-or-insert))
