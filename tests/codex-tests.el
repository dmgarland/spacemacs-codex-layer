(require 'ert)
(require 'term)

(load-file
 (expand-file-name "../funcs.el"
                   (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest codex-test-at-key-opens-context-menu ()
  (should (eq (lookup-key codex-terminal-context-mode-map (kbd "@"))
              #'codex/context-menu-or-insert)))

(ert-deftest codex-test-file-context-token-quotes-spaces ()
  (should (equal (codex/file-context-token "src/My File.ts")
                 "@\"src/My File.ts\"")))

(ert-deftest codex-test-fuzzy-search-prefers-exact-filename-match ()
  (let* ((root (make-temp-file "codex-project" t))
         (files '("src/Button.tsx"
                  "src/button-helper.tsx"
                  "docs/button-guide.md")))
    (should (equal (car (codex--sort-files-for-query "Button.tsx" files root))
                   "src/Button.tsx"))))

(ert-deftest codex-test-collect-workspace-files-excludes-ignored-paths ()
  (let ((root (make-temp-file "codex-project" t)))
    (dolist (path '("src/app.ts"
                    "node_modules/pkg/index.js"
                    "build/output.js"
                    "images/logo.png"))
      (make-directory (file-name-directory (expand-file-name path root)) t)
      (with-temp-file (expand-file-name path root)
        (insert "test"))))
    (let ((files (codex--collect-workspace-files root)))
      (should (member "src/app.ts" files))
      (should-not (member "node_modules/pkg/index.js" files))
      (should-not (member "build/output.js" files))
      (should-not (member "images/logo.png" files))))

(ert-deftest codex-test-gitignore-files-are-excluded-when-git-is-available ()
  (skip-unless (executable-find "git"))
  (let ((root (make-temp-file "codex-project" t)))
    (should (zerop (call-process "git" nil nil nil "-C" root "init" "-q")))
    (with-temp-file (expand-file-name ".gitignore" root)
      (insert "ignored.js\n"))
    (with-temp-file (expand-file-name "ignored.js" root)
      (insert "ignored"))
    (with-temp-file (expand-file-name "kept.js" root)
      (insert "kept"))
    (let ((files (codex--collect-workspace-files root)))
      (should (member "kept.js" files))
      (should-not (member "ignored.js" files)))))

(ert-deftest codex-test-duplicate-filenames-remain-distinct ()
  (let* ((root (make-temp-file "codex-project" t))
         (files '("src/Button.tsx" "tests/Button.tsx")))
    (should (equal (codex--sort-files-for-query "Button" files root)
                   '("src/Button.tsx" "tests/Button.tsx")))))

(ert-deftest codex-test-deleted-token-is-reported-invalid ()
  (let* ((root (make-temp-file "codex-project" t))
         (path "src/orphan.ts")
         (absolute-path (expand-file-name path root)))
    (make-directory (file-name-directory absolute-path) t)
    (with-temp-file absolute-path
      (insert "export const orphan = true;\n"))
    (delete-file absolute-path)
    (should-not (codex/file-context-token-valid-p path root))))

(ert-deftest codex-test-multiple-selection-does-not-send-duplicates ()
  (let ((sent nil)
        (codex--selected-context-files nil))
    (cl-letf (((symbol-function 'codex--project-root)
               (lambda (&optional _directory) "/tmp/project/"))
              ((symbol-function 'codex--workspace-files)
               (lambda (_root) '("src/app.ts" "src/other.ts")))
              ((symbol-function 'codex--read-file-context)
               (let ((selections '("src/app.ts" "src/app.ts" nil)))
                 (lambda (_root _files)
                   (prog1 (car selections)
                     (setq selections (cdr selections))))))
              ((symbol-function 'codex--terminal-send-self-insert)
               (lambda (string)
                 (push string sent))))
      (codex/file-context-picker)
      (should (equal sent '("@src/app.ts ")))
      (should (equal codex--selected-context-files '("src/app.ts"))))))
