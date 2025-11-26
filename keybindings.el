(spacemacs/declare-prefix "a c" "codex")

(spacemacs/set-leader-keys
  "a c !" 'codex/run-in-project-root     ;; full project run, modifies files
  "a c r" 'codex/send-region-to-codex    ;; send region to codex
  "a c f" 'codex/send-current-file-path  ;; send pwd to codex e.g home/user/foo
  "a c F" 'codex/send-current-file-and-line ;; e.g. /home/user/foo:31
  "a c x" 'codex/run-command             ;; arbitrary CLI invocation
  "a c ." 'codex/run-here)                ;; run in current dir

