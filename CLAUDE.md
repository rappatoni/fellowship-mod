@AGENTS.md

# Claude Code and Emacs

When operating through `claude-code-ide`, use active-buffer, selected-region,
diagnostic, compilation, and project context only as immediate working context.
Do not treat editor context as evidence that a repository task is complete.

Do not use unrestricted Elisp evaluation. Prefer narrow, named, reviewable
Emacs tools. Continuous focus monitoring belongs to Emacs/Org, not to an
autonomous Claude loop.
