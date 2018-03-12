(packages:
  evil-anzu
  evil-args
  evil-cleverparens
  evil-ediff
  evil-escape
  evil-exchange
  evil-iedit-state
  evil-indent-plus
  evil-lion
  evil-lisp-state
  ;; for testing purpose, contribute by reporting bugs and sending PRs
  ;; to https://github.com/gabesoft/evil-mc
  ;; To enable it add `(global-evil-mc-mode)' to user-config function
  evil-mc
  evil-nerd-commenter
  evil-matchit
  evil-numbers
  evil-search-highlight-persist
  evil-surround
  ;; Temporarily disabled, pending the resolution of
  ;; https://github.com/7696122/evil-terminal-cursor-changer/issues/8
  ;; evil-terminal-cursor-changer
  evil-tutor
  (evil-unimpaired :location (recipe :fetcher local))
  evil-visual-mark-mode
  evil-visualstar
  (hs-minor-mode :location built-in)
  linum-relative
  vi-tilde-fringe
  )
