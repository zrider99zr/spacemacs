(packages:
  async
  bind-map
  bind-key
  diminish
  evil
  (exec-path-from-shell :toggle (or (spacemacs/system-is-mac)
                                    (spacemacs/system-is-linux)
                                    (eq window-system 'x)))
  hydra
  use-package
  which-key

  (evil-evilified-state :location local :protected t)
  (holy-mode :location local)
  (hybrid-mode :location local)
  (spacemacs-theme :location built-in)
  )
