(packages:
  anzu
  fancy-battery
  ;; dependency of spaceline-all-the-icons which came from
  ;; the emacs wiki, we fetch it from Emacs Mirror for now.
  ;; TODO eventually remove this if font-lock+ is available
  ;; in an ELPA repository.
  (font-lock+ :step pre
              :location (recipe :fetcher github
                                :repo emacsmirror/font-lock-plus))
  neotree
  spaceline
  spaceline-all-the-icons
  symon
  (vim-powerline :location local)
  )
