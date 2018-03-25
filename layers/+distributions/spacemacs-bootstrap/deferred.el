
;; scrolling transient state
(spacemacs|define-transient-state scroll
  :title "Scrolling Transient State"
  :doc "
 Buffer^^^^              Full page^^^^     Half page^^^^        Line/column^^^^
 ──────^^^^───────────── ─────────^^^^──── ─────────^^^^─────── ───────────^^^^─────
 [_<_/_>_] beginning/end [_f_/_b_] down/up [_J_/_K_] down/up    [_j_/_k_] down/up
 ^ ^ ^ ^                 ^ ^ ^ ^          [_H_/_L_] left/right [_h_/_l_] left/right
 ^ ^ ^ ^                 ^ ^ ^ ^          [_d_/_u_] down/up     ^ ^ ^ ^"
  :bindings
  ;; buffer
  ("<" evil-goto-first-line)
  (">" evil-goto-line)
  ;; full page
  ("f" evil-scroll-page-down)
  ("b" evil-scroll-page-up)
  ;; half page
  ("d" evil-scroll-down)
  ("u" evil-scroll-up)
  ("J" evil-scroll-down)
  ("K" evil-scroll-up)
  ("H" evil-scroll-left)
  ("L" evil-scroll-right)
  ;; lines and columns
  ("j" evil-scroll-line-down)
  ("k" evil-scroll-line-up)
  ("h" evil-scroll-column-left)
  ("l" evil-scroll-column-right))

(spacemacs/set-leader-keys
  ;; buffer
  "N<" 'spacemacs/scroll-transient-state/evil-goto-first-line
  "N>" 'spacemacs/scroll-transient-state/evil-goto-line
  ;; full page
  "Nf" 'spacemacs/scroll-transient-state/evil-scroll-page-down
  "Nb" 'spacemacs/scroll-transient-state/evil-scroll-page-up
  ;; half page
  "Nd" 'spacemacs/scroll-transient-state/evil-scroll-down
  "Nu" 'spacemacs/scroll-transient-state/evil-scroll-up
  "NJ" 'spacemacs/scroll-transient-state/evil-scroll-down
  "NK" 'spacemacs/scroll-transient-state/evil-scroll-up
  "NH" 'spacemacs/scroll-transient-state/evil-scroll-left
  "NL" 'spacemacs/scroll-transient-state/evil-scroll-right
  ;; lines and columns
  "Nj" 'spacemacs/scroll-transient-state/evil-scroll-line-down
  "Nk" 'spacemacs/scroll-transient-state/evil-scroll-line-up
  "Nh" 'spacemacs/scroll-transient-state/evil-scroll-column-left
  "Nl" 'spacemacs/scroll-transient-state/evil-scroll-column-right)

;; pasting transient-state
(evil-define-command spacemacs//transient-state-0 ()
  :keep-visual t
  :repeat nil
  (interactive)
  (if current-prefix-arg
      (progn
        (setq this-command #'digit-argument)
        (call-interactively #'digit-argument))
    (setq this-command #'evil-beginning-of-line
          hydra-deactivate t)
    (call-interactively #'evil-beginning-of-line)))

(spacemacs|define-transient-state paste
  :title "Pasting Transient State"
  :doc "\n[%s(length kill-ring-yank-pointer)/%s(length kill-ring)] \
 [_C-j_/_C-k_] cycles through yanked text, [_p_/_P_] pastes the same text \
 above or below. Anything else exits."
  :bindings
  ("C-j" evil-paste-pop)
  ("C-k" evil-paste-pop-next)
  ("p" evil-paste-after)
  ("P" evil-paste-before)
  ("0" spacemacs//transient-state-0))

(when dotspacemacs-enable-paste-transient-state
  (define-key evil-normal-state-map
    "p" 'spacemacs/paste-transient-state/evil-paste-after)
  (define-key evil-normal-state-map
    "P" 'spacemacs/paste-transient-state/evil-paste-before))
;; fold transient state
(when (eq 'evil dotspacemacs-folding-method)
  (spacemacs|define-transient-state fold
    :title "Code Fold Transient State"
    :doc "
 Close^^          Open^^              Toggle^^             Other^^
 ───────^^──────  ─────^^───────────  ─────^^────────────  ─────^^───
 [_c_] at point   [_o_] at point      [_a_] around point   [_q_] quit
 ^^               [_O_] recursively   ^^
 [_m_] all        [_r_] all"
    :foreign-keys run
    :bindings
    ("a" evil-toggle-fold)
    ("c" evil-close-fold)
    ("o" evil-open-fold)
    ("O" evil-open-fold-rec)
    ("r" evil-open-folds)
    ("m" evil-close-folds)
    ("q" nil :exit t)
    ("C-g" nil :exit t)
    ("<SPC>" nil :exit t)))
(spacemacs/set-leader-keys "z." 'spacemacs/fold-transient-state/body)

