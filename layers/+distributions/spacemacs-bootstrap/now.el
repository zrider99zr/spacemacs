





;; evil

(defun spacemacs/set-evil-search-module (style)
  "Set the evil search module depending on STYLE."
  (cond
   ((or (eq 'vim style)
        (and (eq 'hybrid style)
             (bound-and-true-p hybrid-mode-use-evil-search-module)))
    ;; if Evil is loaded already, just setting `evil-search-module' isn't
    ;; enough, we need to call `evil-select-search-module' as well (this is done
    ;; automatically when `evil-search-module' is changed via customize)
    (if (featurep 'evil-search)
        (evil-select-search-module 'evil-search-module 'evil-search)
      (setq-default evil-search-module 'evil-search)))
   (t
    (if (featurep 'evil-search)
        (evil-select-search-module 'evil-search-module 'isearch)
      (setq-default evil-search-module 'isearch)))))

;; ensure that the search module is set at startup
;; must be called before evil is required to really take effect.
(spacemacs/set-evil-search-module dotspacemacs-editing-style)
(add-hook 'spacemacs-editing-style-hook 'spacemacs/set-evil-search-module)

(require 'evil)
(evil-mode)

;; Thanks to `editorconfig-emacs' for many of these
(defvar spacemacs--indent-variable-alist
  ;; Note that derived modes must come before their sources
  '(((awk-mode c-mode c++-mode java-mode
      idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
    (groovy-mode . groovy-indent-offset)
    (python-mode . python-indent-offset)
    (cmake-mode . cmake-tab-width)
    (coffee-mode . coffee-tab-width)
    (cperl-mode . cperl-indent-level)
    (css-mode . css-indent-offset)
    (elixir-mode . elixir-smie-indent-basic)
    ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
    (enh-ruby-mode . enh-ruby-indent-level)
    (erlang-mode . erlang-indent-level)
    (js2-mode . js2-basic-offset)
    (js3-mode . js3-indent-level)
    ((js-mode json-mode) . js-indent-level)
    (latex-mode . (LaTeX-indent-level tex-indent-basic))
    (livescript-mode . livescript-tab-width)
    (mustache-mode . mustache-basic-offset)
    (nxml-mode . nxml-child-indent)
    (perl-mode . perl-indent-level)
    (puppet-mode . puppet-indent-level)
    (ruby-mode . ruby-indent-level)
    (rust-mode . rust-indent-offset)
    (scala-mode . scala-indent:step)
    (sgml-mode . sgml-basic-offset)
    (sh-mode . sh-basic-offset)
    (typescript-mode . typescript-indent-level)
    (web-mode . web-mode-markup-indent-offset)
    (yaml-mode . yaml-indent-offset))
  "An alist where each key is either a symbol corresponding
to a major mode, a list of such symbols, or the symbol t,
acting as default. The values are either integers, symbols
or lists of these.")

(defun spacemacs//set-evil-shift-width ()
  "Set the value of `evil-shift-width' based on the indentation settings of the
current major mode."
  (let ((shift-width
         (catch 'break
           (dolist (test spacemacs--indent-variable-alist)
             (let ((mode (car test))
                   (val (cdr test)))
               (when (or (and (symbolp mode) (derived-mode-p mode))
                         (and (listp mode) (apply 'derived-mode-p mode))
                         (eq 't mode))
                 (when (not (listp val))
                   (setq val (list val)))
                 (dolist (v val)
                   (cond
                    ((integerp v) (throw 'break v))
                    ((and (symbolp v) (boundp v))
                     (throw 'break (symbol-value v))))))))
           (throw 'break (default-value 'evil-shift-width)))))
    (when (and (integerp shift-width)
               (< 0 shift-width))
      (setq-local evil-shift-width shift-width))))

(defun spacemacs/evil-smart-doc-lookup ()
  "Run documentation lookup command specific to the major mode.
Use command bound to `SPC m h h` if defined, otherwise fall back
to `evil-lookup'"
  (interactive)
  (let ((binding (key-binding (kbd (concat dotspacemacs-leader-key " mhh")))))
    (if (commandp binding)
        (call-interactively binding)
      (evil-lookup))))

(defmacro spacemacs|define-text-object (key name start end)
  "Define a text object and a surround pair.
START and END are strings (not regular expressions) that define
the boundaries of the text object."
  `(progn
     (spacemacs|define-text-object-regexp ,key ,name
                                          ,(regexp-quote start)
                                          ,(regexp-quote end))
     (with-eval-after-load 'evil-surround
       (push (cons (string-to-char ,key)
                   (if ,end
                       (cons ,start ,end)
                     ,start))
             evil-surround-pairs-alist))))

(defmacro spacemacs|define-text-object-regexp (key name start-regexp end-regexp)
  "Define a text object.
START-REGEXP and END-REGEXP are the boundaries of the text object."
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-outer-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regexp ,end-regexp beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regexp ,end-regexp beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

;; need to delay this macro since it relies on evil key maps to be defined
(with-eval-after-load 'evil
  (defmacro evil-map (state key seq)
    "Map for a given STATE a KEY to a sequence SEQ of keys.

Can handle recursive definition only if KEY is the first key of SEQ.
Example: (evil-map visual \"<\" \"<gv\")"
    (let ((map (intern (format "evil-%S-state-map" state))))
      `(define-key ,map ,key
         (lambda ()
           (interactive)
           ,(if (string-equal key (substring seq 0 1))
                `(progn
                   (call-interactively ',(lookup-key evil-normal-state-map key))
                   (execute-kbd-macro ,(substring seq 1)))
              (execute-kbd-macro ,seq)))))))

;; Use evil as a default jump handler
(push 'evil-goto-definition spacemacs-default-jump-handlers)

;; State cursors
(defun spacemacs/state-color-face (state)
  "Return the symbol of the face for the given STATE."
  (intern (format "spacemacs-%s-face" (symbol-name state))))

(defun spacemacs/state-color (state)
  "Return the color string associated to STATE."
  (face-background (spacemacs/state-color-face state)))

(defun spacemacs/current-state-color ()
  "Return the color string associated to the current state."
  (face-background (spacemacs/state-color-face evil-state)))

(defun spacemacs/state-face (state)
  "Return the face associated to the STATE."
  (spacemacs/state-color-face state))

(defun spacemacs/current-state-face ()
  "Return the face associated to the current state."
  (let ((state (if (eq evil-state 'operator)
                    evil-previous-state
                  evil-state)))
    (spacemacs/state-color-face state)))

(defun spacemacs/add-evil-cursor (state color shape)
  "Define a cursor and face for a new evil state.
An appropriate entry is added to `', as well.

For evil states that do not need an evil cursor use
`spacemacs/define-evil-state-face' instead."
  (add-to-list 'spacemacs-evil-cursors (list state color shape))
  (spacemacs/define-evil-state-face state color)
  (set (intern (format "evil-%s-state-cursor" state))
       (list (when dotspacemacs-colorize-cursor-according-to-state color)
             shape)))

(defun spacemacs/define-evil-state-face (state color)
  "Define a face for an evil state.
For evil states that also need an entry to `spacemacs-evil-cursors' use
`spacemacs/add-evil-cursor' instead."
  ;; this function and `spacemacs/add-evil-cursor' need to be separate because
  ;; some states must explicitly *not* have their own evil spacemacs cursor
  ;; for example treemacs: it needs no cursor since it solely uses hl-line-mode
  ;; and having an evil cursor defined anyway leads to the cursor sometimes
  ;; visibly flashing in treemacs buffers
  (eval `(defface ,(intern (format "spacemacs-%s-face" state))
           `((t (:background ,color
                             :foreground ,(face-background 'mode-line)
                             :inherit 'mode-line)))
           (format "%s state face." state)
           :group 'spacemacs)))

(defun spacemacs/set-state-faces ()
  (cl-loop for (state color cursor) in spacemacs-evil-cursors
           do
           (set-face-attribute (intern (format "spacemacs-%s-face" state))
                               nil
                               :foreground (face-background 'mode-line))))

(defun evil-insert-state-cursor-hide ()
  (setq evil-insert-state-cursor '((hbar . 0))))

(defvar spacemacs-evil-cursors '(("normal" "DarkGoldenrod2" box)
                                 ("insert" "chartreuse3" (bar . 2))
                                 ("emacs" "SkyBlue2" box)
                                 ("hybrid" "SkyBlue2" (bar . 2))
                                 ("replace" "chocolate" (hbar . 2))
                                 ("evilified" "LightGoldenrod3" box)
                                 ("visual" "gray" (hbar . 2))
                                 ("motion" "plum3" box)
                                 ("lisp" "HotPink1" box)
                                 ("iedit" "firebrick1" box)
                                 ("iedit-insert" "firebrick1" (bar . 2)))
  "Colors assigned to evil states with cursor definitions.
To add your own, use `spacemacs/add-evil-curosr'.")
(cl-loop for (state color shape) in spacemacs-evil-cursors
         do (spacemacs/add-evil-cursor state color shape))
(add-hook 'spacemacs-post-theme-change-hook 'spacemacs/set-state-faces)

;; evil ex-command
(define-key evil-normal-state-map (kbd dotspacemacs-ex-command-key) 'evil-ex)
(define-key evil-visual-state-map (kbd dotspacemacs-ex-command-key) 'evil-ex)
(define-key evil-motion-state-map (kbd dotspacemacs-ex-command-key) 'evil-ex)
(setq evil-ex-substitute-global dotspacemacs-ex-substitute-global)

;; evil-want-Y-yank-to-eol must be set via customize to have an effect
(customize-set-variable 'evil-want-Y-yank-to-eol dotspacemacs-remap-Y-to-y$)

;; bind evil-jump-forward for GUI only.
(define-key evil-motion-state-map [C-i] 'evil-jump-forward)

;; Make the current definition and/or comment visible.
(define-key evil-normal-state-map "zf" 'reposition-window)
;; toggle maximize buffer
(define-key evil-window-map (kbd "o") 'spacemacs/toggle-maximize-buffer)
(define-key evil-window-map (kbd "C-o") 'spacemacs/toggle-maximize-buffer)
;; make cursor keys work
(define-key evil-window-map (kbd "<left>") 'evil-window-left)
(define-key evil-window-map (kbd "<right>") 'evil-window-right)
(define-key evil-window-map (kbd "<up>") 'evil-window-up)
(define-key evil-window-map (kbd "<down>") 'evil-window-down)
(spacemacs/set-leader-keys "re" 'evil-show-registers)
;; motions keys for help buffers
(evil-define-key 'motion help-mode-map (kbd "<escape>") 'quit-window)
(evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
(evil-define-key 'motion help-mode-map (kbd "S-<tab>") 'backward-button)
(evil-define-key 'motion help-mode-map (kbd "]") 'help-go-forward)
(evil-define-key 'motion help-mode-map (kbd "gf") 'help-go-forward)
(evil-define-key 'motion help-mode-map (kbd "[") 'help-go-back)
(evil-define-key 'motion help-mode-map (kbd "gb") 'help-go-back)
(evil-define-key 'motion help-mode-map (kbd "gh") 'help-follow-symbol)

;; It's better that the default value is too small than too big
(setq-default evil-shift-width 2)
;; After major mode has changed, reset evil-shift-width
(add-hook 'after-change-major-mode-hook 'spacemacs//set-evil-shift-width 'append)

;; Keep the region active when shifting
(when dotspacemacs-retain-visual-state-on-shift
  (evil-map visual "<" "<gv")
  (evil-map visual ">" ">gv"))

;; move selection up and down
(when dotspacemacs-visual-line-move-text
  (define-key evil-visual-state-map "J" (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map "K" (concat ":m '<-2" (kbd "RET") "gv=gv")))

(evil-ex-define-cmd "enew" 'spacemacs/new-empty-buffer)

(define-key evil-normal-state-map (kbd "K") 'spacemacs/evil-smart-doc-lookup)
(define-key evil-normal-state-map (kbd "gd") 'spacemacs/jump-to-definition)
(define-key evil-normal-state-map (kbd "gD") 'spacemacs/jump-to-definition-other-window)

;; define text objects
(spacemacs|define-text-object "$" "dollar" "$" "$")
(spacemacs|define-text-object "*" "star" "*" "*")
(spacemacs|define-text-object "8" "block-star" "/*" "*/")
(spacemacs|define-text-object "|" "bar" "|" "|")
(spacemacs|define-text-object "%" "percent" "%" "%")
(spacemacs|define-text-object "/" "slash" "/" "/")
(spacemacs|define-text-object "_" "underscore" "_" "_")
(spacemacs|define-text-object "-" "hyphen" "-" "-")
(spacemacs|define-text-object "~" "tilde" "~" "~")
(spacemacs|define-text-object "=" "equal" "=" "=")
(spacemacs|define-text-object "«" "double-angle-bracket" "«" "»")
(spacemacs|define-text-object "｢" "corner-bracket" "｢" "｣")
(spacemacs|define-text-object "‘" "single-quotation-mark" "‘" "’")
(spacemacs|define-text-object "“" "double-quotation-mark" "“" "”")
(evil-define-text-object evil-pasted (count &rest args)
  (list (save-excursion (evil-goto-mark ?\[) (point))
        (save-excursion (evil-goto-mark ?\]) (point))))
(define-key evil-inner-text-objects-map "P" 'evil-pasted)
;; define text-object for entire buffer
(evil-define-text-object evil-inner-buffer (count &optional beg end type)
  (list (point-min) (point-max)))
(define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)

;; turn off evil in corelv buffers
(push '("\\*LV\\*") evil-buffer-regexps)

;; replace `dired-goto-file' with `helm-find-files', since `helm-find-files'
;; can do the same thing and with fuzzy matching and other features.
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map "J" 'spacemacs/helm-find-files)
  (define-key dired-mode-map "j" 'spacemacs/helm-find-files)
  (evil-define-key 'normal dired-mode-map (kbd dotspacemacs-leader-key)
    spacemacs-default-map))

;; support smart 1parens-strict-mode
(when (ddls/package-used-p 'smartparens)
  (defadvice evil-delete-backward-char-and-join
      (around spacemacs/evil-delete-backward-char-and-join activate)
    (if (bound-and-true-p smartparens-strict-mode)
        (call-interactively 'sp-backward-delete-char)
      ad-do-it)))

;; Define history commands for comint
(when (eq dotspacemacs-editing-style 'vim)
  (evil-define-key 'insert comint-mode-map
    (kbd "C-k") 'comint-previous-input
    (kbd "C-j") 'comint-next-input))
(evil-define-key 'normal comint-mode-map
  (kbd "C-k") 'comint-previous-input
  (kbd "C-j") 'comint-next-input)

;; ignore repeat
(evil-declare-ignore-repeat 'spacemacs/next-error)
(evil-declare-ignore-repeat 'spacemacs/previous-error)

