

(defun spacemacs/get-mode-line-theme-name ()
  "Return the mode-line theme name."
  (if (listp dotspacemacs-mode-line-theme)
      (car dotspacemacs-mode-line-theme)
    dotspacemacs-mode-line-theme))

(defun spacemacs/mode-line-separator ()
  "Return the separator type for the mode-line.
Return nil if no separator is defined."
  (when (listp dotspacemacs-mode-line-theme)
    (plist-get (cdr dotspacemacs-mode-line-theme) :separator)))

(defun spacemacs/mode-line-separator-scale ()
  "Return the separator scale for the mode-line.
Return nil if no scale is defined."
  (when (listp dotspacemacs-mode-line-theme)
    (plist-get (cdr dotspacemacs-mode-line-theme) :separator-scale)))


;; spaceline

(defun spacemacs/customize-powerline-faces ()
  "Alter powerline face to make them work with more themes."
  (when (boundp 'powerline-inactive2)
    (set-face-attribute 'powerline-inactive2 nil
                        :inherit 'font-lock-comment-face)))

(defun spacemacs//evil-state-face ()
  (let ((state (if (eq 'operator evil-state) evil-previous-state evil-state)))
    (intern (format "spacemacs-%S-face" state))))

(defun spacemacs//restore-powerline (buffer)
  "Restore the powerline in buffer"
  (with-current-buffer buffer
    (setq-local mode-line-format (default-value 'mode-line-format))
    (powerline-set-selected-window)
    (powerline-reset)))

(defun spacemacs//set-powerline-for-startup-buffers ()
  "Set the powerline for buffers created when Emacs starts."
  (dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
    (when (and (get-buffer buffer)
               (ddls/package-used-p 'spaceline))
      (spacemacs//restore-powerline buffer))))

(defun spacemacs//prepare-diminish ()
  (when spaceline-minor-modes-p
    (let ((unicodep (dotspacemacs|symbol-value
                     dotspacemacs-mode-line-unicode-symbols)))
      (setq spaceline-minor-modes-separator
            (if unicodep (if (display-graphic-p) "" " ") "|"))
      (dolist (mm spacemacs--diminished-minor-modes)
        (let ((mode (car mm)))
          (when (and (boundp mode) (symbol-value mode))
            (let* ((unicode (cadr mm))
                   (ascii (caddr mm))
                   (dim (if unicodep
                            unicode
                          (if ascii ascii unicode))))
              (diminish mode dim))))))))

(defcustom spacemacs-spaceline-additional-segments
  '((new-version :when active))
  "Additional segments for the Spacemacs modeline.

They are inserted in the modeline between `global' and
`buffer-position'.

Must be a list of valid segments; see `spaceline-install' for
more information on what constitutes a valid segment."
  :type '(repeat sexp)
  :group 'spacemacs)

(when (memq (spacemacs/get-mode-line-theme-name)
            '(spacemacs all-the-icons custom))
  (require 'spaceline-config)
  (add-hook 'spacemacs-post-user-config-hook 'spaceline-compile)
  (add-hook 'spacemacs-post-theme-change-hook
            'spacemacs/customize-powerline-faces)
  (add-hook 'spacemacs-post-theme-change-hook 'powerline-reset)
  (spacemacs|add-toggle mode-line-responsive
    :status spaceline-responsive
    :on (progn (setq spaceline-responsive t)
               (powerline-reset))
    :off (progn (setq spaceline-responsive nil)
                ;; seems necessary to recompile when turning off
                (spaceline-compile))
    :documentation "Make the mode-line responsive."
    :evil-leader "tmr")
  (setq powerline-default-separator
        (or (and (memq (spacemacs/get-mode-line-theme-name)
                       '(spacemacs custom))
                 (spacemacs/mode-line-separator))
            'wave)
        powerline-image-apple-rgb (spacemacs/system-is-mac)
        powerline-scale (or (spacemacs/mode-line-separator-scale) 1.5)
        powerline-height (spacemacs/compute-mode-line-height))
  ;; (spacemacs|do-after-display-system-init
  ;;  ;; seems to be needed to avoid weird graphical artefacts with the
  ;;  ;; first graphical client
  ;;  (require 'spaceline)
  ;;  (spaceline-compile))

  (spacemacs/customize-powerline-faces)
  (setq spaceline-org-clock-p nil
        spaceline-highlight-face-func 'spacemacs//evil-state-face)
  ;; Segment toggles
  (dolist (spec '((minor-modes "tmm")
                  (major-mode "tmM")
                  (version-control "tmv")
                  (new-version "tmV")
                  (point-position "tmp")
                  (org-clock "tmc")))
    (let* ((segment (car spec))
           (status-var (intern (format "spaceline-%S-p" segment))))
      (eval `(spacemacs|add-toggle ,(intern (format "mode-line-%S" segment))
               :status ,status-var
               :on (setq ,status-var t)
               :off (setq ,status-var nil)
               :documentation ,(format "Show %s in the mode-line."
                                       (replace-regexp-in-string
                                        "-" " " (format "%S" segment)))
               :evil-leader ,(cadr spec)))))
  ;; unicode
  (let ((unicodep (dotspacemacs|symbol-value
                   dotspacemacs-mode-line-unicode-symbols)))
    (setq spaceline-window-numbers-unicode unicodep
          spaceline-workspace-numbers-unicode unicodep))
  (add-hook 'spaceline-pre-hook 'spacemacs//prepare-diminish)
  ;; New spacemacs version segment
  (defpowerline spacemacs-powerline-new-version
    (propertize
     spacemacs-version-check-lighter
     'mouse-face 'mode-line-highlight
     'help-echo (format "New version %s | Click with mouse-1 to update"
                        spacemacs-new-version)
     'local-map (let ((map (make-sparse-keymap)))
                  (define-key map
                    [mode-line down-mouse-1]
                    (lambda (event)
                      (interactive "@e")
                      (if (yes-or-no-p
                           (format
                            (concat "Do you want to update to the newest "
                                    "version %s ?") spacemacs-new-version))
                          (progn
                            (spacemacs/switch-to-version
                             spacemacs-new-version))
                        (message "Update aborted."))))
                  map)))
  (spaceline-define-segment new-version
    (when spacemacs-new-version
      (spacemacs-powerline-new-version
       (spacemacs/get-new-version-lighter-face
        spacemacs-version spacemacs-new-version))))
  (let ((theme (intern (format "spaceline-%S-theme"
                               (spacemacs/get-mode-line-theme-name)))))
    (apply theme spacemacs-spaceline-additional-segments))
  ;; Additional spacelines
  ;; (when (package-installed-p 'helm)
  ;;   (spaceline-helm-mode t))
  ;; (when (configuration-layer/package-used-p 'info+)
  ;;   (spaceline-info-mode t))
  ;; Enable spaceline for buffers created before the configuration of
  ;; spaceline
  (spacemacs//set-powerline-for-startup-buffers))
