;; -*- nameless-current-name: "ddls"; lexical-binding: t -*-
;;; core-double-dot-layer-system.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Authors: Sylvain Benner <sylvain.benner@gmail.com>
;;          Eugene Yaremenko <>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; Spacemacs Layer system 2.0
;;
;;; License: GPLv3

(require 'cl)
(require 'cl-generic)
(require 'eieio)
(require 'ht)

(require 'core-dotspacemacs)

(defun spacemacs-buffer/message (msg &rest args)
  "Display MSG in *Messages* prepended with '(Spacemacs)'.
The message is displayed only if `init-file-debug' is non nil.
ARGS: format string arguments."
  (when init-file-debug
    (message "(Spacemacs) %s" (apply 'format msg args))))

(defvar spacemacs-buffer--errors nil
  "List of errors during startup.")

(defun spacemacs-buffer/error (msg &rest args)
  "Display MSG as an Error message in `*Messages*' buffer.
ARGS: format string arguments."
  (let ((msg (apply 'format msg args)))
    (message "(Spacemacs) Error: %s" msg)
    (when message-log-max
      (add-to-list 'spacemacs-buffer--errors msg 'append))))

(defvar spacemacs-buffer--warnings nil
  "List of warnings during startup.")

(defun spacemacs-buffer/warning (msg &rest args)
  "Display MSG as a warning message but in buffer `*Messages*'.
ARGS: format string arguments."
  (let ((msg (apply 'format msg args)))
    (message "(Spacemacs) Warning: %s" msg)
    (when message-log-max
      (add-to-list 'spacemacs-buffer--warnings msg 'append))))



;; Macros

;; Layers

(defmacro layers: (&rest layers)
  "Define used layers."
  (declare (indent defun))
  ;; (setq dotspacemacs--configuration-layers-saved
  ;;       dotspacemacs-configuration-layers)
  )

;; Packages

(defmacro packages: (&rest packages-specs)
  "Define packages owned by the layer from where this macro is called.

PACKAGES-SPECS is a list of symbols and/or lists declaring the packages
and their associated properties. "
  (declare (indent defun))
  (let ((layer-name (ddls//get-directory-name
                     (if load-file-name
                         ;; File is being loaded
                         (file-name-directory load-file-name)
                       ;; File is being evaluated
                       default-directory))))
    `(setq ,(ddls//package:-variable-name layer-name) ',packages-specs)))

;; Autoload

(defmacro autoload: (package &rest plist)
  "Defines the autloads for a given PACKAGE.

PACKAGE is a package symbol.

PLIST is a property list supporting the following keywords:

- `:key-bindings' dispatched to the macro `key-bindings:'
- TODO
"
  (declare (indent defun))
  )

;; Load

(defmacro load: (package &rest body)
  "Load a package by executing the given BODY."
  (declare (indent defun)))

;; Key bindings

(defun stubmax/package-used-p (pkg)
  "Is PKG used? Used packager are: [foo everyoneneedsme bar]"
  (let ((used-packages '(foo everyoneneedsme bar)))
    (message "Testing if \"%s\" package is used." pkg)
    (message "Used packages: %s" used-packages)
    (memq pkg used-packages)))

(defun stubmax/major-mode-prefix ()
  "Get current prefix for major modes in the leader menu. (it is \"m\")"
  (message "Asking what major mode prefix is (it is \"m\" btw).")
  "m")

(defun stubmax/bind-key-global (key-seq fn-symbol label)
  "Bind global KEY-SEQ to FN-SYMBOL function.
Display LABEL in leader menu instead of the function name."
  (message
   "%S"
   `(stubmax/bind-key-global ,key-seq ,fn-symbol ,label)))

(defun stubmax/declare-prefix-global (key-prefix label)
  "Declare global KEY-PREFIX with LABEL in leader menu."
  (message
   "%S"
   `(stubmax/declare-prefix-global ,key-prefix ,label)))

(defun stubmax/bind-key-for-major-mode (mode key-seq fn-symbol label)
  "Bind major-mode MODE KEY-SEQ to FN-SYMBOL function.
Display LABEL in leader menu instead of the function name."
  (message
   "%S"
   `(stubmax/bind-key-for-major-mode ,mode ,key-seq ,fn-symbol ,label)))

(defun stubmax/declare-prefix-for-major-mode (mode key-prefix label)
  "Declare MODE major-mode KEY-PREFIX with LABEL in leader menu"
  (message
   "%S"
   `(stubmax/declare-prefix-for-major-mode ,mode ,key-prefix ,label)))

(defun stubmax/bind-key-for-minor-mode (mode key-seq fn-symbol label)
  "Bind minor-mode KEY-SEQ to FN-SYMBOL function.
Display LABEL in leader menu instead of the function name."
  (message
   "%S"
   `(stubmax/bind-key-for-minor-mode ,mode ,key-seq ,fn-symbol ,label)))

(defun stubmax/declare-prefix-for-minor-mode (mode key-prefix label)
  "Declare MODE minor-mode KEY-PREFIX with LABEL in leader menu"
  (message
   "%S"
   `(stubmax/declare-prefix-for-minor-mode ,mode ,key-prefix ,label)))

;; Key bindings - Implementation details

(cl-defstruct spacemacs--bind-state
  "State object for `spacemacs|bind' macro implementation.
CTYPE - current binding type.
RSEXP - accumulator with the macro output.
This structure has one interpreter method for each supported CTYPE.
NOTE: CTYPE is a type of a currently processed binding(:major/:minor/global...)"
  ctype rsexp)

(cl-defgeneric spacemacs//bind-interpret (state binding)
  (:documentation "Based on BINDING type modify STATE using BINDING value."))

(cl-defmethod spacemacs//bind-interpret ((state spacemacs--bind-state)
                                         (keyword symbol))
  "Set STATE slot ctype(current type) to the KEYWORD value."
  (if (not (keywordp keyword))
      (cl-call-next-method)
    (setf (spacemacs--bind-state-ctype state) keyword)
    state))

(cl-defmethod spacemacs//bind-interpret ((state spacemacs--bind-state)
                                         (sexp list))
  "Apply STATE method from ctype slot to SEXP and append output to rsexp slot."
  (cl-callf append (spacemacs--bind-state-rsexp state)
    (funcall (spacemacs--bind-state-ctype state) state sexp))
  state)

(defun spacemacs//bind-indenter (pos pdat)
  "Indentation function for `spacemacs|bind' macro."
  (list
   (+ (car pdat)
      (if (or (= 1 (- (caddr pdat) (cadr pdat)))
              (save-excursion (goto-char pos) (looking-at-p "[[:space:]]*:")))
          1 2))))

(defun spacemacs//bind-form-walker (form path k-fn p-fn)
  "Part of `spacemacs--bind-state' interpreters implementation.
FORM is a node of a binding tree without mode (car of the root form).
PATH is a key sequence path (concatenation of cars) to the current tree node.
K-FN called for each key binding node with 3 arguments: full_key_sequence,
function_symbol and label_for_leader_menu.
P-FN called for each prefix binding node with 2 arguments:
full_key_prefix_sequence and label_for_leader_menu.
Both K-FN and P-FN should return binding evaluation forms.
The forms will be concatenated and substituted by `spacemacs|bind' macro."
  (append
   (when (char-or-string-p (car form))
     (list (cl-destructuring-bind
               (key-or-prefix
                leader-label-or-fn-symbol
                leader-label-or-next-form)
               (seq-take form 3)
             (let ((full-key-or-prefix (concat path key-or-prefix)))
               (if (symbolp leader-label-or-fn-symbol)
                   (funcall k-fn
                            full-key-or-prefix
                            leader-label-or-fn-symbol
                            leader-label-or-next-form)
                 (funcall p-fn
                          full-key-or-prefix
                          leader-label-or-fn-symbol))))))
   (when-let ((unwrapped-car (and (consp (car-safe form))
                                  (car form))))
     (spacemacs//bind-form-walker
      unwrapped-car
      path
      k-fn
      p-fn))
   (when-let ((next-child (and (consp (caddr form))
                               (caddr form))))
     (spacemacs//bind-form-walker
      next-child
      (concat path (car form))
      k-fn
      p-fn))
   (when-let ((kb-form-next-sibling (and (consp (cadr form))
                                         (cadr form))))
     (spacemacs//bind-form-walker
      kb-form-next-sibling
      path
      k-fn
      p-fn))
   (when-let ((p-form-next-sibling (and (consp (cadddr form))
                                        (cadddr form))))
     (spacemacs//bind-form-walker
      p-form-next-sibling
      (concat path (car form))
      k-fn
      p-fn))))

;; Key bindings - keywords handlers

(cl-defmethod :global ((_ spacemacs--bind-state) form)
  "Interpreter for global binding forms."
  (spacemacs//bind-form-walker
   form
   ""
   (lambda (key-seq fn-symbol label)
     `(stubmax/bind-key-global ,key-seq ',fn-symbol ,label))
   (lambda (key-prefix label)
     `(stubmax/declare-prefix-global ,key-prefix ,label))))

(cl-defmethod :major ((_ spacemacs--bind-state) form)
  "Interpreter for major mode binding forms."
  (let ((mode (pop form)))
    (spacemacs//bind-form-walker
     form
     (stubmax/major-mode-prefix)
     (lambda (key-seq fn-symbol label)
       `(stubmax/bind-key-for-major-mode ',mode ,key-seq ',fn-symbol ,label))
     (lambda (key-prefix label)
       `(stubmax/declare-prefix-for-major-mode ',mode ,key-prefix ,label)))))

(cl-defmethod :minor ((_ spacemacs--bind-state) form)
  "Interpreter for minor mode binding forms."
  (let ((mode (pop form)))
    (spacemacs//bind-form-walker
     form
     ""
     (lambda (key-seq fn-symbol label)
       `(stubmax/bind-key-for-minor-mode ',mode ,key-seq ',fn-symbol ,label))
     (lambda (key-prefix label)
       `(stubmax/declare-prefix-for-minor-mode ',mode ,key-prefix ,label)))))

(defmacro key-bindings: (package &rest bindings)
  "Key-bindings macro.
If PACKAGE is used then bind keys and prefixes from BINDINGS.

BINDINGS format:
 <DELIMITER_KEYWORD>
  <BINDING_FORM>
  ...
  <BINDING_FORM>
 <DELIMITER_KEYWORD>
  ...
DELIMITER_KEYWORD - specifies a type of fallowing <BINDING_FORM> (or forms).
Currently supported types: (:major :minor :global).
Global <BINDING_FORM> format:
  TODO: Describe the format.
Major and minor <BINDING_FORM> format is the same as a global one but the root
form starts with a corresponding mode symbol.
\(fn PROVIDER <<DELIMITER_KEYWORD> <BINDING_FORMS>...>...)"
  (declare (indent spacemacs//bind-indenter))
  (spacemacs--bind-state-rsexp
   (seq-reduce 'spacemacs//bind-interpret
               bindings
               (make-spacemacs--bind-state
                :ctype (pop bindings)
                :rsexp `(when (stubmax/package-used-p ',package))))))


;; Variables

(defun ddls//package:-variable-name (layer-name)
  "Return the variable name containing the list of package specs for LAYER-NAME."
  (intern (format "%s-packages" layer-name)))

(defconst ddls-layers-directory
  (expand-file-name (concat spacemacs-start-directory "layers/"))
  "Spacemacs layers directory.")

(defconst ddls-private-directory
  (expand-file-name (concat spacemacs-start-directory "private/"))
  "Spacemacs private layers base directory.")

(defconst ddls-private-layer-directory
  (let ((dotspacemacs-layer-dir
         (when dotspacemacs-directory
           (expand-file-name
            (concat dotspacemacs-directory "layers/")))))
    (if (and dotspacemacs-directory
             (file-exists-p dotspacemacs-layer-dir))
        dotspacemacs-layer-dir
      ddls-private-directory))
  "Spacemacs default directory for private layers.")

(defvar ddls-categories '()
  "List of strings corresponding to category names. A category is a
directory with a name starting with `+'.")

(defvar ddls-exclude-all-layers nil
  "If non nil then only the distribution layer is loaded.")

(defvar ddls--inhibit-warnings nil
  "If non-nil then warning messages emitted by the layer system are ignored.")

(defconst ddls-spacemacs-bigfile
  (expand-file-name (concat spacemacs-cache-directory "spacemacs.bfc.el"))
  "Spacemacs big f*****g configuration file.")


;; Classes

;; class: layer

(defvar ddls--indexed-layers (make-hash-table :size 1024)
  "Hash map to index `ddls-layer' objects by their names.")

(defvar ddls--used-layers '()
  "A non-sorted list of used layer name symbols.")

(defvar ddls--packages-layer-file "packages2.el"
  "File name of layer file listing packages.")

(defclass ddls-layer ()
  ((name :initarg :name
         :type symbol
         :documentation "Name of the layer.")
   (dir :initarg :dir
        :initform nil
        :type (satisfies (lambda (x) (or (null x) (stringp x))))
        :documentation "Absolute path to the layer directory.")
   (packages-specs :initarg :packages
             :initform nil
             :type list
             :documentation "List of package symbols declared in this layer.")
   (select-query :initarg :select-query
                 :initform nil
                 :type list
                 :documentation "Query to select used packages.")
   (selected-packages :initarg :selected-packages
             :initform nil
             :type list
             :documentation "List of used package names.")
   (variables :initarg :variables
              :initform nil
              :type list
              :documentation "A list of variable-value pairs.")
   (lazy-install :initarg :lazy-install
                 :initform nil
                 :type boolean
                 :documentation
                 "If non-nil then the layer needs to be installed")
   (disabled :initarg :disabled-for
             :initform nil
             :type list
             :documentation "A list of layers where this layer is disabled.")
   (enabled :initarg :enabled-for
            :initform 'unspecified
            :type (satisfies (lambda (x) (or (listp x) (eq 'unspecified x))))
            :documentation
            (concat "A list of layers where this layer is enabled. "
                    "(Takes precedence over `:disabled-for'.)"))
   ;; Note:
   ;; 'can-shadow' is a commutative relation:
   ;;     if Y 'can-shadow' X then X 'can-shadow' Y
   ;; but the 'shadow' operation is not commutative, the order of the operands
   ;; is determined by the order of the layers in the dotfile
   ;; variable `dotspacemacs-configuration-layers'
   (can-shadow :initarg :can-shadow
               :initform 'unspecified
               :type (satisfies (lambda (x) (or (listp x) (eq 'unspecified x))))
               :documentation "A list of layers this layer can shadow."))
  "A configuration layer.")

(defmethod ddls-layer/initialize ((layer ddls-layer))
  "Initialize the layer."
  ;; packages
  ;; packages file is mandatory so we don't have to check for their existence.
  (load (concat (oref layer :dir) ddls--packages-layer-file) nil t)
  (let* ((lname (oref layer :name))
         (pspecs (symbol-value (ddls//package:-variable-name lname))))
    (oset layer :packages pspecs)
    (dolist (specs pspecs)
      (let* ((pname (ddls//get-package-name-from-specs specs))
             (package (ddls/make-indexed-package-from-specs
                       specs (ddls/get-indexed-package pname))))
        (ddls//add-indexed-package package)
        )))
  ;; autoloads
  ;; TODO
  )

(defmethod ddls-layer/load ((layer ddls-layer))
  "Load layer (only used layers should be loaded)."
  ;; cannot selected the packages now, will be done when looping on the
  ;; used layers -- keeping this line here commented for now
  (dolist (specs (oref layer :packages))
    (let* ((pname (ddls//get-package-name-from-specs specs))
           (package (ddls/make-indexed-package-from-specs
                     specs (ddls/get-indexed-package pname))))
      (ddls-layer/select-package layer package))))

(defmethod ddls-layer/load-deferred ((layer ddls-layer))
  "Load deferred configuration of the layer.")

(defmethod ddls-layer/select-package ((layer ddls-layer) package)
  "Select package by applying `:select-query' slot.
If the package is selected then it is added to `:selected-packages' list."
  (let* ((query (oref layer :select-query))
         (pname (oref package :name)))
    (when (or
           ;; (my-layer ...))
           (null query)
           ;; (my-layer :packages package1 package2 ...)
           (and (not (eq 'not (car query)))
                (memq pname query))
           ;; (my-layer :packages (not package1 package2 ...))
           (and (eq 'not (car query))
                (not (memq pname query))))
      (push pname (oref layer :selected-packages)))))

(defmethod ddls-layer/owned-packages ((layer ddls-layer) &optional props)
  "Return the list of owned packages-specs by LAYER.
If PROPS is non-nil then return packages-specs as lists with their properties.
LAYER has to be installed for this method to work properly."
  (delq nil (mapcar
             (lambda (x)
               (let* ((pkg-name (if (listp x) (car x) x))
                      (pkg (configuration-layer/get-package pkg-name)))
                 (when (eq (oref layer :name) (car (oref pkg :owners))) x)))
             (ddls-layer/get-packages layer props))))

(defmethod ddls-layer/owned-packages ((layer nil) &optional props)
  "Accept nil as argument and return nil."
  nil)

(defmethod ddls-layer/get-local-directory ((layer ddls-layer))
  "Return the local directory of LAYER."
  (concat (oref layer :dir) "local/"))

(defmethod ddls-layer/get-shadowing-layers ((layer ddls-layer))
  "Return the list of used layers that shadow LAYER."
  (let ((rank (cl-position (oref layer :name) ddls--used-layers))
        (shadow-candidates (oref layer :can-shadow))
        shadowing-layers)
    (when (and (numberp rank)
               (not (eq 'unspecified shadow-candidates))
               (listp shadow-candidates))
      (mapcar
       (lambda (other)
         (let ((orank (cl-position other ddls--used-layers)))
           ;; OTHER shadows LAYER if and only if OTHER's rank is bigger than
           ;; LAYER's rank.
           (when (and (numberp orank) (< rank orank))
             (add-to-list 'shadowing-layers other))))
       ;; since the 'can-shadow' relation is commutative it is safe to use this
       ;; list, i.e. if LAYER can shadow layers X and Y then X and Y can shadow
       ;; LAYER.
       shadow-candidates))
    shadowing-layers))

(defmethod ddls-layer/get-packages ((layer ddls-layer) &optional props)
  "Return the list of packages-specs for LAYER.
If PROPS is non-nil then return packages-specs as lists along with their
properties."
  (let ((all (eq 'all (oref layer :selected-packages))))
    (delq nil (mapcar
               (lambda (x)
                 (let ((pkg-name (if (listp x) (car x) x)))
                   (when (or all (memq pkg-name
                                       (oref layer :selected-packages)))
                     (if props x pkg-name))))
               (oref layer :packages)))))

(defmethod ddls-layer/set-specs ((layer ddls-layer) specs)
  "Set slots info from LAYER-SPECS for corresponding indexed layer."
  (let ((disabled (when (listp specs)
                    (spacemacs/mplist-get specs :disabled-for)))
        (enabled (if (and (listp specs)
                          (memq :enabled-for specs))
                     (spacemacs/mplist-get specs :enabled-for)
                   'unspecified))
        (variables (when (listp specs)
                     (spacemacs/mplist-get specs :variables)))
        (shadow (if (and (listp specs)
                         (memq :can-shadow specs))
                    (spacemacs/mplist-get specs :can-shadow)
                  'unspecified))
        (select-query (when (listp specs)
                        (spacemacs/mplist-get specs :packages))))
    (oset layer :disabled-for disabled)
    (oset layer :enabled-for enabled)
    (oset layer :variables variables)
    (unless (eq 'unspecified shadow)
      (oset layer :can-shadow shadow))
    ;; we need to unwrap `select-query'
    (oset layer :select-query (if (and (not (null (car select-query)))
                                       (listp (car select-query)))
                                  (car select-query)
                                select-query))))

(defmethod ddls-layer/mark-as-used ((layer ddls-layer))
  "Mark the layer as used by adding it to the used layers list."
  (add-to-list 'ddls--used-layers (oref layer :name)))

(defun ddls/make-indexed-layer (name dir)
  "Return a `ddls-layer' object whose goal is to be indexed.
NAME is the name of the layer passed as a string.
DIR is the directory where the layer is, if it is nil then search in the indexed
layers for the path.
Return nil if the passed DIR is not valid."
  (let ((obj (ddls-layer (symbol-name name) :name name)))
    (if (or (null dir)
            (and dir (not (file-exists-p dir))))
        (ddls//warning "Cannot make layer %S without a valid directory!" name)
      (let* ((dir (file-name-as-directory dir)))
        (oset obj :dir dir)
        obj))))

(defun ddls//get-layer-name-from-specs (specs)
  "Return the name symbol of layer given the passed SPECS."
  (if (listp specs) (car specs) specs))

(defun ddls/layer-used-p (name)
  "Return non-nil if NAME is the name of a used and non-shadowed layer."
  (or (eq 'dotfile name)
      (let ((obj (ddls/get-indexed-layer name)))
        (when obj
          (and (not (ddls-layer/get-shadowing-layers obj))
               (memq name ddls--used-layers))))))

;; class: package

(defvar ddls--indexed-packages (make-hash-table :size 2048)
  "Hash map to index `ddls-package' objects by their names.")

(defvar ddls--used-packages '()
  "An alphabetically sorted list of used package names.")

(defvar ddls--protected-packages nil
  "A list of packages that will be protected from removal as orphans.")

(defconst ddls--package-properties-read-only-p nil
  "If non-nil then package properties are read only and cannot be overriden.")

(defclass ddls-package ()
  ((name :initarg :name
         :type symbol
         :documentation "Name of the package.")
   (min-version :initarg :min-version
                :initform nil
                :type list
                :documentation "Minimum version to install as a version list.")
   (owners :initarg :owners
           :initform nil
           :type list
           :documentation "The layer defining the init function.")
   (pre-layers :initarg :pre-layers
               :initform '()
               :type list
               :documentation "List of layers with a pre-init function.")
   (post-layers :initarg :post-layers
                :initform '()
                :type list
                :documentation "List of layers with a post-init function.")
   (location :initarg :location
             :initform elpa
             :type (satisfies (lambda (x)
                                (or (stringp x)
                                    (memq x '(built-in local site elpa))
                                    (and (listp x) (eq 'recipe (car x))))))
             :documentation "Location of the package.")
   (toggle :initarg :toggle
           :initform t
           :type (satisfies (lambda (x) (or (symbolp x) (listp x))))
           :documentation
           "Package is enabled/installed if toggle evaluates to non-nil.")
   (step :initarg :step
         :initform nil
         :type (satisfies (lambda (x) (member x '(nil bootstrap pre))))
         :documentation "Initialization step.")
   (lazy-install :initarg :lazy-install
                 :initform nil
                 :type boolean
                 :documentation
                 "If non-nil then the package needs to be installed")
   (protected :initarg :protected
              :initform nil
              :type boolean
              :documentation
              "If non-nil then this package cannot be excluded.")
   (excluded :initarg :excluded
             :initform nil
             :type boolean
             :documentation
             "If non-nil this package is excluded from all layers.")
   (requires :initarg :requires
             :initform nil
             :type list
             :documentation
             "Packages that must be enabled for this package to be enabled.")))

(defmethod ddls-package/mark-as-used ((pkg ddls-package))
  "Mark the package as used by adding it to the used packages list."
  (add-to-list 'ddls--used-packages (oref pkg :name)))

(defmethod ddls-package/toggled-p ((pkg ddls-package) &optional inhibit-msg)
  "Evaluate the `toggle' slot of passed PKG.
If INHIBIT-MSG is non nil then any message emitted by the toggle evaluation
is ignored."
  (let ((message-log-max (unless inhibit-msg message-log-max))
        (toggle (oref pkg :toggle)))
    (eval toggle t)))

(defmethod ddls-package/reqs-satisfied-p ((pkg ddls-package) &optional inhibit-msg)
  "Check if requirements of a package are all enabled.
If INHIBIT-MSG is non nil then any message emitted by the toggle evaluation
is ignored."
  (not (memq nil (mapcar
                  (lambda (dep-pkg)
                    (let ((pkg-obj (configuration-layer/get-package dep-pkg)))
                      (when pkg-obj
                        (ddls-package/enabled-p pkg-obj inhibit-msg))))
                  (oref pkg :requires)))))

(defmethod ddls-package/enabled-p ((pkg ddls-package) &optional inhibit-msg)
  "Check if a package is enabled.
This checks the excluded property, evaluates the toggle, if any, and recursively
checks whether dependent packages-specs are also enabled.
If INHIBIT-MSG is non nil then any message emitted by the toggle evaluation
is ignored."
  (and (or (oref pkg :protected) (not (oref pkg :excluded)))
       (ddls-package/reqs-satisfied-p pkg inhibit-msg)
       (ddls-package/toggled-p pkg inhibit-msg)))

(defmethod ddls-package/used-p ((pkg ddls-package))
  "Return non-nil if PKG is a used package."
  (and (not (null (oref pkg :owners)))
       (not (oref pkg :excluded))
       (ddls-package/enabled-p pkg t)))

(defmethod ddls-package/distant-p ((pkg ddls-package))
  "Return non-nil if PKG is a distant package (i.e. not built-in Emacs)."
  (and (not (memq (oref pkg :location) '(built-in site local)))
       (not (stringp (oref pkg :location)))))

(defmethod ddls-package/get-safe-owner ((pkg ddls-package))
  "Safe method to return the name of the layer which owns PKG."
  ;; The owner of a package is the first *used* layer in `:owners' slot.
  ;; Note: for packages-specs in `ddls--used-packages' the owner is
  ;; always the car of the `:owners' slot.
  (let ((layers (oref pkg :owners)))
    (while (and (consp layers)
                (not (ddls/layer-used-p (car layers))))
      (pop layers))
    (when (ddls/layer-used-p (car layers))
      (car layers))))

(defmethod ddls-package/set-property2 ((pkg ddls-package) slot value)
  "Set SLOT to the given VALUE for the package PKG.
If `ddls--package-properties-read-only-p' is non-nil then VALUE
is not set for the given SLOT."
  (unless ddls--package-properties-read-only-p
    (set-slot-value pkg slot value)))

(defun ddls/make-indexed-package-from-specs (specs lname &optional obj)
  "Return a `ddls-package' object based on SPECS.
LNAME is the name of the layer where the SPECS are listed.
If OBJ is non nil then copy SPECS properties into OBJ, otherwise create a new
object."
  (let* ((pname (ddls//get-package-name-from-specs specs))
         (pname-str (symbol-name pname))
         (layer (unless (eq 'dotfile lname) (ddls/get-indexed-layer lname)))
         (min-version (when (listp specs) (plist-get (cdr specs) :min-version)))
         (step (when (listp specs) (plist-get (cdr specs) :step)))
         (toggle (when (listp specs) (plist-get (cdr specs) :toggle)))
         (requires (when (listp specs) (plist-get (cdr specs) :requires)))
         (requires (if (listp requires) requires (list requires)))
         (excluded (when (listp specs) (plist-get (cdr specs) :excluded)))
         (location (when (listp specs) (plist-get (cdr specs) :location)))
         (protected (when (listp specs) (plist-get (cdr specs) :protected)))
         (init-func (intern (format "%S/init-%S" lname pname)))
         (pre-init-func (intern (format "%S/pre-init-%S" lname pname)))
         (post-init-func (intern (format "%S/post-init-%S" lname pname)))
         (copyp (not (null obj)))
         (obj (if obj obj (ddls-package pname-str :name pname)))
         (ownerp (or (and (eq 'dotfile lname)
                          (null (oref obj :owners)))
                     (fboundp init-func))))
    (when min-version
      (ddls-package/set-property2 obj :min-version (version-to-list min-version)))
    (when step
      (ddls-package/set-property2 obj :step step))
    (when toggle
      (ddls-package/set-property2 obj :toggle toggle))
    (when (and ownerp requires)
      (ddls-package/set-property2 obj :requires requires))
    (ddls-package/set-property2 obj :excluded
                               (and (ddls/layer-used-p lname)
                                    (or excluded (oref obj :excluded))))
    (when location
      (if (and (listp location)
               (eq (car location) 'recipe)
               (eq (plist-get (cdr location) :fetcher) 'local))
          (cond
           (layer (let ((path (expand-file-name
                               (format "%s%s"
                                       (ddls-layer/get-local-directory layer)
                                       pname-str))))
                    (ddls-package/set-property2
                     obj :location `(recipe :fetcher file :path ,path))))
           ((eq 'dotfile lname) nil))
        (ddls-package/set-property2 obj :location location)))
    ;; cannot override protected packages
    (unless copyp
      ;; a bootstrap package is protected
      (ddls-package/set-property2
       obj :protected (or protected (eq 'bootstrap step)))
      (when protected
        (add-to-list 'ddls--protected-packages pname)))
    (when ownerp
      ;; warn about mutliple owners
      (when (and (oref obj :owners)
                 (not (memq lname (oref obj :owners))))
        (ddls//warning
         (format (concat "More than one init function found for "
                         "package %S. Previous owner was %S, "
                         "replacing it with layer %S.")
                 pname (car (oref obj :owners)) lname)))
      ;; last owner wins over the previous one
      (object-add-to-list obj :owners lname))
    ;; check consistency between package and defined init functions
    (unless (or ownerp
                (eq 'dotfile lname)
                (eq 'system lname)
                (fboundp pre-init-func)
                (fboundp post-init-func)
                (oref obj :excluded))
      (ddls//warning
       (format (concat "package %s not initialized in layer %s, "
                       "you may consider removing this package from "
                       "the package list or use the :toggle keyword "
                       "instead of a `when' form.")
               pname lname)))
    ;; check if toggle can be applied
    (when (and (not ownerp)
               (and (not (eq 'unspecified toggle))
                    toggle))
      (ddls//warning
       (format (concat "Ignoring :toggle for package %s because "
                       "layer %S does not own it.")
               pname lname)))
    ;; check if requires can be applied
    (when (and (not ownerp) requires)
      (ddls//warning
       (format (concat "Ignoring :requires for package %s because "
                       "layer %S does not own it.")
               pname lname)))
    (when (fboundp pre-init-func)
      (object-add-to-list obj :pre-layers lname))
    (when (fboundp post-init-func)
      (object-add-to-list obj :post-layers lname))
    obj))

(defun ddls/get-indexed-package (name)
  "Return a package object with name NAME.
Return nil if package object is not found."
  (when (ht-contains? ddls--indexed-packages name)
    (ht-get ddls--indexed-packages name)))

(defun ddls//get-package-name-from-specs (specs)
  "Return the name symbol of package given the passed SPECS."
  (if (listp specs) (car specs) specs))

(defun ddls/package-used-p (name)
  "Return non-nil if NAME is the name of a used package."
  (let ((obj (ddls/get-indexed-package name)))
    (and obj (ddls-package/get-safe-owner obj)
         (not (oref obj :excluded))
         (not (memq nil (mapcar
                         'ddls/package-used-p
                         (oref obj :requires)))))))


;; System Functions

(defun ddls/test ()
  (interactive)
  (setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
  (let ((start-time (current-time))
        (ddls--inhibit-warnings t)
        (dotspacemacs-configuration-layers
         '(
           python
           spacemacs-defaults
           spacemacs-completion
           spacemacs-layouts
           spacemacs-editing
           spacemacs-editing-visual
           spacemacs-evil
           spacemacs-language
           spacemacs-misc
           spacemacs-modeline
           spacemacs-navigation
           spacemacs-org
           spacemacs-project
           spacemacs-purpose
           spacemacs-visual
           neotree
           )))
    ;; reinit data
    (setq ddls--indexed-layers (make-hash-table :size 1024))
    (setq ddls--used-layers nil)
    (setq ddls--indexed-packages (make-hash-table :size 2048))
    (setq ddls--used-packages nil)
    ;; execute test
    (setq ptime (current-time))
    (ddls/load-spacemacs-bfc)
    (message ">>>>>>>> load BFC time: %.3fs" (float-time (time-subtract (current-time) ptime)))
    (setq ptime (current-time))
    (ddls/set-used-layers-specs)
    (message ">>>>>>>> layer specs time: %.3fs" (float-time (time-subtract (current-time) ptime)))
    (setq ptime (current-time))
    (dotimes (i 5) (ddls/load-used-layers))
    (message ">>>>>>>> layer loading time: %.3fs" (float-time (time-subtract (current-time) ptime)))
    (message ">>>>>>>> total elapsed time: %.3fs"
             (float-time (time-subtract (current-time) start-time))))
  (setq gc-cons-threshold (car dotspacemacs-gc-cons) gc-cons-percentage (cadr dotspacemacs-gc-cons)))

;; (let (timer-idle-list timer-list file-name-handler-alist emacs-lisp-mode-hook auto-mode-alist)
;;   (profiler-start 'cpu)
;;   (ddls/test)
;;   (profiler-report)
;;   (profiler-stop))
;;(dotimes (i 100) (ddls/test))

;; BFC

(defun ddls/generate-spacemacs-bfc ()
  "Generate the Spacemacs big configuration file."
  (interactive)
  ;; layers
  (setq ddls--indexed-layers (make-hash-table :size 1024))
  (ddls//index-layers-from-directory (list ddls-layers-directory))
  ;; packages
  (dolist (lname (ht-keys ddls--indexed-layers))
    (let ((layer (ddls/get-indexed-layer lname)))
      (ddls-layer/initialize layer)))
  ;; write big configuration file
  (spacemacs/dump-vars-to-file '(ddls--indexed-layers
                          ddls--indexed-packages)
                        ddls-spacemacs-bigfile))

(defun ddls/load-spacemacs-bfc ()
  "Load the Spacemacs big configuration file."
  (load ddls-spacemacs-bigfile))

(defun ddls//add-indexed-layer (layer)
  "Index a LAYER object."
  (puthash (oref layer :name) layer ddls--indexed-layers))

(defun ddls/get-indexed-layer (layer-name)
  "Return a layer object with name LAYER-NAME.
Return nil if layer object is not found."
  (when (ht-contains? ddls--indexed-layers layer-name)
    (ht-get ddls--indexed-layers layer-name)))

;; Layers

(defun ddls/index-layers ()
  "Index all discovered layers in layer directories."
  ;; load cached index layers first
  ;; then crawl file system for user's private layers
  (let ((dirs `(;; layers in private folder ~/.emacs.d/private
                ,ddls-private-directory
                ;; layers in dotdirectory
                ;; this path may not exist, so check if it does
                ,(when dotspacemacs-directory
                   (let ((dir (expand-file-name (concat dotspacemacs-directory
                                                        "layers/"))))
                     (when (file-exists-p dir) (list dir))))
                ;; additional layer directories provided by the user
                ,dotspacemacs-configuration-layer-path)))
    (ddls//index-layers-from-directory dirs)))

(defun ddls//index-layers-from-directory (directories)
  "Index all discovered layers in DIRECTORIES."
  (let ((search-paths directories)
        (discovered '()))
    ;; filter out directories that don't exist
    (setq search-paths
          (ddls/filter-objects
           search-paths
           (lambda (x)
             (when x
               (let ((exists (file-exists-p x)))
                 (unless exists
                   (ddls//warning
                    "Layer directory \"%s\" not found. Ignoring it." x))
                 exists)))))
    ;; depth-first search of subdirectories
    (while search-paths
      (let ((current-path (car search-paths)))
        (setq search-paths (cdr search-paths))
        (dolist (sub (directory-files current-path t nil 'nosort))
          ;; ignore ".", ".." and non-directories
          (unless (or (string-equal ".." (substring sub -2))
                      (string-equal "." (substring sub -1))
                      (not (file-directory-p sub)))
            (let ((type (ddls//directory-type sub)))
              (cond
               ((eq 'category type)
                (let ((category (ddls//get-category-from-path
                                 sub)))
                  (spacemacs-buffer/message "-> Discovered category: %S" category)
                  (push category ddls-categories)
                  (setq search-paths (cons sub search-paths))))
               ((eq 'layer type)
                (let* ((layer-name (intern (file-name-nondirectory sub)))
                       (indexed-layer (ddls/get-indexed-layer layer-name)))
                  (if indexed-layer
                      (ddls//warning
                       (concat
                        "Duplicated layer %s detected in directory \"%S\", "
                        "replacing old directory \"%s\" with new directory.")
                       layer-name sub (oref indexed-layer :dir))
                    (spacemacs-buffer/message
                     "-> Discovered configuration layer: %S" layer-name)
                    (ddls//add-indexed-layer
                     (ddls/make-indexed-layer layer-name sub)))))
               (t
                ;; layer not found, add it to search path
                (setq search-paths (cons sub search-paths)))))))))))

(defun ddls/set-used-layers-specs ()
  "Read used layers specs and set slots of corresponding indexed layers."
  ;; (dotspacemacs|call-func dotspacemacs/layers "Calling dotfile layers...")
  (let ()
    (unless ddls-exclude-all-layers
      (dolist (specs dotspacemacs-configuration-layers)
        (let* ((name (ddls//get-layer-name-from-specs specs))
               (layer (ddls/get-indexed-layer name)))
          (if (null layer)
              (ddls//warning
               "Unknown layer '%S' declared in dotfile." name)
            (ddls-layer/set-specs layer specs)
            (ddls-layer/mark-as-used layer)))))))

(defun ddls/load-used-layers ()
  "Load the used layers."
  (dolist (lname ddls--used-layers)
    (let ((layer (ddls/get-indexed-layer lname)))
      (ddls-layer/load layer)
      (dolist (pname (oref layer :selected-packages))
        (ddls-package/mark-as-used (ddls/get-indexed-package pname))))))

(defun ddls//add-indexed-package (package)
  "Index a PACKAGE object."
  (puthash (oref package :name) package ddls--indexed-packages))



;; Util Functions

(defun ddls//directory-type (path)
  "Return the type of directory pointed by PATH.
Possible return values:
  layer    - the directory is a layer
  category - the directory is a category
  nil      - the directory is a regular directory."
  (when (file-directory-p path)
    (if (string-match
         "^+" (file-name-nondirectory
               (directory-file-name
                (concat ddls-layers-directory path))))
        'category
      (let ((files (directory-files path)))
        (when (member ddls--packages-layer-file files)
          'layer)))))

(defun ddls//get-category-from-path (dirpath)
  "Return a category symbol from the given DIRPATH.
The directory name must start with `+'.
Returns nil if the directory is not a category."
  (when (file-directory-p dirpath)
    (let ((dirname (file-name-nondirectory
                    (directory-file-name
                     (concat ddls-layers-directory
                             dirpath)))))
      (when (string-match "^+" dirname)
        (intern (substring dirname 1))))))

(defun ddls//get-directory-name (filepath)
  "Return the name of the parent directory for passed FILEPATH"
  (file-name-nondirectory (directory-file-name (file-name-directory filepath))))

(defun ddls/filter-objects (objects ffunc)
  "Return a filtered OBJECTS list where each element satisfies FFUNC."
  (reverse (cl-reduce (lambda (acc x) (if (funcall ffunc x) (push x acc) acc))
                      objects
                      :initial-value nil)))

(defun ddls//warning (msg &rest args)
  "Display MSG as a warning message in buffer `*Messages*'.
If `ddls--inhibit-warnings' is non nil then this function is a
no-op."
  (unless ddls--inhibit-warnings (apply 'spacemacs-buffer/warning msg args)))

(provide 'core-double-dot-layer-system)
