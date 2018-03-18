;;; binder.el -- Key binding macro -*- lexical-binding: t -*-

(require 'seq)

;; (eval-when-compile
;;   (require 'cl-generic)
;;   (require 'cl-lib))
(require 'cl-generic)
(require 'cl-lib)

;; =============================================================================
;; Stubs

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

;; =============================================================================
;; Implementation details

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

;; =============================================================================
;; To implement a new binding type add corresponding method here:

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

;; =============================================================================
;; macro

(defmacro spacemacs|bind (provider &rest bindings)
  "Key-binding macro.
If package PROVIDER enabled bind keys and prefixes from BINDINGS.
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
                :rsexp `(when (stubmax/package-used-p ',provider))))))

;;==============================================================================
;; Test

;; (spacemacs|bind
;;   everyoneneedsme
;;   :major
;;    (awesome-mode
;;     ("e" "playground"
;;      ("r" awesome-palyground "awesome playground"))
;;     ("t" "test"
;;      ("a" spacemacs/awesome-run-test-all "run all test")
;;      ("u" "unit"
;;       ("m" spacemacs/awesome-run-unit-test-module "utest module")
;;       ("p" spacemacs/awesome-run-unit-test-project "utest project"))
;;      ("g" "generative"
;;       ("r" spacemacs/awesome-run-generative-test "gentest"))))
;;    (epic-mode
;;     ("e" "playground"
;;      ("r" epic-palyground "epic playground")))
;;   :minor
;;    (mediocre-mode
;;     ("b" "boring"
;;      ("d" do-boring-stuff "Zzz..")))
;;   :global
;;    ("t" "toggle"
;;     ("c" "celestial"
;;      ("m" spacemacs/toggle-moon "on/off Moon")
;;      ("s" spacemacs/toggle-sun "on/off Sun"))))

;; Log from stubs:
;; Testing if "everyoneneedsme" package is used.
;; Used packages: (foo everyoneneedsme bar)
;; (stubmax/declare-prefix-for-major-mode awesome-mode "me" "playground")
;; (stubmax/bind-key-for-major-mode awesome-mode "mer" awesome-palyground "awesome playground")
;; (stubmax/declare-prefix-for-major-mode awesome-mode "mt" "test")
;; (stubmax/bind-key-for-major-mode awesome-mode "mta" spacemacs/awesome-run-test-all "run all test")
;; (stubmax/declare-prefix-for-major-mode awesome-mode "mtu" "unit")
;; (stubmax/bind-key-for-major-mode awesome-mode "mtum" spacemacs/awesome-run-unit-test-module "utest module")
;; (stubmax/bind-key-for-major-mode awesome-mode "mtup" spacemacs/awesome-run-unit-test-project "utest project")
;; (stubmax/declare-prefix-for-major-mode epic-mode "me" "playground")
;; (stubmax/bind-key-for-major-mode epic-mode "mer" epic-palyground "epic playground")
;; (stubmax/declare-prefix-for-minor-mode mediocre-mode "b" "boring")
;; (stubmax/bind-key-for-minor-mode mediocre-mode "bd" do-boring-stuff "Zzz..")
;; (stubmax/declare-prefix-global "t" "toggle")
;; (stubmax/declare-prefix-global "tc" "celestial")
;; (stubmax/bind-key-global "tcm" spacemacs/toggle-moon "on/off Moon")
;; (stubmax/bind-key-global "tcs" spacemacs/toggle-sun "on/off Sun")

;; Macroexpand =>
;; (if (stubmax/package-used-p (quote everyoneneedsme))
;;     (progn (stubmax/declare-prefix-for-major-mode (quote awesome-mode) "me" "playground")
;;            (stubmax/bind-key-for-major-mode (quote awesome-mode) "mer" (quote awesome-palyground) "awesome playground")
;;            (stubmax/declare-prefix-for-major-mode (quote awesome-mode) "mt" "test")
;;            (stubmax/bind-key-for-major-mode (quote awesome-mode) "mta" (quote spacemacs/awesome-run-test-all) "run all test")
;;            (stubmax/declare-prefix-for-major-mode (quote awesome-mode) "mtu" "unit")
;;            (stubmax/bind-key-for-major-mode (quote awesome-mode) "mtum" (quote spacemacs/awesome-run-unit-test-module) "utest module")
;;            (stubmax/bind-key-for-major-mode (quote awesome-mode) "mtup" (quote spacemacs/awesome-run-unit-test-project) "utest project")
;;            (stubmax/declare-prefix-for-major-mode (quote epic-mode) "me" "playground")
;;            (stubmax/bind-key-for-major-mode (quote epic-mode) "mer" (quote epic-palyground) "epic playground")
;;            (stubmax/declare-prefix-for-minor-mode (quote mediocre-mode) "b" "boring")
;;            (stubmax/bind-key-for-minor-mode (quote mediocre-mode) "bd" (quote do-boring-stuff) "Zzz..")
;;            (stubmax/declare-prefix-global "t" "toggle") (stubmax/declare-prefix-global "tc" "celestial")
;;            (stubmax/bind-key-global "tcm" (quote spacemacs/toggle-moon) "on/off Moon")
;;            (stubmax/bind-key-global "tcs" (quote spacemacs/toggle-sun) "on/off Sun")))
