;;; core-ddls-utest.el --- Spacemacs Unit Test File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;;          Eugene Yaremenko <>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'mocker)
(require 'core-command-line)
(require 'core-double-dot-layer-system)

(defun helper--add-layers (layers &optional usedp)
  "Set the layer variables given a list of LAYERS objects."
  (dolist (layer layers)
    (ddls//add-indexed-layer layer)
    (when usedp
      (ddls-layer/mark-as-used layer)))
  ;; hackish but we need to reverse the list in order to have the layer
  ;; in the correct order (this reverse in normally performed in function
  ;; ddls/set-used-layers-specs
  (when usedp
    (setq ddls--used-layers
          (reverse ddls--used-layers))))


;; ddls//get-directory-name

(ert-deftest test-get-directory-name--returns-name ()
  (should (string-equal
           (ddls//get-directory-name
            "/home/user/.emacs.d/layers/+lang/emacs-lisp/autoload.el")
           "emacs-lisp")))


;; ddls//get-layer-name-from-current-file

(ert-deftest test-get-layer-name-from-current-file--loaded-file ()
  (let ((load-file-name
         "/home/user/.emacs.d/layers/+lang/emacs-lisp/autoload.el"))
    (should (string-equal
             (ddls//get-layer-name-from-current-file)
             "emacs-lisp"))))

(ert-deftest test-get-layer-name-from-current-file--evaluated-file ()
  (let (load-file-name
        (default-directory "/home/user/.emacs.d/layers/+lang/emacs-lisp/"))
    (should (string-equal
             (ddls//get-layer-name-from-current-file)
             "emacs-lisp"))))


;; ddls//get-layer-from-current-file

(ert-deftest test-get-layer-from-current-file--returns-used-layer ()
  (let (ddls--used-layers
        (ddls--indexed-layers (make-hash-table :size 1024))
        (load-file-name
         "/home/user/.emacs.d/layers/+lang/usedlayer/autoload.el"))
    (helper--add-layers `(,(ddls-layer "usedlayer" :name 'usedlayer)) 'used)
    (should (eq (ddls//get-layer-from-current-file)
                (ddls/get-indexed-layer 'usedlayer)))))

(ert-deftest test-get-layer-from-current-file--returns-not-used-layer ()
  (let (ddls--used-layers
        (ddls--indexed-layers (make-hash-table :size 1024))
        (load-file-name
         "/home/user/.emacs.d/layers/+lang/notusedlayer/autoload.el"))
    (helper--add-layers `(,(ddls-layer "notusedlayer" :name 'notusedlayer)))
    (should (eq (ddls//get-layer-from-current-file)
                (ddls/get-indexed-layer 'notusedlayer)))))


;; ddls//create-extensions-regexp
