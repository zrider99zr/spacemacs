;;; core-configuration-layer-ftest.el --- Spacemacs Functional Test File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'core-configuration-layer)

;; ---------------------------------------------------------------------------
;; configuration-layer//declare-used-layers
;; ---------------------------------------------------------------------------

(ert-deftest test-declare-layers--bootstrap-layer-always-first ()
  (let ((dotspacemacs-distribution 'spacemacs)
        (dotspacemacs-configuration-layers '(emacs-lisp
                                             (git :variables foo 'bar)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer/discover-layers 'refresh-index)
    (configuration-layer//declare-used-layers dotspacemacs-configuration-layers)
    (should (eq 'spacemacs-bootstrap
                (first configuration-layer--used-layers)))))

(ert-deftest test-declare-layers--defaults-layer-is-second-for-base-distribution ()
  (let ((dotspacemacs-distribution 'spacemacs-base)
         (dotspacemacs-configuration-layers '(emacs-lisp
                                               (git :variables foo 'bar)))
         configuration-layer--used-layers
         (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer/discover-layers 'refresh-index)
    (configuration-layer//declare-used-layers dotspacemacs-configuration-layers)
    (should (eq 'spacemacs-defaults (second configuration-layer--used-layers)))))

(ert-deftest test-declare-layers--base-layer-is-third-for-base-distribution ()
  (let ((dotspacemacs-distribution 'spacemacs-base)
        (dotspacemacs-configuration-layers '(emacs-lisp
                                             (git :variables foo 'bar)))
        configuration-layer--used-layers
        (configuration-layer--indexed-layers (make-hash-table :size 1024)))
    (configuration-layer/discover-layers 'refresh-index)
    (configuration-layer//declare-used-layers dotspacemacs-configuration-layers)
    (should (eq 'spacemacs-base (third configuration-layer--used-layers)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//stable-elpa-verify-archive
;; ---------------------------------------------------------------------------

(defvar test-stable-elpa-verify-archive--verification-ok-error nil)
(ert-deftest test-stable-elpa-verify-archive--verification-ok ()
  (let (result)
    (cl-letf (((symbol-function 'configuration-layer//stable-elpa-tarball-local-file)
               (lambda ()
                 (concat spacemacs-test-directory
                         "core/data/signed-test-stable-elpa.tar.gz")))
              ((symbol-function 'configuration-layer//stable-elpa-tarball-local-sign-file)
               (lambda ()
                 (concat spacemacs-test-directory
                         "core/data/signed-test-stable-elpa.tar.gz.sig")))
              ((symbol-function 'configuration-layer//stable-elpa-ask-to-continue)
               (lambda (x)
                 (setq test-stable-elpa-verify-archive--verification-ok-error x)
                 nil))
              ((symbol-function 'configuration-layer//error)
               (lambda (x)
                 (setq fatal-error x)
                 nil))
              ((symbol-function 'message) 'ignore))
      (setq result (configuration-layer//stable-elpa-verify-archive)))
    (should (if (null test-stable-elpa-verify-archive--verification-ok-error)
                result
              (message "Verification error was: %s"
                       test-stable-elpa-verify-archive--verification-ok-error)
              nil))))

(defvar test-stable-elpa-verify-archive--verification-failed-error nil)
(ert-deftest test-stable-elpa-verify-archive--verification-failed ()
  (let (result)
    (cl-letf (((symbol-function 'configuration-layer//stable-elpa-tarball-local-file)
               (lambda ()
                 (concat spacemacs-test-directory
                         "core/data/test-stable-elpa.tar.gz")))
              ((symbol-function 'configuration-layer//stable-elpa-tarball-local-sign-file)
               (lambda ()
                 (concat spacemacs-test-directory
                         "core/data/signed-test-stable-elpa.tar.gz.sig")))
              ((symbol-function 'configuration-layer//stable-elpa-ask-to-continue)
               (lambda (x)
                 (setq test-stable-elpa-verify-archive--verification-failed-error x)
                 nil))
              ((symbol-function 'configuration-layer//error)
               (lambda (x)
                 (setq fatal-error x)
                 nil))
              ((symbol-function 'message) 'ignore))
      (setq result (null (configuration-layer//stable-elpa-verify-archive))))
    (should
     (cond
      ((and result
            (string-match-p "^Verification failed!.*"
                            test-stable-elpa-verify-archive--verification-failed-error)) t)
      (t (message "Verification error was: %s"
                  test-stable-elpa-verify-archive--verification-failed-error)
         nil)))))
