(key-bindings: anaconda-mode
  :major
   (python-mode
    ("g" "goto"
     ("a" anaconda-mode-find-assignments "go to variable assignments")
     ("b" anaconda-mode-go-back "go to last position")
     ("u" anaconda-mode-find-references "find references"))
    ("h" "help"
     ("h" anaconda-mode-show-doc "show documentation of symbol around point"))))

(key-bindings: company-anaconda)

(key-bindings: cython-mode
  :major
   (cython-mode
    ("g" "goto"
     ("u" anaconda-mode-find-references "find references")
     ("h" "help/documentation"
      ("h" anaconda-mode-show-doc "show documentation")))))

(key-bindings: helm-pydoc
  :major
   (python-mode
    ("h" "help/documentation"
     ("d" helm-pydoc "look for documentation using pydoc and helm."))))

(key-bindings: hy-mode
  :major
   (hy-mode
    ("e" "evaluate"
     ("e" lisp-eval-last-sexp "evaluate expression before point")
     ("f" lisp-eval-defun "evaluate function around point")
     ("F" lisp-eval-defun "evaluate function around point and ???")
     ("r" lisp-eval-defun "evaluate region")
     ("R" lisp-eval-defun "evaluate region and ???"))
    ("s" "REPL"
     ("B" switch-to-lisp "send buffer to REPL and focus the REPL buffer")
     ("b" lisp-load-file "send buffer to REPL")
     ("i" inferior-lisp "open REPL"))))

(key-bindings: importmagic
  :major
   (python-mode
    ("r" "refactor"
     "f" importmagic-fix-symbol-at-point "fix import around point")))

(key-bindings: live-py-mode
  :major
   (python-mode ("l" live-py-mode "open live py-mode")))

(key-bindings: lsp-python)

(key-bindings: nose
  :major
   (python-mode
    ("t" "test"
     ("A" spacemacs/python-test-pdb-all "run all tests with pdb")
     ("a" spacemacs/python-test-all "run all tests")
     ("B" spacemacs/python-test-pdb-module "run tests in buffer with pdb")
     ("b" spacemacs/python-test-module "run tests in buffer")
     ("l" spacemacs/python-test-last "run last tests")
     ("T" spacemacs/python-test-pdb-one "run test around point with pdb")
     ("t" spacemacs/python-test-one "run test around point")
     ("M" spacemacs/python-test-pdb-module "run tests of module with pdb")
     ("m" spacemacs/python-test-module "run tests of module")
     ("S" spacemacs/python-test-pdb-suite "run tests of suite with pdb")
     ("s" spacemacs/python-test-suite "run tests of suite"))))

(key-bindings: pip-requirements)

(key-bindings: pipenv)

(key-bindings: pippel
  :major
   (python-mode ("P" pippel-list-packages)))

(key-bindings: py-isort
  :major
   (python-mode
    ("r" "refactor"
     ("I" py-isort-buffer "sort imports"))))

(key-bindings: pyenv-mode
  :major
   (python-mode
    ("v" "virtual env"
     ("s" pyenv-mode-set "???")
     ("u" pyenv-mode-unset "???"))))

(key-bindings: pylookup
  :major
   (python-mode
    ("h" "help"
     ("H" pylookup-lookup "use pylookup to display documentation of symbol around point"))))

(key-bindings: pytest
  :major
   (python-mode
    ("t" "test"
     ("A" spacemacs/python-test-pdb-all "run all tests with pdb")
     ("a" spacemacs/python-test-all "run all tests")
     ("B" spacemacs/python-test-pdb-module "run tests in buffer with pdb")
     ("b" spacemacs/python-test-module "run tests in buffer")
     ("l" spacemacs/python-test-last "run last tests")
     ("T" spacemacs/python-test-pdb-one "run test around point with pdb")
     ("t" spacemacs/python-test-one "run test around point")
     ("M" spacemacs/python-test-pdb-module "run tests of module with pdb")
     ("m" spacemacs/python-test-module "run tests of module")
     ("S" spacemacs/python-test-pdb-suite "run tests of suite with pdb")
     ("s" spacemacs/python-test-suite "run tests of suite"))))

(key-bindings: python
  :major
   (python-mode
    ("c" "compile/execute"
     ("cc" spacemacs/python-execute-file "execute file")
     ("cC" spacemacs/python-execute-file-focus "execute file and ???"))
    ("d" "debug"
     ("b" spacemacs/python-toggle-breakpoint "toggle breakpoint on current line"))
    ("r" "refactor"
     ("i" spacemacs/python-remove-unused-imports "remove unused import"))
    ("s" "REPL"
     ("sB" spacemacs/python-shell-send-buffer-switch "send buffer to REPL and focus it")
     ("sb" python-shell-send-buffer "send buffer to REPL")
     ("sF" spacemacs/python-shell-send-defun-switch "send function around point to REPL and focus it")
     ("sf" python-shell-send-defun "send function around point to REPL")
     ("si" spacemacs/python-start-or-switch-repl "start REPL")
     ("sR" spacemacs/python-shell-send-region-switch "send region to REPL and focus it")
     ("sr" python-shell-send-region "send region to REPL"))))

(key-bindings: pyvenv
  :major
   (python-mode
    ("v" "virtual env"
     ("a" pyvenv-activate "activate virtual env")
     ("d" pyvenv-deactivate "deactivate virtual env")
     ("w" pyvenv-workon "???"))))

(key-bindings: yapfify
  :major
   (python-mode
    ("=" yapfify-buffer "format buffer with yapfify")))
