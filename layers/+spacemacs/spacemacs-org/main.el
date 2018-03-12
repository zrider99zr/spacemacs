(packages:
  flyspell
  ;; default-org package does not exist, we invent this package name
  ;; to allow the `org' layer to own the `org' package instead of this
  ;; layer. So it is easier for users to steal the ownership of the
  ;; `org' package.
  (default-org-config :location built-in)
  (org-plus-contrib :step pre)
  org-bullets
  (space-doc :location local)
  toc-org
  )
