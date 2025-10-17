(use-modules (srfi srfi-64)
	     (gideon-lib cmds)
	     (gideon-lib system)
	     (gideon-lib term))

(%gideon-root (string-append "." "/test_root"))
(%root-manifest (string-append (%gideon-root) "/root.scm"))
(%dry-run? #t)

(test-begin "gideon-install")
(define test-manifest "foo")

(gideon-add (list test-manifest))
(gideon-install '("hello" "cowsay" "sl") test-manifest)
(test-equal
     (read-pkgs (relative->absolute test-manifest (%gideon-root)))
  (list "hello" "cowsay" "sl"))

(test-equal
    (read-manifests (%root-manifest))
  (list (relative->absolute "foo" (%gideon-root))))
(test-end "gideon-install")


(test-begin "gideon-remove")
(gideon-remove '("hello" "cowsay") test-manifest)

(test-equal
    (read-pkgs (relative->absolute test-manifest (%gideon-root)))
	       '("sl"))
(test-end "gideon-remove")

(test-begin "gideon-list")
(test-equal
    (call-with-output-string 
      gideon-list)
  "sl      ./test_root/foo.scm\n")
(test-end "gideon-list")

(test-begin "gideon-del")
(gideon-del (list test-manifest))

(test-equal
    (read-manifests (%root-manifest))
  '())
(test-end "gideon-del")



(system* "rm" "-r" (%gideon-root))
