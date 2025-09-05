(use-modules (srfi srfi-64)
	     (zeta-lib cmds)
	     (zeta-lib system)
	     (zeta-lib term))

;; (set! %zeta-root (string-append "." "/test_root"))
;; (set! %root-manifest (string-append %zeta-root "/root.scm"))
;; (set! %dry-run? #t)
;; &&& change

(%zeta-root (string-append "." "/test_root"))
(%root-manifest (string-append (%zeta-root) "/root.scm"))
(%dry-run? #t)

;; (when (file-exists? %zeta-root)
;;   (system* "rm" "-r" (%zeta-root)))

(test-begin "zeta-install")
(define test-manifest "foo")

(zeta-add (list test-manifest))
(zeta-install '("hello" "cowsay" "sl") test-manifest)
(test-equal
     (read-pkgs (relative->absolute test-manifest (%zeta-root)))
  (list "hello" "cowsay" "sl"))

(test-equal
    (read-manifests (%root-manifest))
  (list (relative->absolute "foo" (%zeta-root))))
(test-end "zeta-install")


(test-begin "zeta-remove")
(zeta-remove '("hello" "cowsay") test-manifest)

(test-equal
    (read-pkgs (relative->absolute test-manifest (%zeta-root)))
	       '("sl"))
(test-end "zeta-remove")

(test-begin "zeta-list")
(test-equal
    (call-with-output-string 
      zeta-list)
  "sl      ./test_root/foo.scm\n")
(test-end "zeta-list")

(test-begin "zeta-del")
(zeta-del (list test-manifest))

(test-equal
    (read-manifests (%root-manifest))
  '())
(test-end "zeta-del")



(system* "rm" "-r" (%zeta-root))
