(define-module (zeta-lib system)
  #:use-module (ice-9 match)
  #:use-module (zeta-lib term)
  #:export (%zeta-root
	    %root-manifest
	    %rebuild?
	    %dry-run?
	    apply-root-manifest
	    relative->absolute
	    get-zeta-root
	    set-zeta-root
	    mkdir-p
	    touch
	    make-file-at-path
	    make-scm-file-at-path
	    read-file
	    write-file
	    read-pkgs
	    read-manifests
	    manifest-with-pkgs
	    root-with-manifests
	    define-recursive))


(define %zeta-root (or (getenv "ZETA_ROOT")
		       (string-append (getenv "HOME") "/.zeta")
		       (error-with-msg "Something went seriously wrong. $HOME environment variable cannot be read.")))

(define %root-manifest
  (string-append %zeta-root "/root.scm"))

(define %rebuild? #f)

(define %dry-run? #f)

(define* (apply-root-manifest #:optional (root-file %root-manifest))
  (unless (eq? (system* "guix" "package" "-m" root-file) 0)
    (error-with-msg "Guix package command failed. Make sure `guix` is properly installed and a network connection is available."
		    ;; TODO: Add facility to pass different args to `guix`
		    )))

(define* (relative->absolute rel-path #:optional (root-path %zeta-root))
  (string-append root-path
		 "/" rel-path
		 ".scm"))

(define (mkdir-p path)
  (system* "mkdir" "-p" path))

(define (touch filename)
  (system* "touch" filename))

(define (make-file-at-path path filename)
  (mkdir-p path)
  (touch (string-append path "/" filename)))

(define (make-scm-file-at-path path filename)
  (make-file-at-path path (string-append filename ".scm")))

(define (write-file filename str)
  (call-with-output-file filename
    (lambda (output-port)
      (display str output-port))))

(define (read-file filepath)
  (call-with-input-file filepath
    (lambda (input-port)
      (read input-port))))

(define* (read-pkgs manifest #:optional (pedantic #t))
  ;; "pedantic" arg: If #t, errors out on non-readable manifest, else interprets it as empty
  (match (read-file manifest)
    (('specifications->manifest
      ('quote
       (pkgs ...))) pkgs)
    (_ (if pedantic
	   (begin
	     (error-with-msg
	      (format #f "Cannot read package specifications from manifest ~a" manifest))
	     (error-with-msg
	      (format #f "Manifests must have the format (specifications->manifest '(spec1 spec2 ...))")))
	   '()))))

(define* (read-manifests root-file #:optional (pedantic #t))
  ;; "pedantic" arg: If #t, errors out on non-readable root, else interprets it as empty
  (match (read-file root-file)
    (('concatenate-manifests
      ('map ('lambda ('filepath)
	      ('primitive-eval
	       ('call-with-input-file 'filepath
		 ('lambda ('input-port)
		   ('read 'input-port)))))
	    ('quote (manifests ...)))) manifests)
    (_ (if pedantic
	   (begin (error-with-msg "Cannot read manifests from root file.")
		  ;; TODO: More helpful error. Maybe add command like `zeta fix` too fix broken root?
		  )
	   '()))))

(define (manifest-with-pkgs pkgs)
  (format #f "(specifications->manifest\n '(~a))"
	  (string-join
	   (map (lambda (pkg)
		  (format #f "\"~a\"" pkg))
		pkgs)
	   "\n   ")))

(define (root-with-manifests manifests)
  (format #f
"(concatenate-manifests
  (map (lambda (filepath)
     (primitive-eval
       (call-with-input-file filepath
         (lambda (input-port)
           (read input-port)))))
  '(~a)))"
	  (string-join
	   (map (lambda (manifest)
		  (format #f "\"~a\"" manifest))
		manifests)
	   "\n    ")))


(define-syntax define-recursive
  ;; Macro for simplifying the definition of procedures that act on lists recursively.
  ;; First argument MUST be a list.
  (lambda (x)
    (syntax-case x (single recurse finish)
      ((define-recursive (proc-name list-arg extra-arg ...)
	 (single identifier)
	 exp ...
	 ;; Recurse with arguments (cdr list-arg) recurse-arg ... 
	 (recurse recurse-arg ...)
	 ;; If finished, evaluate expressions finish-exp ...
	 (finish finish-exp ...))
       #'(define (proc-name list-arg ...)
	   (define recurse? (not (nil? (cdar (list list-arg)))))
	   ;; `item` is introduced as a binding for a "single element" of the list
	   (let ((identifier (caar (list list-arg))))
	     exp ...
	     (if recurse?
		 ;; First arg is assumed to be (cdr list-arg).
		 (proc-name (cdr list-arg) recurse-arg ...)
		 (begin finish-exp ...))))
       ))))
