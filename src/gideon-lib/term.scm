(define-module (gideon-lib term)
  #:export (println
	    usage
	    error-with-msg
	    warning-with-msg
	    info-with-msg))

(define (println str)
  (display str)
  (newline))

(define (usage)
  (println "Usage: gideon CMD [-m|--manifest manifest] PACKAGE ... | MANIFEST ...")
  (format #t "\
PACKAGEs are interpreted as it would be by `guix install`.
MANIFESTs are interpreted as suffix-less paths relative to $GIDEON_ROOT.
  Ex: foo/bar => $GIDEON_ROOT/foo/bar.scm

Subcommands:
- gideon intall [-m | --manifest MANIFEST] PKG ...
  Install PKG ... at manifest MANIFEST ... (or prompt for it, if not provided).

- gideon remove [-m | --manifest MANIFEST] PKG ...
  Remove  PKG ... from manifest MANIFEST ...
  Detects MANIFEST automatically if PKG is only present at one manifest.

- gideon add MANIFEST ...
  Creates new manifests MANIFEST ... and adds them to the root manifest.

- gideon del MANIFEST ...
  Deletes the manifests MANIFEST ... and deletes them from the root manifest.

- gideon list
  Lists all installed packages and their respective manifests.

- gideon apply
  Applies the root manifest.

- gideon rescan 
  Rescans root manifest from $GIDEON_ROOT directory tree.

- gideon purge
  Deletes all files in the directory tree $GIDEON_ROOT not specified in
  the root manifest.

- gideon init
  (Re-) Creates the directory $GIDEON_ROOT and root.scm in it.
  This command usually does not need to be invoked manually.
")
  (exit 1))

(define (colorize str color)
  (let ((terminate "\x1b[0m")
	(colors
	 '((red . "\x1b[1;31m")
	   (blue . "\x1b[1;34m")
	   (yellow . "\x1b[1;33m"))))
    (format #f "~a~a~a"
	    (or (assoc-ref colors color) (error "Color not defined."))
	    str
	    terminate)))

(define (error-with-msg msg)
  (display (colorize "ERROR: " 'red))
  (println msg)
  (exit 1))

(define (info-with-msg msg)
  (display (colorize "INFO: " 'blue))
  (println msg))

(define (warning-with-msg msg)
  (display (colorize "WARNING: " 'yellow))
  (println msg))
