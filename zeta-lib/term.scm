(define-module (zeta-lib term)
  #:export (println
	    usage
	    error-with-msg
	    info-with-msg))

(define (println str)
  (display str)
  (newline))

(define (usage)
  (println "Usage: zeta CMD [-m|--manifest manifest] PACKAGE ... | MANIFEST ...")
  (format #t "Subcommands: \

- zeta intall [-m | --manifest MANIFEST] PKG ...
  Installs the specified packages at the specified manifest, if
  specified, or prompts to select one from $ZETA_ROOT

- zeta remove [-m | --manifest MANIFEST] PKG ...
  Same as zeta install, but for removing packages.

- zeta add MANIFEST ...
  Creates a new manifest under MANIFEST ... (which is interpreted as
  a suffix-less path relative to $ZETA_ROOT) and adds it to
  root.scm. For example: zeta add foo/bar would create and add the
  manifest ~/.zeta/foo/bar.scm to root.scm, assuming that
  $ZETA_ROOT is left at its default value (and not otherwise
  modified programatically, via %zeta-root)
  
- zeta del MANIFEST ...
  Same as zeta add, except for deleting manifest.

- zeta list
  Lists all installed packages and their respective manifests.

- zeta apply
  Applies the root manifest.

- zeta init
  Creates the directory $ZETA_ROOT and root.scm in it, which is
  initially just a \"skeleton\" to later load manifests into.
  This command usually does not need to be invoked manually.")
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
