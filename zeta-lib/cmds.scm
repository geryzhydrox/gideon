(define-module (zeta-lib cmds)
  #:use-module (zeta-lib prompts)
  #:use-module (zeta-lib system)
  #:use-module (zeta-lib term)
  #:use-module (ice-9 readline)
  #:export (zeta-apply
	    zeta-add
	    zeta-del
	    zeta-install
	    zeta-remove
	    zeta-list
	    zeta-rescan
	    zeta-purge
	    zeta-init))

(define (zeta-apply)
  (unless (%dry-run?) (apply-root-manifest)))

(define-recursive (zeta-add manifest-paths)
  (bind manifest-path)
  (when (not (file-exists? (%root-manifest)))
    (info-with-msg "No root manifest detected, creating new one...")
    (zeta-init #f))
  (let* ((slash-index (string-rindex manifest-path #\/))
	 (path (if slash-index
		   (string-append (%zeta-root) "/"
				  (string-take manifest-path (1+ slash-index)))
		   (%zeta-root)))
	 (filepath (relative->absolute manifest-path (%zeta-root))))
    ;; Abuse short-circuiting behaviour of `or` and `and` 
    (when (or (and (file-exists? filepath)
		   (yn-prompt (format #f "Manifest ~a already exists. Overwrite?" filepath))
		   (delete-file filepath)
		   (%rebuild? #t))
	      (not (file-exists? filepath)))
      (info-with-msg (format #f "Adding manifest ~a" filepath))
      (mkdir-p path)
      (touch filepath)
      (write-file filepath (manifest-with-pkgs '()))
      (let* ((manifests (read-manifests (%root-manifest)))
	     (new-manifests (if (member filepath manifests)
				(begin
				  (info-with-msg "Manifest already contained in root. Skipping...")
				  manifests)
				(append manifests (list filepath))))
	     (new-root (root-with-manifests new-manifests)))
	(write-file (%root-manifest) new-root))))
    (recurse
     (%rebuild?))
    (finish
     (when (%rebuild?) (zeta-apply))))

(define-recursive (zeta-del manifest-paths)
  (bind manifest-path)
  (let ((filepath (relative->absolute manifest-path (%zeta-root))))
    (unless (file-exists? filepath)
      (error-with-msg (format #f "Specified manifest ~a does not exist" filepath)))
    (info-with-msg (format #f "Deleting manifest ~a" filepath))
    (delete-file filepath)
    (let* ((manifests (read-manifests (%root-manifest)))
	   (new-manifests (if (member filepath manifests)
			      (delete filepath manifests)
			      (begin
				(info-with-msg "Manifest not contained in root. Skipping..."))))
	   (new-root (root-with-manifests new-manifests)))
      (write-file (%root-manifest) new-root)))
    (recurse
     (cdr manifest-paths))
    (finish
     (zeta-apply)))

(define-recursive (zeta-install pkgs manifest-path)
  (bind pkg)
  (when (not (file-exists? (%root-manifest)))
    (info-with-msg "Root manifest does not exist.")
    (zeta-init #f))
  (define creating-new-manifest? #f)
  (unless manifest-path
    (info-with-msg "No manifest specified")
    (let* ((answer (numbered-prompt "Install at:"  
				    (append (read-manifests (%root-manifest)) (list "Create new manifest"))))
	   (new-manifest-path (if (string= answer "Create new manifest") 
				  (begin (set! creating-new-manifest? #t) (readline "Create new manifest at: "))
				  (string-drop-right 
				   (string-drop answer (1+ (string-length (%zeta-root)))) 4))))
      (set! manifest-path new-manifest-path)))
  (let ((filepath (relative->absolute manifest-path (%zeta-root))))
    (when creating-new-manifest? (zeta-add (list manifest-path)))
    (unless (file-exists? filepath)
      (info-with-msg (format #f "Specified manifest ~a does not exist" filepath))
      (when (yn-prompt "Create manifest?") (zeta-add (list manifest-path))))
    (info-with-msg (format #f "Installing package ~a at manifest ~a" pkg filepath))
    (let* ((manifest-pkgs (read-pkgs filepath))
	   (new-pkgs (if (member pkg manifest-pkgs)
			 (begin
			   (info-with-msg "Package already installed. Skipping...")
			   manifest-pkgs)
			 (append manifest-pkgs (list pkg))))
	   (new-file (manifest-with-pkgs new-pkgs)))
      (write-file filepath new-file)))
  (recurse
   manifest-path)
  (finish
   (zeta-apply)))

(define-recursive (zeta-remove pkgs manifest-path)
  (bind pkg)
  (define available-manifests '())
  (define manifest-provided? manifest-path)
  (unless manifest-path
    (info-with-msg "No manifest specified")
    (ftw (%zeta-root)
	 (lambda (filename statinfo flag)
	   (when (and
		  (eq? flag 'regular)
		  (not (string= filename (%root-manifest)))
		  (member pkg (read-pkgs filename)))
	     (append! available-manifests (list filename)))
	   #t
	   ))
    (let ((answer (cond ((nil? available-manifests) #f)
			((equal? (length available-manifests) 1) (car available-manifests))
			(#t (numbered-prompt (format #f "Choose manifest to remove `~a` from:" pkg) available-manifests)))))
      (set! manifest-path
	    (if answer
		(string-drop-right 
		 (string-drop answer (1+ (string-length (%zeta-root)))) 4) 
		(error-with-msg "Specified package is not installed."))
	    )))
  (let ((filepath (relative->absolute manifest-path (%zeta-root))))
    (unless (file-exists? filepath)
	(error-with-msg (format #f "Specified manifest ~a does not exist" filepath)))
    (info-with-msg (format #f "Deleting package ~a from manifest ~a" pkg filepath))
    (let* ((manifest-pkgs (read-pkgs filepath))
	   (new-pkgs (if (member pkg manifest-pkgs)
			 (delete pkg manifest-pkgs)
			 (begin
			   (info-with-msg "Package not installed. Skipping...")
			   manifest-pkgs)))
	   (new-file (manifest-with-pkgs new-pkgs)))
      (write-file filepath new-file)
      ))
  (recurse
   (if manifest-provided?
       manifest-path
       #f))
  (finish
   (zeta-apply)))

(define* (zeta-init #:optional (manual #t))
  (define filepath (format #f "~a/root.scm" (%zeta-root)))
  (when (or
	 (not manual)   
	 (and
	  manual
	  (if (file-exists? (%zeta-root)) 
	      (yn-prompt "Root manifest already exists. Overwrite?")
	      #t)))
    (info-with-msg (format #f "Creating root manifest ~a..." filepath))
    (make-file-at-path (%zeta-root) "root.scm")
    (write-file filepath (root-with-manifests '()))
    (%root-manifest (string-append (%zeta-root) "/" "root.scm"))
    (info-with-msg "Done.")
    ))

(define* (zeta-list #:optional (output-port #t))
  (define pkg+locations '())

  (walk-zeta-tree (lambda (filename)
		    (when (string= (string-take-right filename 4) ".scm")
		      (for-each (lambda (pkg)
				  (unless (member pkg pkg+locations)
				    (set! pkg+locations
					  (append (list (list pkg filename)) pkg+locations))))
				(read-pkgs filename)))))
  (when (nil? pkg+locations) (error-with-msg "No manifests recognized."))
  (define padding 5)
  (define max-len (apply max (map (lambda (lst)
				    (string-length (car lst))) pkg+locations)))
  (define format-str (string-append "~" (number->string (+ max-len padding)) "a"))
  (for-each (lambda (pkg+location)
	      (format output-port (string-append format-str " ~a\n") (car pkg+location)
		      (cadr pkg+location))) pkg+locations))

(define (zeta-rescan)
  (info-with-msg (format #f "Rescanning root manifest ~a" (%root-manifest)))
  (define scanned-manifests '())
  (walk-zeta-tree (lambda (filename)
		    (when (string= (string-take-right filename 4) ".scm")
		      (info-with-msg (format #f "Found manifest ~a" filename))
		      (set! scanned-manifests (append scanned-manifests (list filename))))))
  (define fixed-root (root-with-manifests scanned-manifests))
  (write-file (%root-manifest) fixed-root))

(define (zeta-purge)
  (warning-with-msg (format #f "Deleting all manifests not specified in ~a" (%root-manifest)))
  (when (yn-prompt "Are you sure you want to proceed?")
    (walk-zeta-tree (lambda (filename)
		      (unless (member filename (read-manifests (%root-manifest)))
			(info-with-msg (format #f "Deleting ~a ...." filename))
			(delete-file filename))))))
