#!/usr/bin/env csi -s

;; Release script for multi-egg mono-repo
;; Usage: ./release.scm "schematra:1.2.3,oauthtoothy:2.1.0"
;; Usage: ./release.scm "schematra:1.2.3"

(import scheme
        chicken.base
        chicken.io
        chicken.process
	chicken.process-context
        chicken.string
        chicken.file
        chicken.file.posix
        srfi-1
        srfi-13)

(define (usage)
  (print "Usage: " (program-name) " 'egg1:version1,egg2:version2,...'")
  (print "Example: " (program-name) " 'schematra:1.2.3,oauthtoothy:2.1.0'")
  (exit 1))

(define (git-command . args)
  (let-values (((result exit-status) (apply run-command "git" args)))
    (if (= exit-status 0)
        result
        (begin
          (print "Error: git command failed: git " (string-join args " "))
          (print "Output: " result)
          (exit 1)))))

(define (run-command command . args)
  (let-values (((in out pid) (process command args)))
    (let ((result (read-string #f in)))
      (let-values (((waited-pid normal-exit? exit-status) (process-wait pid)))
        (close-input-port in)
        (close-output-port out)
        (values (if (eof-object? result) "" result)
                (if normal-exit? exit-status 1))))))

(define (check-git-repo)
  (unless (directory-exists? ".git")
    (print "Error: Not in a git repository")
    (exit 1)))

(define (check-clean-working-dir)
  (let ((status (git-command "status" "--porcelain")))
    (unless (string-null? (string-trim status))
      (print "Error: Working directory is not clean. Please commit your changes first.")
      (exit 1))))

(define (parse-egg-versions egg-list-string)
  (map (lambda (egg-version)
         (let ((parts (string-split egg-version ":")))
           (if (= (length parts) 2)
               (cons (first parts) (second parts))
               (begin
                 (print "Error: Invalid format in " egg-version)
                 (exit 1)))))
       (string-split egg-list-string ",")))

(define (egg-directory-exists? egg)
  (directory-exists? (string-append "eggs/" egg)))

(define (release-info-file-exists? egg)
  (file-exists? (string-append "eggs/" egg "/" egg ".release-info")))

(define (read-release-info-file egg)
  (let ((file-path (string-append "eggs/" egg "/" egg ".release-info")))
    (with-input-from-file file-path
      (lambda ()
        (read-string #f)))))

(define (write-release-info-file egg content)
  (let ((file-path (string-append "eggs/" egg "/" egg ".release-info")))
    (with-output-to-file file-path
      (lambda ()
        (display content)))))

(define (insert-release-line content version)
  ;; Find the position after the first two lines (repo and uri)
  ;; and insert the new release line there (newest first)
  (let ((lines (string-split content "\n")))
    (if (>= (length lines) 2)
        (let ((header-lines (take lines 2))
              (release-lines (drop lines 2))
              (new-release-line (string-append "(release \"" version "\")")))
          ;; Filter out empty lines and rebuild
          (let ((clean-release-lines (filter (lambda (line) 
                                              (not (string-null? (string-trim line))))
                                            release-lines)))
            (string-join (append header-lines 
                                (list new-release-line)
                                clean-release-lines) 
                       "\n")))
        (begin
          (print "Error: Invalid release-info file format")
          (exit 1)))))

(define (tag-exists? tag)
  (let-values (((result exit-status) 
                (run-command "sh" "-c" 
                            (string-append "git rev-parse " tag " 2>/dev/null"))))
    (= exit-status 0)))

;; (create-tarball "schematra" "0.2.3")
(define (create-tarball egg version)
  (let* ((tarball-name (string-append egg "-" version ".tar.gz"))
         (prefix (string-append egg "-" version "/"))
         (source (string-append "HEAD:eggs/" egg)))
    (print "DEBUG: Creating tarball with git archive")
    (print "  prefix: " prefix)
    (print "  source: " source)
    (let-values (((result exit-status) 
                  (run-command "git" "archive" "--format=tar.gz" 
                               (string-append "--prefix=" prefix)
                               source)))
      (if (= exit-status 0)
          (begin
            (with-output-to-file tarball-name
              (lambda () (display result)))
            tarball-name)
          (begin
            (print "Error creating tarball (exit status: " exit-status "): " result)
            (exit 1))))))

(define (gh-available?)
  (let-values (((result exit-status) (run-command "which" "gh")))
    (= exit-status 0)))

(define (create-github-release egg version tarball)
  (let ((tag (string-append egg "-" version))
        (title (string-append egg " v" version))
        (notes (string-append "Release " egg " version " version)))
    (let-values (((result exit-status) 
                  (run-command "gh" "release" "create" tag tarball 
                              "--title" title "--notes" notes)))
      (= exit-status 0))))

(define (release-egg egg version)
  (print "Processing " egg " v" version "...")
  
  ;; Check if egg directory exists
  (unless (egg-directory-exists? egg)
    (print "Error: eggs/" egg " directory does not exist")
    (exit 1))
  
  ;; Check if release-info file exists
  (unless (release-info-file-exists? egg)
    (print "Error: eggs/" egg "/" egg ".release-info does not exist")
    (exit 1))
  
  ;; Update release-info file
  (print "  Adding release to release-info file...")
  (let* ((current-content (read-release-info-file egg))
         (updated-content (insert-release-line current-content version)))
    (write-release-info-file egg updated-content))
  
  ;; Commit the release-info change
  (print "  Committing release-info changes...")
  (run-command "git" "add" (string-append "eggs/" egg "/" egg ".release-info"))
  (run-command "git" "commit" "-m" (string-append "Release " egg " v" version))
  
  ;; Create tarball
  (print "  Creating tarball...")
  (let ((tarball (create-tarball egg version)))

    ;; Check if tag already exists
    (let ((tag (string-append egg "-" version)))
      (if (tag-exists? tag)
          (print "  Warning: Tag " tag " already exists, skipping...")
          (begin
            ;; Tag the commit
            (print "  Creating tag " tag "...")
            (run-command "git" "tag" tag)

            ;; Push tag to remote
            (print "  Pushing tag to remote...")
            (run-command "git" "push" "origin" tag)
            
            ;; Create GitHub release if gh is available
            (if (gh-available?)
                (begin
                  (print "  Creating GitHub release...")
                  (if (create-github-release egg version tarball)
                      (print "  ✓ GitHub release created successfully")
                      (begin
                        (print "  ✗ Failed to create GitHub release")
                        (print "  You can manually create a release and upload " tarball))))
                (begin
                  (print "  GitHub CLI not available, skipping release creation")
                  (print "  You can manually create a release and upload " tarball)))
            
            (print "  ✓ " egg " v" version " released")
            (print))))))

(define (main)
  (let ((args (command-line-arguments)))
    (if (null? args)
        (usage)
        (let ((egg-list (car args)))
          (print "Releasing eggs...")
          (print)
          
          ;; Pre-flight checks
          (check-git-repo)
          (check-clean-working-dir)
          
          ;; Parse and process each egg
          (let ((eggs (parse-egg-versions egg-list)))
            (for-each (lambda (egg-version)
                        (release-egg (car egg-version) (cdr egg-version)))
                      eggs)
            
            (print "All releases completed!"))))))

(main)
