#lang racket/base
;; Copyright Neil Van Dyke. See file "info.rkt".

(require racket/system
         mcfly)

(module+ test
  (require overeasy))

(doc (section "Introduction")

     (para "This package adds automatic "
           (hyperlink "http://en.wikipedia.org/wiki/Sudo"
                      (tt "sudo"))
           " functionality to the Racket standard procedures for creating
processes on Unix-like systems.  This can be used for running commands as
different user accounts, such as for running privileged programs for system
administration.")

     (para "The procedures of this package wrap the Racket standard procedures,
to automatically use programs like "
           (code "sudo")
           " and "
           (code "gksudo")
           " when necessary.  There are features for specifying which programs
to try, in which order.")

     (para "For example, say you're writing a system administration program in
Racket, which needs to call the "
           (tt "do-stuff")
           " program, which can only be run as user "
           (tt "root")
           ".  Instead of using the Racket "
           (racket system*)
           " procedure, you use "
           (racket system*/sudo)
           ", like so:")

     (racketblock
      (system*/sudo "/sbin/do-stuff"
                    "remove"
                    ".*video.*"
                    "costing"
                    ">"
                    "$0"))

     (para "When Bob runs this Racket program on a system into which he has
only an SSH command-line interface, logged in as user "
           (tt "bob")
           ", that "
           (racket system*/sudo)
           " call might effectively behave like:")

     (racketblock
      (system* "/usr/bin/sudo"
               "-u"
               "root"
               "/sbin/do-stuff"
               "remove"
               ".*video.*"
               "costing"
               ">"
               "$0"))

     (para "As an aside, "
           (racket system*/sudo)
           " actually found the "
           (tt "sudo")
           " program in "
           (filepath "/usr/bin/sudo")
           "; it would have looked a couple other likely places, if not found
there.  For security reasons, it does not search the user's own executable
search path, however.")

     (para "Later, when Bob runs the Racket program when logged in as user "
           (tt "root")
           ", that same "
           (racket system*/sudo)
           " call will not require a "
           (tt "sudo")
           ", and instead be like:")

     (racketblock
      (system* "/sbin/do-stuff"
               "remove"
               ".*video.*"
               "costing"
               ">"
               "$0"))

     (para "When Bob later runs the same Racket program on his workstation,
as user "
           (tt "bob")
           ", where he's using a graphical desktop, that same "
           (racket system*/sudo)
           " call might use the GUI "
           (tt "gksudo")
           " program:")

     (racketblock
      (system* "/usr/local/bin/gksudo"
               "-u"
               "root"
               "/sbin/do-stuff remove \".*video.*\" costing \">\" \"\\$0\""))

     (para "This is a good time to mention a restriction that this package
imposes: notice in the example above that "
           (tt "gksudo")
           " takes a single string as the command line to run, rather than the
safer individual arguments that are not re-parsed.  This package has to use
quoting and escaping to sanitize the command line for re-parsing.
Additionally, to avoid potential problems in, say, shell scripts that might be
called by "
           (tt "gksudo")
           ", this package limits commands and arguments used with "
           (tt "gksudo")
           " to contain only printable ASCII characters in the range 32 to 126.
This prevents things like newlines that might be handled improperly in shell
scripts, and multi-byte character encodings that might defeat quoting and
escaping.")

     (para "This package also supports users other than "
           (tt "root")
           ", with the "
           (racket #:user)
           " optional keyword argument to each procedure.  For example, to start a process running the PostgreSQL "
           (tt "psql")
           " program as user "
           (tt "postgres")
           ", you might do something like:")

     (racketblock
      (subprocess/sudo #:user "postgres"
                       #f ; stdout-file-output-port
                       #f ; stdin-file-input-port
                       #f ; stderr-file-output-port
                       "/usr/bin/psql"
                       "-h"
                       my-database-host
                       "-d"
                       my-database-name))

     (para "Note: Even a properly administered "
           (tt "sudo")
           " configuration can make it easier to compromise the reliability of
security of a system, accidentally or intentionally, in some cases.  At the
same time, judicious configuration and use of "
           (tt "sudo")
           " can conceivably reduce accidents and vulnerabilites.  (Ask any
network administrator who, in lieu of "
           (tt "sudo")
           " has had a "
           (tt "root")
           " login window open, and accidentally hit the middle mouse button,
pasting a pile shell script text, which was then executed happily and
destructively as "
           (tt "root")
           ".)  Similarly, this package can be beneficial for reliability and
security, but must be used judiciously."))

(doc (section "Interface")

     (para "The interface to this package consists mainly of a set of
procedures that correspond to Racket standard process creation procedures, plus
a parameter that controls how a "
           (tt "sudo")
           " program is found, by default."))

(doc (defproc (sudo? (x any/c))
         boolean?
       (para "Predicate for permissible values of the "
             (racket current-sudo)
             " parameter, and of the "
             (racket #:sudo)
             " keyword argument of the various procedures.  This value
specifies how to select a "
             (tt "sudo")
             " program whenever one is needed.")
       (para "The values can be expressed by the contract:")
       (racketblock
        (or/c 'sudo
              'gksudo
              absolute-path?
              (listof (or/c 'sudo
                            'gksudo
                            absolute-path?))))
       (para "More specifically, the value may be either one of the following,
or a list of one or more of the following:")
       (itemlist
        (item (racket 'sudo)
              " -- The normal "
              (tt "sudo")
              " program, in one of a few conventional places, if the executable
file exists.")
        (item (racket 'gksudo)
              " -- The "
              (tt "gksudo")
              " program, in one of a few conventional places, if the executable
file exists, and only if X is in use.  Currently, X in use is determined by
whether or not the "
              (tt "DISPLAY")
              " environment variable is set.")
        (item "An absolute path, which is used if the executable file exists
there.  It is then called with the syntax of normal "
              (tt "sudo")
              ".  Note that, since "
              (tt "gksudo")
              " uses a different command line syntax than "
              (tt "sudo")
              ", if you want to use "
              (tt "gksudo")
              ", generally you must use the "
              (racket 'gksudo)
              " value instead of an absolute path."))
       (para "When the value a list of the alternatives above, then they are
tried in order, until one succeeds.")))
(provide sudo?)
(define sudo?
  (let ((immediate-sudo? (lambda (x)
                           (or (eq? 'sudo      x)
                               (eq? 'gksudo    x)
                               (absolute-path? x)))))
    (lambda (x)
      (or (immediate-sudo?        x)
          (andmap immediate-sudo? x)))))

(doc (defparam current-sudo val sudo?
       (para "Parameter for how to select a "
             (tt "sudo")
             " program, if the "
             (racket #:sudo)
             " argument is not given.  The default value is "
             (racket '(gksudo sudo))
             ", which means that, by default, "
             (tt "gksudo")
             " will be used if it can be found and X is in use, and otherwise "
             (tt "sudo")
             " will be used.")))
(provide current-sudo)
(define current-sudo (make-parameter '(gksudo sudo)))

(define (%sudo:find-existing-file-or-false path-strings)
  (let loop ((path-strings path-strings))
    (if (null? path-strings)
        #f
        (let ((path-string (car path-strings)))
          (if (file-exists? path-string)
              path-string
              (loop (cdr path-strings)))))))

(define %sudo:assemble-command-line-bytes
  ;; TODO: Possibly make this do quoting and escaping by just doing
  ;; backslashes, no doublequotes.
  (let* ((space-ascii       32)
         (doublequote-ascii 34)
         (dollar-ascii      36)
         (backslash-ascii   92)
         (backquote-ascii   96)
         (escapable-asciis  (list doublequote-ascii
                                  dollar-ascii
                                  backslash-ascii
                                  backquote-ascii))
         (space-bstr        (bytes space-ascii))
         (string-to-bytes   (lambda (error-name str)
                              (with-handlers ((exn:fail:contract?
                                               (lambda (e)
                                                 (raise-type-error error-name
                                                                   "sudo-safe word"
                                                                   str))))
                                (string->bytes/latin-1 str)))))
    (lambda (error-name words)
      (if (null? words)
          #""
          (apply bytes-append
                 (let loop-words ((words words))
                   (let* ((word (car words))
                          (word (cond ((bytes? word) word)
                                      ((string? word) (string-to-bytes error-name
                                                                       word))
                                      ((path? word)   (string-to-bytes error-name
                                                                       (path->string word)))
                                      (else (raise-type-error error-name
                                                              "(or/c bytes? string? path?)"
                                                              word)))))
                     (cons (cond ((equal? #"" word) #"\"\"")
                                 ((regexp-match? #rx#"[^-_./=:a-zA-Z0-9]" word)
                                  (apply bytes
                                         (cons doublequote-ascii
                                               (let ((word-len (bytes-length word)))
                                                 (let loop-chars ((i 0))
                                                   (if (= i word-len)
                                                       (cons doublequote-ascii '())
                                                       (let ((c (bytes-ref word i)))
                                                         (cond ((member c escapable-asciis)
                                                                (cons backslash-ascii
                                                                      (cons c
                                                                            (loop-chars (add1 i)))))
                                                               ((<= 32 c 126)
                                                                (cons c (loop-chars (add1 i))))
                                                               (else (raise-type-error error-name
                                                                                       "sudo-safe word"
                                                                                       word))))))))))
                                 (else word))
                           (let ((words (cdr words)))
                             (if (null? words)
                                 '()
                                 (cons space-bstr
                                       (loop-words words))))))))))))

(module+ test

  (test (%sudo:assemble-command-line-bytes 'my '())
        #"")

  (test (%sudo:assemble-command-line-bytes 'my '("aaa" "b b" "ccc"))
        #"aaa \"b b\" ccc")

  (test (%sudo:assemble-command-line-bytes 'my '("aaa" "" "bbb"))
        #"aaa \"\" bbb")

  (test (%sudo:assemble-command-line-bytes 'my '("/bin/foo" "--bar=3" "--baz=x x" "$20" "&" ";" "xxx"))
        #"/bin/foo --bar=3 \"--baz=x x\" \"\\$20\" \"&\" \";\" xxx")

  (test (%sudo:assemble-command-line-bytes 'my '("a$a$a" "b`b`b" "c\"c\"c" "d\\d\\d"))
        #"\"a\\$a\\$a\" \"b\\`b\\`b\" \"c\\\"c\\\"c\" \"d\\\\d\\\\d\"")

  (test #:code (%sudo:assemble-command-line-bytes 'my '("a\0b"))
        #:exn  #rx"^my: expected argument of type <sudo-safe word>")

  (test #:code (%sudo:assemble-command-line-bytes 'my '("a\rb"))
        #:exn  #rx"^my: expected argument of type <sudo-safe word>")

  (test #:code (%sudo:assemble-command-line-bytes 'my '("a\nb"))
        #:exn  #rx"^my: expected argument of type <sudo-safe word>")

  )

(define (%sudo:sudo-command+args #:error-name error-name
                                 #:user       user
                                 #:sudo       sudo
                                 #:command    command
                                 #:args       args)
  (log-debug (format "%sudo:sudo-command+args: command ~S args ~S"
                     command
                     args))
  (or (absolute-path? command)
      (raise-type-error error-name
                        "absolute-path?"
                        command))
  (let* ((actual-user (getenv "USER"))
         (need-sudo?  (cond ((or (not actual-user)
                                 (equal? "" actual-user))
                             (log-warning (format "~S: USER environment variable is ~S"
                                                  error-name
                                                  actual-user)))
                            ((equal? actual-user user) #f)
                            (else #t))))
    (if need-sudo?
        (let loop ((kinds (if (pair? sudo)
                              sudo
                              (list sudo))))
          (if (null? kinds)
              (error error-name
                     "could not find suitable sudo program in #:sudo ~S"
                     sudo)
              (let loop-kind ((kind       (car kinds))
                              (executable #f))
                (case kind
                  ((gksudo)
                   (cond ((and (getenv "DISPLAY")
                               (or executable
                                   (%sudo:find-existing-file-or-false
                                    '("/usr/bin/gksudo"
                                      "/usr/sbin/gksudo"
                                      "/usr/local/bin/gksudo"))))
                          => (lambda (executable)
                               (values executable
                                       (list "-u"
                                             user
                                             (bytes->string/latin-1
                                              (%sudo:assemble-command-line-bytes error-name
                                                                                 (cons command
                                                                                       args)))))))
                         (else (loop (cdr kinds)))))
                  ((sudo)
                   (cond ((or executable
                              (%sudo:find-existing-file-or-false
                               '("/usr/bin/sudo"
                                 "/var/setuid-wrappers/sudo"
                                 "/usr/sbin/sudo"
                                 "/usr/local/bin/sudo")))
                          => (lambda (executable)
                               (values executable
                                       `("-u" ,user ,command ,@args))))
                         (else (loop (cdr kinds)))))
                  (else (cond ((and (string? kind)
                                    (absolute-path? (string->path kind)))
                               (loop-kind 'sudo kind))
                              ((absolute-path? kind)
                               (loop-kind 'sudo (path->string kind)))
                              (else (error error-name
                                           "invalid kind ~S in #:sudo ~S"
                                           kind
                                           sudo))))))))
        (values command args))))

(define-syntax %sudo:define/provide-wrapper
  (syntax-rules ()
    ((_ PROC/SUDO PROC (EXTRA-ARGn ...))
     (begin
       (provide PROC/SUDO)
       (define (PROC/SUDO #:user (user "root")
                          #:sudo (sudo (current-sudo))
                          EXTRA-ARGn ...
                          command
                          . args)
         (log-debug (format "~S: received command ~S args ~S"
                            (quote PROC/SUDO)
                            command
                            args))
         (let-values (((command args) (%sudo:sudo-command+args
                                       #:error-name (quote PROC/SUDO)
                                       #:user       user
                                       #:sudo       sudo
                                       #:command    command
                                       #:args       args)))
           (log-debug (format "~S: running command ~S args ~S"
                              (quote PROC/SUDO)
                              command
                              args))
           (apply PROC EXTRA-ARGn ... command args)))))))

(doc (defproc (subprocess/sudo (#:user user string? "root")
                               (#:sudo sudo sudo?   (current-sudo))
                               (stdout    any/c)
                               (stdin     any/c)
                               (stderr    any/c)
                               (command   absolute-path?)
                               (arg       any/c) ...)
         any
       (para "Wrapper for "
             (racket suprocess)
             " with "
             (tt "sudo")
             " support.")))
(%sudo:define/provide-wrapper subprocess/sudo
                              subprocess
                              (stdout stdin stderr))

(doc (defproc (system*/sudo (#:user user string? "root")
                            (#:sudo sudo sudo?   (current-sudo))
                            (command   absolute-path?)
                            (arg       any/c) ...)
         any
       (para "Wrapper for "
             (racket system*)
             " with "
             (tt "sudo")
             " support.")))
(%sudo:define/provide-wrapper system*/sudo
                              system*
                              ())

(doc (defproc (system*/exit-code/sudo (#:user user string? "root")
                                      (#:sudo sudo sudo?   (current-sudo))
                                      (command   absolute-path?)
                                      (arg       any/c) ...)
         any
       (para "Wrapper for "
             (racket system*/exit-code)
             " with "
             (tt "sudo")
             " support.")))
(%sudo:define/provide-wrapper system*/exit-code/sudo
                              system*/exit-code
                              ())

(doc (defproc (process*/sudo (#:user user string? "root")
                             (#:sudo sudo sudo?   (current-sudo))
                             (command   absolute-path?)
                             (arg       any/c) ...)
         any
       (para "Wrapper for "
             (racket process*)
             " with "
             (tt "sudo")
             " support.")))
(%sudo:define/provide-wrapper process*/sudo
                              process*
                              ())

(doc (defproc (process*/ports/sudo (#:user user string? "root")
                                   (#:sudo sudo sudo?   (current-sudo))
                                   (out       any/c)
                                   (in        any/c)
                                   (error-out any/c)
                                   (command   absolute-path?)
                                   (arg       any/c) ...)
         any
       (para "Wrapper for "
             (racket process*/ports)
             " with "
             (tt "sudo")
             " support.")))
(%sudo:define/provide-wrapper process*/ports/sudo
                              process*/ports
                              (out in error-out))

;; (system*/sudo "/usr/bin/whoami")
;; (system*/sudo #:user "postgres" "/usr/bin/whoami")
;; (system*/sudo #:user (getenv "USER") "/usr/bin/whoami")
;;
;; (system*/sudo "/usr/bin/whoami" "--help")
;; (system*/sudo #:user "postgres" "/usr/bin/whoami" "--help")
;; (system*/sudo #:user (getenv "USER") "/usr/bin/whoami" "--help")
;;
;; (system*/sudo #:user (getenv "USER") #:sudo "/usr/bin/sudo" "/usr/bin/whoami")
;;
;; (system*/sudo #:user (getenv "USER") #:sudo '("/nonexistent" "/usr/bin/sudo") "/usr/bin/whoami")
;;
;; (system*/sudo #:sudo '("/nonexistent" "/usr/bin/sudo") "/usr/bin/whoami")

;; TODO: Support KdeSudo.  Documentation is unclear whether it requires
;; GkSudo-like entire command line in one string.

(doc (section "Known Issues")

     (itemlist

      (item "Add standard KdeSudo support.  This is probably just a matter of
finding documentation or setting up a system on which to test, since we have
not yet seen clear documentation of syntax.")

      (item "Permit "
            (racket sudo?)
            " to accept a value of a pair of an absolute path and a symbol
denoting syntax.")

      (item "Determine any other conventional places in filesystem for the
executables to be found.")

      (item "Possibly change the conservative quoting and escaping of "
            (racket 'gksudo)
            ", to permit more characters, without being unsafe.")))

;; TODO: If we use "--" argument to gksudo, we can have multiple arguments for
;; child command and its arguments, except it seems that gksudo joins them
;; together and then reparses, so, e.g., filenames with spaces will break,
;; among other things.  This seems like a bad idea of gksudo.

(doc history

     (#:planet 2:1 #:date "2016-03-07"
               (itemlist
                (item "Tweaked title, deps, filenames.")))
     
     (#:planet 2:0 #:date "2016-02-26"
               (itemlist
                (item "Moving from PLaneT to new package system.")))
                      
     (#:planet 1:1 #:date "2012-10-07"
               (itemlist
                (item "Fixed "
                      (filepath "main.rkt")
                      ".")
                (item "Documentation tweaks.")))

     (#:planet 1:0 #:date "2012-10-06"
               (itemlist

                (item "Initial release."))))
