(require 'scheme)
(require 'comint)

;;; INFERIOR SCHEME MODE STUFF
;;;============================================================================

(defvar inferior-scheme-mode-hook nil
  "*Hook for customising inferior-scheme mode.")
(defvar inferior-scheme-mode-map nil)

(cond ((not inferior-scheme-mode-map)
       (setq inferior-scheme-mode-map
	     (copy-keymap comint-mode-map))
       (define-key inferior-scheme-mode-map "\M-\C-x" ;gnu convention
	           'scheme-send-definition)
       (define-key inferior-scheme-mode-map "\C-x\C-e" 'scheme-send-last-sexp)
       (define-key inferior-scheme-mode-map "\C-c\C-l" 'scheme-load-file)
       (define-key inferior-scheme-mode-map "\C-c\C-k" 'scheme-compile-file)
       (scheme-mode-commands inferior-scheme-mode-map))) 

;; Install the process communication commands in the scheme-mode keymap.
(define-key scheme-mode-map "\M-\C-x" 'scheme-send-definition);gnu convention
(define-key scheme-mode-map "\C-x\C-e" 'scheme-send-last-sexp);gnu convention
(define-key scheme-mode-map "\C-c\C-e" 'scheme-send-definition)
(define-key scheme-mode-map "\C-c\M-e" 'scheme-send-definition-and-go)
(define-key scheme-mode-map "\C-c\C-r" 'scheme-send-region)
(define-key scheme-mode-map "\C-c\M-r" 'scheme-send-region-and-go)
(define-key scheme-mode-map "\C-c\M-c" 'scheme-compile-definition)
(define-key scheme-mode-map "\C-c\C-c" 'scheme-compile-definition-and-go)
(define-key scheme-mode-map "\C-c\C-z" 'switch-to-scheme)
(define-key scheme-mode-map "\C-c\C-l" 'scheme-load-file)
(define-key scheme-mode-map "\C-c\C-k" 'scheme-compile-file) ;k for "kompile"

(defvar scheme-buffer)

(defun inferior-scheme-mode ()
  "Major mode for interacting with an inferior Scheme process.

The following commands are available:
\\{inferior-scheme-mode-map}

A Scheme process can be fired up with M-x run-scheme.

Customisation: Entry to this mode runs the hooks on comint-mode-hook and
inferior-scheme-mode-hook (in that order).

You can send text to the inferior Scheme process from other buffers containing
Scheme source.  
    switch-to-scheme switches the current buffer to the Scheme process buffer.
    scheme-send-definition sends the current definition to the Scheme process.
    scheme-compile-definition compiles the current definition.
    scheme-send-region sends the current region to the Scheme process.
    scheme-compile-region compiles the current region.

    scheme-send-definition-and-go, scheme-compile-definition-and-go,
        scheme-send-region-and-go, and scheme-compile-region-and-go
        switch to the Scheme process buffer after sending their text.
For information on running multiple processes in multiple buffers, see
documentation for variable scheme-buffer.

Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Scheme; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  ;; Customise in inferior-scheme-mode-hook
  (setq comint-prompt-regexp "^[^>\n]*>+ *") ; OK for cscheme, oaklisp, T,...
  (scheme-mode-variables)
  (setq major-mode 'inferior-scheme-mode)
  (setq mode-name "Inferior Scheme")
  (setq mode-line-process '(":%s"))
  (use-local-map inferior-scheme-mode-map)
  (setq comint-input-filter (function scheme-input-filter))
  (setq comint-get-old-input (function scheme-get-old-input))
  (run-hooks 'inferior-scheme-mode-hook))

(defvar inferior-scheme-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.")

(defun scheme-input-filter (str)
  "Don't save anything matching inferior-scheme-filter-regexp"
  (not (string-match inferior-scheme-filter-regexp str)))

(defun scheme-get-old-input ()
  "Snarf the sexp ending at point"
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun scheme-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (scheme-args-to-list (substring string (+ 1 where)
						 (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (scheme-args-to-list (substring string pos
						 (length string)))))))))

(defvar scheme-program-name "scheme"
  "*Program invoked by the run-scheme command")

;;;###autoload
(defun run-scheme (cmd)
  "Run an inferior Scheme process, input and output via buffer *scheme*.
If there is a process already running in *scheme*, just switch to that buffer.
With argument, allows you to edit the command line (default is value
of scheme-program-name).  Runs the hooks from inferior-scheme-mode-hook
\(after the comint-mode-hook is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
			 (read-string "Run Scheme: " scheme-program-name)
			 scheme-program-name)))
  (if (not (comint-check-proc "*scheme*"))
      (let ((cmdlist (scheme-args-to-list cmd)))
	(set-buffer (apply 'make-comint "scheme" (car cmdlist)
			   nil (cdr cmdlist)))
	(inferior-scheme-mode)))
  (setq scheme-program-name cmd)
  (setq scheme-buffer "*scheme*")
  (switch-to-buffer "*scheme*"))


(defun scheme-send-region (start end)
  "Send the current region to the inferior Scheme process."
  (interactive "r")
  (comint-send-region (scheme-proc) start end)
  (comint-send-string (scheme-proc) "\n"))

(defun scheme-send-definition ()
  "Send the current definition to the inferior Scheme process."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (scheme-send-region (point) end))))

(defun scheme-send-last-sexp ()
  "Send the previous sexp to the inferior Scheme process."
  (interactive)
  (scheme-send-region (save-excursion (backward-sexp) (point)) (point)))

(defvar scheme-compile-exp-command "(compile '%s)"
  "*Template for issuing commands to compile arbitrary Scheme expressions.")

(defun scheme-compile-region (start end)
  "Compile the current region in the inferior Scheme process.
\(A BEGIN is wrapped around the region: (BEGIN <region>))"
  (interactive "r")
  (comint-send-string (scheme-proc) (format scheme-compile-exp-command
					    (format "(begin %s)"
						    (buffer-substring start end))))
  (comint-send-string (scheme-proc) "\n"))

(defun scheme-compile-definition ()
  "Compile the current definition in the inferior Scheme process."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (scheme-compile-region (point) end))))

(defun switch-to-scheme (eob-p)
  "Switch to the scheme process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer scheme-buffer)
      (pop-to-buffer scheme-buffer)
      (error "No current process buffer. See variable scheme-buffer."))
  (cond (eob-p
	 (push-mark)
	 (goto-char (point-max)))))

(defun scheme-send-region-and-go (start end)
  "Send the current region to the inferior Scheme process.
Then switch to the process buffer."
  (interactive "r")
  (scheme-send-region start end)
  (switch-to-scheme t))

(defun scheme-send-definition-and-go ()
  "Send the current definition to the inferior Scheme. 
Then switch to the process buffer."
  (interactive)
  (scheme-send-definition)
  (switch-to-scheme t))

(defun scheme-compile-definition-and-go ()
  "Compile the current definition in the inferior Scheme. 
Then switch to the process buffer."
  (interactive)
  (scheme-compile-definition)
  (switch-to-scheme t))

(defun scheme-compile-region-and-go (start end)
  "Compile the current region in the inferior Scheme. 
Then switch to the process buffer."
  (interactive "r")
  (scheme-compile-region start end)
  (switch-to-scheme t))

(defvar scheme-source-modes '(scheme-mode)
  "*Used to determine if a buffer contains Scheme source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a scheme source file by scheme-load-file and scheme-compile-file.
Used by these commands to determine defaults.")

(defvar scheme-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last scheme-load-file or
scheme-compile-file command. Used for determining the default in the 
next one.")

(defun scheme-load-file (file-name)
  "Load a Scheme file into the inferior Scheme process."
  (interactive (comint-get-source "Load Scheme file: " scheme-prev-l/c-dir/file
				  scheme-source-modes t)) ; T because LOAD 
                                                          ; needs an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
				       (file-name-nondirectory file-name)))
  (comint-send-string (scheme-proc) (concat "(load \""
					    file-name
					    "\"\)\n")))

(defun scheme-compile-file (file-name)
  "Compile a Scheme file in the inferior Scheme process."
  (interactive (comint-get-source "Compile Scheme file: "
				  scheme-prev-l/c-dir/file
				  scheme-source-modes
				  nil)) ; NIL because COMPILE doesn't
                                        ; need an exact name.
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
				       (file-name-nondirectory file-name)))
  (comint-send-string (scheme-proc) (concat "(compile-file \""
					    file-name
					    "\"\)\n")))


(defvar scheme-buffer nil "*The current scheme process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
Cmuscheme.el supports, in a fairly simple fashion, running multiple Scheme
processes. To run multiple Scheme processes, you start the first up with
\\[run-scheme]. It will be in a buffer named *scheme*. Rename this buffer
with \\[rename-buffer]. You may now start up a new process with another
\\[run-scheme]. It will be in a new buffer, named *scheme*. You can
switch between the different process buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Scheme processes --
like scheme-send-definition or scheme-compile-region -- have to choose a
process to send to, when you have more than one Scheme process around. This
is determined by the global variable scheme-buffer. Suppose you
have three inferior Schemes running:
    Buffer	Process
    foo		scheme
    bar		scheme<2>
    *scheme*    scheme<3>
If you do a \\[scheme-send-definition-and-go] command on some Scheme source
code, what process do you send it to?

- If you're in a process buffer (foo, bar, or *scheme*), 
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer scheme-buffer.
This process selection is performed by function scheme-proc.

Whenever \\[run-scheme] fires up a new process, it resets scheme-buffer
to be the new process's buffer. If you only run one process, this will
do the right thing. If you run multiple processes, you can change
scheme-buffer to another process buffer with \\[set-variable].

More sophisticated approaches are, of course, possible. If you find yourself
needing to switch back and forth between multiple processes frequently,
you may wish to consider ilisp.el, a larger, more sophisticated package
for running inferior Lisp and Scheme processes. The approach taken here is
for a minimal, simple implementation. Feel free to extend it.")

(defun scheme-proc ()
  "Returns the current scheme process. See variable scheme-buffer."
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-scheme-mode)
				      (current-buffer)
				      scheme-buffer))))
    (or proc
	(error "No current process. See variable scheme-buffer"))))


;;; Do the user's customisation...

(defvar cmuscheme-load-hook nil
  "This hook is run when cmuscheme is loaded in.
This is a good place to put keybindings.")
	
(run-hooks 'cmuscheme-load-hook)


;;; CHANGE LOG
;;; ===========================================================================
;;; 8/88 Olin
;;; Created. 
;;;
;;; 2/15/89 Olin
;;; Removed -emacs flag from process invocation. It's only useful for
;;; cscheme, and makes cscheme assume it's running under xscheme.el,
;;; which messes things up royally. A bug.
;;;
;;; 5/22/90 Olin
;;; - Upgraded to use comint-send-string and comint-send-region.
;;; - run-scheme now offers to let you edit the command line if
;;;   you invoke it with a prefix-arg. M-x scheme is redundant, and
;;;   has been removed.
;;; - Explicit references to process "scheme" have been replaced with
;;;   (scheme-proc). This allows better handling of multiple process bufs.
;;; - Added scheme-send-last-sexp, bound to C-x C-e. A gnu convention.
;;; - Have not added process query facility a la cmulisp.el's lisp-show-arglist
;;;   and friends, but interested hackers might find a useful application
;;;   of this facility.
;;;
;;; 3/12/90 Olin
;;; - scheme-load-file and scheme-compile-file no longer switch-to-scheme.
;;;   Tale suggested this.

(provide 'cmuscheme)

;;; cmuscheme.el ends here
