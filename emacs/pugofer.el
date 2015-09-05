;;;Arunachala Siva Arunachala Ramana

;;; pugofer.el --- Pugofer process in a buffer.

;;; Hacked from scheme.el by rpm; search, replace, hack out errors

;;; Code:


(require 'comint)

(defun pugofer-mode-commands (map)
  (define-key map "\177" 'backward-delete-char-untabify)
  (define-key map "\e\C-q" 'pugofer-indent-sexp))

(defun pugofer-mode-variables ())  ;needs to be filled

(defvar pugofer-mode-map nil)
(if (not pugofer-mode-map)
    (progn
      (setq pugofer-mode-map (make-sparse-keymap))
      (pugofer-mode-commands pugofer-mode-map)))

;;; INFERIOR PUGOFER MODE STUFF
;;;============================================================================

(defcustom inferior-pugofer-mode-hook nil
  "*Hook for customising inferior-pugofer mode.")
(defvar inferior-pugofer-mode-map nil)

(cond ((not inferior-pugofer-mode-map)
       (setq inferior-pugofer-mode-map
	     (copy-keymap comint-mode-map))
       (define-key inferior-pugofer-mode-map "\M-\C-x" ;gnu convention
	           'pugofer-send-definition)
       (define-key inferior-pugofer-mode-map "\C-x\C-e" 'pugofer-send-last-sexp)
       (define-key inferior-pugofer-mode-map "\C-c\C-l" 'pugofer-load-file)
       (define-key inferior-pugofer-mode-map "\C-c\C-k" 'pugofer-compile-file)
;       (pugofer-mode-commands inferior-pugofer-mode-map)
)) 

;; Install the process communication commands in the pugofer-mode keymap.
(define-key pugofer-mode-map "\M-\C-x" 'pugofer-send-definition);gnu convention
(define-key pugofer-mode-map "\C-x\C-e" 'pugofer-send-last-sexp);gnu convention
(define-key pugofer-mode-map "\C-c\C-e" 'pugofer-send-definition)
(define-key pugofer-mode-map "\C-c\M-e" 'pugofer-send-definition-and-go)
(define-key pugofer-mode-map "\C-c\C-r" 'pugofer-send-region)
(define-key pugofer-mode-map "\C-c\M-r" 'pugofer-send-region-and-go)
(define-key pugofer-mode-map "\C-c\M-c" 'pugofer-compile-definition)
(define-key pugofer-mode-map "\C-c\C-c" 'pugofer-compile-definition-and-go)
(define-key pugofer-mode-map "\C-c\C-z" 'switch-to-pugofer)
(define-key pugofer-mode-map "\C-c\C-l" 'pugofer-load-file)
(define-key pugofer-mode-map "\C-c\C-k" 'pugofer-compile-file) ;k for "kompile"

(defvar pugofer-buffer)

(defun inferior-pugofer-mode ()
  "Major mode for interacting with an inferior Pugofer process.

The following commands are available:
\\{inferior-pugofer-mode-map}

A Pugofer process can be fired up with M-x run-pugofer.

Customisation: Entry to this mode runs the hooks on comint-mode-hook and
inferior-pugofer-mode-hook (in that order).

You can send text to the inferior Pugofer process from other buffers containing
Pugofer source.  
    switch-to-pugofer switches the current buffer to the Pugofer process buffer.
    pugofer-send-definition sends the current definition to the Pugofer process.
    pugofer-compile-definition compiles the current definition.
    pugofer-send-region sends the current region to the Pugofer process.
    pugofer-compile-region compiles the current region.

    pugofer-send-definition-and-go, pugofer-compile-definition-and-go,
        pugofer-send-region-and-go, and pugofer-compile-region-and-go
        switch to the Pugofer process buffer after sending their text.
For information on running multiple processes in multiple buffers, see
documentation for variable pugofer-buffer.

Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Pugofer; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  ;; Customise in inferior-pugofer-mode-hook
  (setq comint-prompt-regexp "^[^?\n]*?+ *") ; OK for cpugofer, oaklisp, T,...
  (pugofer-mode-variables)
  (setq major-mode 'inferior-pugofer-mode)
  (setq mode-name "Inferior Pugofer")
  (setq mode-line-process '(":%s"))
  (use-local-map inferior-pugofer-mode-map)
  (setq comint-input-filter (function pugofer-input-filter))
;  (setq comint-get-old-input (function pugofer-get-old-input))
  (run-hooks 'inferior-pugofer-mode-hook))

(defvar inferior-pugofer-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.")

(defun pugofer-input-filter (str)
  "Don't save anything matching inferior-pugofer-filter-regexp"
  (not (string-match inferior-pugofer-filter-regexp str)))

;(defun pugofer-get-old-input ()
;  "Snarf the sexp ending at point"
;  (save-excursion
;    (let ((end (point)))
;      (backward-sexp)
;      (buffer-substring (point) end))))

(defun pugofer-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (pugofer-args-to-list (substring string (+ 1 where)
						 (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (pugofer-args-to-list (substring string pos
						 (length string)))))))))

(defvar pugofer-program-name "pugofer"
  "*Program invoked by the run-pugofer command")

;;;###autoload
(defun run-pugofer (cmd)
  "Run an inferior Pugofer process, input and output via buffer *pugofer*.
If there is a process already running in *pugofer*, just switch to that buffer.
With argument, allows you to edit the command line (default is value
of pugofer-program-name).  Runs the hooks from inferior-pugofer-mode-hook
\(after the comint-mode-hook is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
			 (read-string "Run Pugofer: " pugofer-program-name)
			 pugofer-program-name)))
  (if (not (comint-check-proc "*pugofer*"))
      (let ((cmdlist (pugofer-args-to-list cmd)))
	(set-buffer (apply 'make-comint "pugofer" (car cmdlist)
			   nil (cdr cmdlist)))
	(inferior-pugofer-mode)))
  (setq pugofer-program-name cmd)
  (setq pugofer-buffer "*pugofer*")
  (switch-to-buffer "*pugofer*"))


(defun pugofer-send-region (start end)
  "Send the current region to the inferior Pugofer process."
  (interactive "r")
  (comint-send-region (pugofer-proc) start end)
  (comint-send-string (pugofer-proc) "\n"))

(defun pugofer-send-definition ()
  "Send the current definition to the inferior Pugofer process."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (pugofer-send-region (point) end))))

(defun pugofer-send-last-sexp ()
  "Send the previous sexp to the inferior Pugofer process."
  (interactive)
  (pugofer-send-region (save-excursion (backward-sexp) (point)) (point)))

(defvar pugofer-compile-exp-command "(compile '%s)"
  "*Template for issuing commands to compile arbitrary Pugofer expressions.")

(defun pugofer-compile-region (start end)
  "Compile the current region in the inferior Pugofer process.
\(A BEGIN is wrapped around the region: (BEGIN <region>))"
  (interactive "r")
  (comint-send-string (pugofer-proc) (format pugofer-compile-exp-command
					    (format "(begin %s)"
						    (buffer-substring start end))))
  (comint-send-string (pugofer-proc) "\n"))

(defun pugofer-compile-definition ()
  "Compile the current definition in the inferior Pugofer process."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (pugofer-compile-region (point) end))))

(defun switch-to-pugofer (eob-p)
  "Switch to the pugofer process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer pugofer-buffer)
      (pop-to-buffer pugofer-buffer)
      (error "No current process buffer. See variable pugofer-buffer."))
  (cond (eob-p
	 (push-mark)
	 (goto-char (point-max)))))

(defun pugofer-send-region-and-go (start end)
  "Send the current region to the inferior Pugofer process.
Then switch to the process buffer."
  (interactive "r")
  (pugofer-send-region start end)
  (switch-to-pugofer t))

(defun pugofer-send-definition-and-go ()
  "Send the current definition to the inferior Pugofer. 
Then switch to the process buffer."
  (interactive)
  (pugofer-send-definition)
  (switch-to-pugofer t))

(defun pugofer-compile-definition-and-go ()
  "Compile the current definition in the inferior Pugofer. 
Then switch to the process buffer."
  (interactive)
  (pugofer-compile-definition)
  (switch-to-pugofer t))

(defun pugofer-compile-region-and-go (start end)
  "Compile the current region in the inferior Pugofer. 
Then switch to the process buffer."
  (interactive "r")
  (pugofer-compile-region start end)
  (switch-to-pugofer t))

(defvar pugofer-source-modes '(pugofer-mode)
  "*Used to determine if a buffer contains Pugofer source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a pugofer source file by pugofer-load-file and pugofer-compile-file.
Used by these commands to determine defaults.")

(defvar pugofer-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last pugofer-load-file or
pugofer-compile-file command. Used for determining the default in the 
next one.")

(defun pugofer-load-file (file-name)
  "Load a Pugofer file into the inferior Pugofer process."
  (interactive (comint-get-source "Load Pugofer file: "
				  pugofer-prev-l/c-dir/file
				  pugofer-source-modes t)) ; T because LOAD 
                                                          ; needs an exact name
  (pu-comint-check-source file-name) ; Check to see if buffer needs saved.
  (let* ((d (file-name-directory    file-name))
	 (f (file-name-nondirectory file-name))
	 (ftobesent (if (string= ""
				 (file-relative-name d default-directory))
			f  file-name)))
    (setq pugofer-prev-l/c-dir/file (cons d f))
    (comint-send-string (pugofer-proc)
			(concat ":l " ftobesent "\n"))
    (switch-to-pugofer t)) ;; rpm
  )

(defun pugofer-compile-file (file-name)
  "Compile a Pugofer file in the inferior Pugofer process."
  (interactive (comint-get-source "Compile Pugofer file: "
				  pugofer-prev-l/c-dir/file
				  pugofer-source-modes
				  nil)) ; NIL because COMPILE doesn't
                                        ; need an exact name.
  (pu-comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq pugofer-prev-l/c-dir/file (cons (file-name-directory    file-name)
				       (file-name-nondirectory file-name)))
  (comint-send-string (pugofer-proc) (concat "(compile-file \""
					    file-name
					    "\"\)\n")))


(defvar pugofer-buffer nil "*The current pugofer process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
pugofer.el supports, in a fairly simple fashion, running multiple Pugofer
processes. To run multiple Pugofer processes, you start the first up with
\\[run-pugofer]. It will be in a buffer named *pugofer*. Rename this buffer
with \\[rename-buffer]. You may now start up a new process with another
\\[run-pugofer]. It will be in a new buffer, named *pugofer*. You can
switch between the different process buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Pugofer processes --
like pugofer-send-definition or pugofer-compile-region -- have to choose a
process to send to, when you have more than one Pugofer process around. This
is determined by the global variable pugofer-buffer. Suppose you
have three inferior Pugofers running:
    Buffer	Process
    foo		pugofer
    bar		pugofer<2>
    *pugofer*    pugofer<3>
If you do a \\[pugofer-send-definition-and-go] command on some Pugofer source
code, what process do you send it to?

- If you're in a process buffer (foo, bar, or *pugofer*), 
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer pugofer-buffer.
This process selection is performed by function pugofer-proc.

Whenever \\[run-pugofer] fires up a new process, it resets pugofer-buffer
to be the new process's buffer. If you only run one process, this will
do the right thing. If you run multiple processes, you can change
pugofer-buffer to another process buffer with \\[set-variable].

More sophisticated approaches are, of course, possible. If you find yourself
needing to switch back and forth between multiple processes frequently,
you may wish to consider ilisp.el, a larger, more sophisticated package
for running inferior Lisp and Pugofer processes. The approach taken here is
for a minimal, simple implementation. Feel free to extend it.")

(defun pugofer-proc ()
  "Returns the current pugofer process. See variable pugofer-buffer."
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-pugofer-mode)
				      (current-buffer)
				      pugofer-buffer))))
    (or proc (run-pugofer pugofer-program-name)))) ;rpm (a hack?)
;	(error "No current process. See variable pugofer-buffer"))))


;;; Do the user's customisation...

(defvar pugofer-load-hook nil
  "This hook is run when pugofer is loaded in.
This is a good place to put keybindings.")
	
(run-hooks 'pugofer-load-hook)


;;; CHANGE LOG
;;; ===========================================================================
;;; 8/88 Olin
;;; Created. 
;;;
;;; 2/15/89 Olin
;;; Removed -emacs flag from process invocation. It's only useful for
;;; cpugofer, and makes cpugofer assume it's running under xpugofer.el,
;;; which messes things up royally. A bug.
;;;
;;; 5/22/90 Olin
;;; - Upgraded to use comint-send-string and comint-send-region.
;;; - run-pugofer now offers to let you edit the command line if
;;;   you invoke it with a prefix-arg. M-x pugofer is redundant, and
;;;   has been removed.
;;; - Explicit references to process "pugofer" have been replaced with
;;;   (pugofer-proc). This allows better handling of multiple process bufs.
;;; - Added pugofer-send-last-sexp, bound to C-x C-e. A gnu convention.
;;; - Have not added process query facility a la cmulisp.el's lisp-show-arglist
;;;   and friends, but interested hackers might find a useful application
;;;   of this facility.
;;;
;;; 3/12/90 Olin
;;; - pugofer-load-file and pugofer-compile-file no longer switch-to-pugofer.
;;;   Tale suggested this.

;;; Around 1993 Rusi
;;; munged together scheme.el and cmuscheme.el
;;; Changed scheme to pugofer and did the minimum to get it 
;;; to work for pugofer

;;; 5 Sept 2015
;;; Changed some defvars to defcustoms

(provide 'pugofer)

;;; original end cmuscheme.el ends here

;;; rpm

(defun pugofer-mode ()
  "Major mode for editing Pugofer code.
Editing commands are similar to those of lisp-mode.

In addition, if an inferior Pugofer process is running, some additional
commands will be defined, for evaluating expressions and controlling
the interpreter, and the state of the process will be displayed in the
modeline of all Pugofer buffers.  The names of commands that interact
with the Pugofer process start with \"xpugofer-\".  For more information
see the documentation for xpugofer-interaction-mode.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{pugofer-mode-map}
Entry to this mode calls the value of pugofer-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (pugofer-mode-initialize)
;  (pugofer-mode-variables)
  (run-hooks 'pugofer-mode-hook))

(defun pugofer-mode-initialize ()
  (use-local-map pugofer-mode-map)
  (setq major-mode 'pugofer-mode)
  (setq mode-name "Pugofer"))

(defun pu-comint-check-source (fname)
  (let ((buff (get-file-buffer fname)))
    (if (and buff
	     (buffer-modified-p buff)
	; (y-or-n-p (format "Save buffer %s first? " (buffer-name buff))))
	; removed (rpm) always saves the gofer file (maybe dangerous)
	     )
	;; save BUFF.
	(let ((old-buffer (current-buffer)))
	  (set-buffer buff)
	  (save-buffer)
	  (set-buffer old-buffer)))))
