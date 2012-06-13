;;; Gofer mode for GNU Emacs
;;;
;;; Last update: 6/12/91
;;;
;;; Author: Stuart Clayman,
;;;	    Dept. Computer Science,
;;;	    University College London
;;;
;;; Email: sclayman@uk.ac.ucl.cs
;;;
;;; Changed by rpm@cs.unipune.ernet.in
;;;
;;; Use:
;;; In .emacs put
;;;
;;;   (autoload 'pugofer-mode "pugofer" "Go into pugofer mode" t)
;;;   (autoload 'run-pugofer "pugofer" "Run pugofer as inferior process" t)
;;;   (autoload 'pugofer-project "pugofer" "Go into a pugofer project" t)
;;;
;;;   (set-variable 'auto-mode-alist
;;;        (append '(
;;;   		("\\.gs$" . pugofer-mode)   ;; pugofer source
;;;   		("\\.lgs$" . pugofer-mode)   ;; literate pugofer source
;;;		("\\.gp$" . pugofer-project) ;; pugofer project files
;;;		) auto-mode-alist))
;;;
;;;  All pugofer source files should end in .gs or .lgs
;;;  All pugofer project files should end in .gp
;;;
;;;  In pugofer source files
;;;	\C-c l		loads the current file
;;;	\C-u \C-c l	loads the current file and does a cd first
;;;	\C-c a		adds the current file
;;;	\C-u \C-c a	adds the current file and does a cd first
;;;	\C-c r		reloads the current file
;;;
;;;
;;;  In pugofer project files
;;;	\C-c p		loads the project file
;;;	\C-u \C-c p	loads the project file and does a cd first


(require 'comint)
(require 'backquote)

(defvar pugofer-mode-hook nil "Gofer mode hook")

(defun mode-line-config-I ()
  (setq mode-line-buffer-identification '("%b"))
  (setq mode-line-modified nil)
  (setq line-number-mode nil)
  (setq mode-line-format '("" mode-line-modified mode-line-buffer-identification "   " global-mode-string "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]--"  "-%-")))

(defun run-pugofer()
  "Run an inferior Gofer process."
  (interactive)
  (save-excursion
;    (if (null (get-buffer "*pugofer*")) (split-window-horizontally))
    (set-buffer (make-comint "pugofer" "gofer"));
    (setq mode-name "PugoferI")
    (display-buffer (current-buffer))
    (mode-line-config-I)
    ;  (setq comint-cd-pattern ":cd")
    (set-process-filter (get-process "pugofer")  'filter)
    (setq comint-prompt-regexp "^[? ]*? \\|^")))

(defun not-running-pugofer ()              ; true if pugofer is running
    (or (null (get-buffer "*pugofer*")) (null (process-status "pugofer"))))

(defun save-buffer-and-go-outline(which arg)
  "Save current Gofer file buffer.
Goto inferior Gofer buffer and load file.
WHICH operation is required.
With ARG for additional operation"
  (save-buffer)
;  (if (not-running-pugofer)
      (save-excursion (run-pugofer))
;    )

  (cond ((equal which "r") (send-string "pugofer" (concat ":reload" "\n")))
	((equal which "l")
	 (if arg
	     (send-string "pugofer" (concat ":cd " default-directory "\n")))
	 (send-string "pugofer" (concat ":l " (buffer-name) "\n")))
	((equal which "a")
	 (if arg
	     (send-string "pugofer" (concat ":cd " default-directory "\n")))
	 (send-string "pugofer" (concat ":a " (buffer-name) "\n")))
	((equal which "p")
	 (if arg 
	     (send-string "pugofer" (concat ":cd " default-directory "\n")))
	 (send-string "pugofer" (concat ":p " (buffer-name) "\n")))
	(t (message "Bad programming in pugofer.el")))

  (switch-to-buffer-other-window "*pugofer*"))

(defun pugofer-load(arg)
  "Save a pugofer source file and load it"
  (interactive "P")
  (save-buffer-and-go-outline "l" arg))

(defun pugofer-add(arg)
  "Save a pugofer source file and add it to the file list"
  (interactive "P")
  (save-buffer-and-go-outline "a" arg))

(defun pugofer-reload(arg)
  "Save a pugofer source file and reload it"
  (interactive "P")
  (save-buffer-and-go-outline "r" arg))

(defun save-pugofer-project-buffer-and-go(arg)
  "Save a pugofer project file and run"
  (interactive "P")
  (save-buffer-and-go-outline "p" arg))

(defvar pugofer-mode-map nil
  "Keymap for Pugofer mode.")

(if pugofer-mode-map
    ()
  (setq pugofer-mode-map (make-sparse-keymap))
  (define-key pugofer-mode-map "\C-cl" 'pugofer-load)
  (define-key pugofer-mode-map "\C-cr" 'pugofer-reload)
  (define-key pugofer-mode-map "\C-ca" 'pugofer-add)
  (define-key pugofer-mode-map "\eg" 'goto-line))

(defun pugofer-mode()
  "Major Mode for editing pugofer scripts
Special commands:
\\{pugofer-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'pugofer-mode)
  (setq mode-name "PugoferF")
  (use-local-map pugofer-mode-map)
  (make-variable-buffer-local 'indent-line-function)
  (setq indent-line-function 'indent-relative)
  (make-variable-buffer-local 'line-number-mode)
  (setq line-number-mode t)
  (setq mode-line-buffer-identification '("%b"))
  (run-hooks 'pugofer-mode-hook)
)
(defun pugofer-project()
  "For Gofer project files"
  (local-set-key "\C-cp" 'save-pugofer-project-buffer-and-go))

(setq truncate-partial-width-windows nil)

(defun match-subexpr (str n)
  (substring str (match-beginning n) (match-end n)))

(setq endhead (make-marker))
(setq beghead (make-marker))

(setq spchr "%")
(setq cmdStart (concat spchr "("))
(setq cmdEnd (concat ")" spchr))
(setq cmdRe (concat "\\(" spchr "\\(.*\\)" spchr "\\)"))
(setq state 'default)

(defun filter (proc str)
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let (moving)
	  (set-buffer (process-buffer proc))
;	  (setq moving (= (point) (process-mark proc)))

	  (save-excursion
	    ;; Insert the text, moving the process-marker.
	;    (goto-char (process-mark proc))
	    (cond ((eq state 'default)
		   (cond ((string-match cmdRe str)
			  (remain-insert str proc)
			  (pu-exec-cmd str))
			 ((string-match cmdStart str)
			  (setq state 'inCommand)
			  (print (format "halfbaked %s" str))
			  (setq cmdStr str))
			 (t (insert-proc str proc))
		    ))
		  ((eq state 'inCommand)
		   (setq cmdStr (concat cmdStr str))
		   (cond ((string-match cmdEnd str)
			  (setq state 'default)
			  (pu-exec-cmd cmdStr))))
		  (t (error "Bad state" state)))
	    
	    ;(set-marker (process-mark proc) (point))
	    )
	  (goto-char (process-mark proc))
	  )
      (set-buffer old-buffer))))

(defun remain-insert (str proc)
  (string-match cmdRe str)
  (insert-proc (substring str (1+ (match-end 0))) proc))

(defun insert-proc (str proc)
  (goto-char (process-mark proc))
  (insert str)
  (set-marker (process-mark proc) (point-max)))

(defun pu-exec-cmd (str)
  (string-match cmdRe str)
  (save-excursion (eval (read (match-subexpr str 2)))))

(defun pu-find-file (&optional f l)
  (cond ((null l) (setq l 0)))
  (cond ((null f) (setq f "*scratch*")))
  (find-file-other-window f)
  (goto-line l)
)

(defun pu-banner (s)
  (insert s)
  (set-marker beghead (point))
  (set-marker endhead (point))
  (set-marker (getpupos) (point))
)

(defun pu-what-files (prj &rest flst)
; prj not yet done
  (delete-region beghead endhead)
  (goto-char beghead)
  (insert "Gofer session for: ")
  (insert (format "%S\n" flst))
  (set-marker endhead (point))
  (set-marker (getpupos) (point-max))
  (goto-char (point-max)))

(defun getpupos ()
  (process-mark (get-process "pugofer")))

