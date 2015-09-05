;; Setups for emacs for using pugofer mode
;; Assumes you have pugofer.el on the load-path

(autoload 'pugofer-mode "pugofer.el" nil t)
(autoload 'run-pugofer "pugofer.el" nil t)
(autoload 'pugofer-project "pugofer.el" nil t)
(set-variable 'auto-mode-alist
      (append '(
	      ("\\.gs$" . pugofer-mode)   ;; gofer source
	      ("\\.lgs$" . pugofer-mode)   ;; literate gofer source
	      ("\\.gp$" . pugofer-project) ;; gofer project files
	      ) auto-mode-alist))



;; These should go into pugofer.el
(defun run-pugofer-std ()
  (interactive)
  (setenv "PUGOFER" "/home/rusi/gofergithub/gofer/src/pustd.pre")
  (run-pugofer pugofer-program-name))

(defun run-pugofer-simple ()
  (interactive)
  (setenv "PUGOFER" "/home/rusi/gofergithub/gofer/src/pusimple.pre")
  (run-pugofer pugofer-program-name))
