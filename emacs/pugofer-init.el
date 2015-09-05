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
;; For now you need to hand-change these paths

(defun run-pugofer-std ()
  (interactive)
  (setenv "PUGOFER" "/home/rusi/gofergithub/gofer/pustd.pre")
  (run-pugofer pugofer-program-name))

(defun run-pugofer-simple ()
  (interactive)
  (setenv "PUGOFER" "/home/rusi/gofergithub/gofer/pusimple.pre")
  (run-pugofer pugofer-program-name))

(defun run-pugofer-cc ()
  (interactive)
  (setenv "PUGOFER" "/home/rusi/gofergithub/gofer/pucc28.pre")
  (run-pugofer pugofer-program-name))
