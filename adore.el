;;;;
;; This file is part of ADORE [ www.adore-design.org ]
;;
;; Copyright (C) 2008-  Sebastien Mosser
;;
;; ADORE is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; ADORE is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with ADORE; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;;
;; @author      Main Sebastien Mosser          [mosser@polytech.unice.fr]
;;;;

;;;;
;; Emacs mode for the ADORE language
;;
;; References and inspirations: 
;;   - http://xahlee.org/emacs/elisp_syntax_coloring.html
;;   - http://www.logic.at/prolog/ediprolog/ediprolog.el
;;   - Emacs usual prolog.el mode
;;;;

;;;;
;; ADORE functions
;;;;
(defvar adore-verbosity "true")
(defun adore-silent () (setq adore-verbosity "false") (verbosity-status))
(defun adore-verbose () (setq adore-verbosity "true") (verbosity-status))
(defun verbosity-status () 
  (if (string= adore-verbosity "false")
      (message "Adore is in SILENT mode")
    (message "Adore is in VERBOSE mode")))
			       
(defun adore-run-genfacts ()
  (let ((f (buffer-file-name)) (current (selected-frame)) 
	(frame (make-frame))   (buffer (generate-new-buffer "ADORE facts")))
    (select-frame frame)
    (switch-to-buffer buffer)
    (call-process-shell-command "adore2facts.sh" nil buffer t f)
    (select-frame current)))

;; Handling the ADORE engine

(defun adore-exec-synchronous (g buffer-name shouldKill)
  (let ((f (buffer-file-name)) (current (selected-frame)) 
	(frame (make-frame)) (buffer (generate-new-buffer buffer-name))
	(opt-g (concat "'" g "'")))
    (select-frame frame)
    (switch-to-buffer buffer)
    (message (concat "Adore: adore-wrapper.sh -f " 
		     f " -g " opt-g " -v " adore-verbosity))
    (let ((code (call-process-shell-command "adore-wrapper.sh" 
					    nil buffer t "-f" f 
					    "-g" opt-g
					    "-v" adore-verbosity)))
      (message (concat "Adore : adore-wrapper.sh ends with exit code " 
		       (number-to-string code)))
      (if (and shouldKill (= code 0)) (delete-frame frame))
      code)))

(defvar adore-frame nil "The frame wich contains the ADORE buffer")
(defvar adore-process nil "The ADORE underlying process")

(defun adore-init-display (b)
  (setq adore-frame (make-frame))
  (select-frame adore-frame)
  (switch-to-buffer b))

(defun adore-kill-engine ()
  (if adore-process 
      (progn 
	(kill-buffer (process-buffer adore-process))
	(delete-process adore-process)))
  (setq adore-process nil)
  (if adore-frame (delete-frame adore-frame))
  (setq adore-frame nil))

(defun adore-run-engine ()
  (adore-kill-engine)
  (require 'comint)
  (let* ((f  (buffer-file-name))
	(b (make-comint "adore" "adore-wrapper.sh" nil "-f" f
			"-v" adore-verbosity)))
    (setq adore-process (get-buffer-process b))
    (adore-init-display b)
    (setq mode-name "ADORE Engine"
	  comint-prompt-regexp "^| [ ?][- ] *")))
    ;;(use-local-map adore-mode-map)
  
 ;; high level function for the UI
(defun adore-run   () (interactive) (adore-run-engine))
(defun adore-kill  () (interactive) (adore-kill-engine))
(defun adore-facts () (interactive) (adore-run-genfacts))

(defun adore-pict  () (interactive) 
  (let ((i (read-from-minibuffer "Process Id: ")))
    (adore-exec-synchronous (concat "display(" i ",_),sleep(1)") 
			    "Adore Picture" t)))

(defun adore-goal  () (interactive)
  (let ((g (read-from-minibuffer "Goal: ")))
    (adore-exec-synchronous g "Adore Goal Execution" nil)))

(defun adore-dgraph  () (interactive)
  (let ((p (read-from-minibuffer "Process Id: ")))
    (adore-exec-synchronous (concat "adore2dgraph(" p "),sleep(1)")
			    "Adore Dependecies Graph" t)))

(defun adore-complete-dgraph () (interactive)
  (adore-exec-synchronous "adore2dgraph,sleep(1)" 
			  "Adore Dependencies Execution" t))


;;;;
;; ADORE Keymap
;;;;
(defvar adore-mode-map nil "Keymap for adore-mode")
(if adore-mode-map  ()
  (setq adore-mode-map (make-sparse-keymap)) 
  (define-key adore-mode-map (kbd "C-c C-r") 'adore-run)
  (define-key adore-mode-map (kbd "C-c C-f") 'adore-facts)
  (define-key adore-mode-map (kbd "C-c C-k") 'adore-kill)
  (define-key adore-mode-map (kbd "C-c C-p") 'adore-pict)
  (define-key adore-mode-map (kbd "C-c C-g") 'adore-goal)
  
  (define-key adore-mode-map [remap comment-dwim] 'adore-comment-dwim)
  (define-key adore-mode-map [menu-bar] (make-sparse-keymap))
  (let ((menuMap (make-sparse-keymap "Adore"))) 
    (define-key adore-mode-map [menu-bar adore] (cons "Adore" menuMap)) 
    (define-key menuMap [goal] '("Execute goal" . adore-goal))
    (define-key menuMap [s3] '("--"))
    (define-key menuMap [dgraph] '("Generate Dep. Graph" . adore-dgraph))
    (define-key menuMap [complete-dgraph] 
      '("Generate Complete Dep. Graph" . adore-complete-dgraph))
    (define-key menuMap [s2] '("--"))
    (define-key menuMap [silent] '("Toggle silence" . adore-silent))
    (define-key menuMap [verbose] '("Toggle verbosity" . adore-verbose))
    (define-key menuMap [s1] '("--"))
    (define-key menuMap [facts] '("Generate facts" . adore-facts))
    (define-key menuMap [pict] '("Generate picture" . adore-pict))
    (define-key menuMap [s0] '("--"))
    (define-key menuMap [kill] '("Kill the Adore engine" . adore-kill))
    (define-key menuMap [run] '("Run the Adore engine" . adore-run))))

;;;;
;; ADORE Mode
;;;;
(define-derived-mode adore-mode c-mode
  "adore mode"
  "Major mode for editing ADORE textual descriptions"
  (setq mode-name "Adore Editor")
  (setq c-basic-offset 2)
  (use-local-map adore-mode-map)

  (defvar adore-keywords 
    '("require" "orchestration" "fragment" "composition" ))
  (defvar adore-keywords-regexp (regexp-opt adore-keywords 'words))

  (defvar adore-activities
    '("const" "nop" "receive" "reply" "throw" "when" "as" "apply" 
      "hook" "^" "$" "toSet" "fail"))
  (defvar adore-activities-regexp (regexp-opt adore-activities 'words))

  (defvar adore-internal
    '("variables" "activities" "relations"))
  (defvar adore-internal-regexp (regexp-opt adore-internal 'words))

  (defvar adore-types
    '("integer" "boolean" "struct" "float" "string" "time" "date" "dateTime"))
  (defvar adore-types-regexp (regexp-opt adore-types 'words))
  
  (setq adore-font-lock-keywords
	`((,adore-activities-regexp . font-lock-builtin-face)
	  (,adore-keywords-regexp .   font-lock-keyword-face)
	  (,adore-internal-regexp .   font-lock-constant-face)
	  (,adore-types-regexp .      font-lock-type-face)))
  (setq font-lock-defaults '((adore-font-lock-keywords))))

;; External interface: providing adore-mode, and enhancing the autoloading
(provide 'adore-mode)
(setq auto-mode-alist (append '(("\\.adore$" . adore-mode)) auto-mode-alist))
