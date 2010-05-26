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

;; Should ADORE be silent or verbose ?
(defvar adore-verbosity "true")

;; Switch ADORE in silent mode
(defun adore-silent () (interactive) 
  (setq adore-verbosity "false") (verbosity-status))

;; Switch ADORE in verbose mode
(defun adore-verbose () (interactive)
  (setq adore-verbosity "true") (verbosity-status))

;; Display the verbosity status in the message bar.
(defun verbosity-status () 
  (if (string= adore-verbosity "false")
      (message "Adore is in SILENT mode")
    (message "Adore is in VERBOSE mode")))

;;;;
;; ADORE facts generator
;;;;

;; display the result of the "facts generation" in a newly created buffer
(defun adore-run-genfacts ()
  (let ((f (buffer-file-name)) (current (selected-frame)) 
	(frame (make-frame))   (buffer (generate-new-buffer "ADORE facts")))
    (select-frame frame)
    (switch-to-buffer buffer)
    (call-process-shell-command "adore2facts.sh" nil buffer t f)
    (select-frame current)))

;;;;
;; Handling the ADORE engine
;;;;

;; ELisp function used to run the "adore-wrapper.sh' program in a new buffer
;;  @param g the goal to execute trought the ADORE wrapper
;;  @param buffer-name the name of the expected buffer
;;  @shouldKill if true, will kill the buffer if adore-wrapper ends properly
;;  @return adore-wrapper exit code (> 0 means error)
(defun adore-exec-synchronous (g buffer-name shouldKill)
  (let ((f (buffer-file-name)) (current (selected-frame)) 
	(frame (make-frame)) (buffer (generate-new-buffer buffer-name))
	(opt-g (concat "\"" g "\"")))
    (select-frame frame) (switch-to-buffer buffer)
    (message (concat "Adore: adore-wrapper.sh -f " 
		     f " -g " opt-g " -v " adore-verbosity))
    (let ((code (call-process-shell-command "adore-wrapper.sh" nil buffer t
					    "-f" f "-g" opt-g
					    "-v" adore-verbosity)))
      (message (concat "Adore : adore-wrapper.sh ends with exit code " 
		       (number-to-string code)))
      (if (and shouldKill (= code 0)) (delete-frame frame)) code)))

;; Generate a unique picture name for this process
;;  @param name a prefix used in the generated name
;;  @return a unique (temporary) file name
(defun adore-gen-picture-name (name) 
  (make-temp-file (concat "adore-" name "-") nil ".png"))

;; Display a given picture file using the ADORE_VIEWER command
;;  @param file the file to display
(defun adore-display-picture (file)
  (call-process-shell-command "$ADORE_VIEWER" nil nil t file))

;;;;
;; Adore Interactive engine
;;;;

;; the frame used by the interactive engine
(defvar adore-frame nil "The frame wich contains the ADORE buffer")
;; the process associated with the interactive engine
(defvar adore-process nil "The ADORE underlying process")

;; Init the ADORE interactive engine display
(defun adore-init-display (b)
  (setq adore-frame (make-frame))
  (select-frame adore-frame)
  (switch-to-buffer b))

;; Kill the engine (and the associated frame), if needed
(defun adore-kill-engine ()
  (if adore-process 
      (progn 
	(kill-buffer (process-buffer adore-process))
	(delete-process adore-process)))
  (setq adore-process nil)
  (if adore-frame (delete-frame adore-frame))
  (setq adore-frame nil))

;; Run the engine in an interactive buffer, displayed in the ADORE frame
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
;;;;
;; Get process id from a 'block'
;;;;

;; extract regexp value from str
(defun adore-extract (regexp str)
  (if (equal nil (string-match regexp str)) 
      nil
    (substring str (car (match-data)) (cadr (match-data)))))

;; line classifier (orchestration of fragment)
(defun adore-get-kind (line)
  (if (not (equal nil 
		  (string-match "orchestration [a-zA-Z0-9]+::[a-zA-Z0-9]+ {" 
				line))) 'orchestration
    (if (not (equal nil (string-match "fragment [a-zA-Z0-9]+<?" line)))
	'fragment
      'unknwon)))

;; Read the content of the current line
(defun adore-read-line () 
  (let ((start (point)))
      (forward-line 1) (forward-char -1)
      (buffer-substring start (point))))

;; extract the ADORE ID for the current definition block
(defun adore-extract-id () 
  (interactive)
  (save-excursion 
    (c-beginning-of-defun)
    (let* ((line (adore-read-line))
	   (kind (adore-get-kind line)))
      (cond ((equal kind 'orchestration) 
	     (let ((target (split-string 
			    (adore-extract "[a-zA-Z0-9]+::[a-zA-Z0-9]+"
					   line) "::" )))
	       (format "%s_%s" (car target) (cadr target))))
	    ((equal kind 'fragment) 
	     (let ((name (adore-extract " [a-zA-Z0-9]+" line)))
	       (substring name 1)))))))

;;;;
;; Adore Navigation
;;;;

(defun adore-navigate-to-required-file ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((fileName (adore-extract 
		     "\\([a-zA-Z0-9_]+/\\)*[a-zA-Z0-9_]+.adore" 
		     (adore-read-line))))
      (if (equal nil fileName)
	  (message "Adore: Unable to guess file name")
	(let ((frame (make-frame)))
	  (select-frame frame)
	  (switch-to-buffer (find-file-noselect fileName)))))))

;;;;
;; high level function for the UI
;;;;
(defun adore-run   () (interactive) (adore-run-engine))
(defun adore-kill  () (interactive) (adore-kill-engine))
(defun adore-facts () (interactive) (adore-run-genfacts))

(defun adore-pict  () (interactive) 
  (let* ((i (read-from-minibuffer "Process Id: "))
	 ;;(f (make-temp-file "adore-pict" nil ".png")))
	 (f (adore-gen-picture-name i)))
    (adore-exec-synchronous (concat "adore2png(" i ",\'" f "\')") 
			    "Adore Picture" t)
    (adore-display-picture f)))
    
(defun adore-goal  () (interactive)
  (let ((g (read-from-minibuffer "Goal: ")))
    (adore-exec-synchronous g "Adore Goal Execution" nil)))


(defun adore-dgraph  () (interactive)
  (let* ((p (read-from-minibuffer "Process Id: "))
	 ;;(f (make-temp-file "adore-dgraph" nil ".png")))
	 (f (adore-gen-picture-name (concat "dgraph-" p))))
    (adore-exec-synchronous (concat "adore2dgraph(" p ",'" f "')")
			    "Adore Dependecies Graph" t)
    (adore-display-picture f)))
    
(defun adore-complete-dgraph () (interactive)
  (let ((f (adore-gen-picture-name "dgraph")))
    (adore-exec-synchronous (concat "adore2dgraph('" f "')") 
			    "Adore Dependencies Execution" t)
    (adore-display-picture f)))


(defun adore-gen-current-pict () (interactive)
  (let* ((i (adore-extract-id))
	 (f (adore-gen-picture-name i)))
    ;;(f (make-temp-file "adore-pict" nil ".png")))
    (if (equal nil i)
	(message "Adore: Unable to guess the process identifier")
      (progn
	(adore-exec-synchronous (concat "adore2png(" i ",\'" f "\')") 
				"Adore Picture" t)
	(adore-display-picture f)))))

;;;;;
;; Adore do'n show interface
;;;;;

(defun adore-do-n-show (goal f bufName)
  (let ((code (adore-exec-synchronous (concat goal "(\'" f "\')") bufName t)))
    (if (= 0 code)
	(let ((frame (make-frame)))
	  (select-frame frame)
	  (switch-to-buffer (find-file-noselect f))))))

(defun adore-do-n-show-temp (goal prefix bufName extension)
  (let ((f (make-temp-file prefix nil extension)))
    (adore-do-n-show goal f bufName)))
	

(defun adore-normalize-compositions () (interactive)
  (adore-do-n-show-temp "doContextNormalization,dumpCompositions"
		   "adore-normalized" "Composition Context Normalization"
		   ".adore" ))

(defun adore-get-exec-semantic () (interactive)
  (adore-do-n-show-temp "buildExecutionSemantic"
		   "adore-semantic" "Execution Semantic Computation"
		   ".txt" ))


(defun adore-show-universe () (interactive)
  (adore-do-n-show-temp "dumpUniverse" "adore-universe" "Serialized Universe"
			".adore" ))

(defun adore-as-xml () (interactive)
  (let ((f (read-from-minibuffer "XML File Name: ")))
    (adore-do-n-show "dumpUniverseAsXml" f "XML Serialized Universe")))
  
(defun adore-metrics () (interactive)
  (let ((f (read-from-minibuffer "XML File Name: ")))
    (adore-do-n-show "writeMetricsIntoFile" f "Universe Metrics")))

(defun adore-gen-dot-code () (interactive)
  (let* ((processId (read-from-minibuffer "Process Id: "))
	 (f (make-temp-file (concat processId "-")  nil ".dot"))
	 (code (adore-exec-synchronous (concat "adore2dotfile(" 
					       processId ",\'" f "\')") 
				       "Graphviz Code (Generated by Adore)" t)))
    (if (= 0 code)
	(let ((frame (make-frame)))
	  (select-frame frame)
	  (switch-to-buffer (find-file-noselect f))))))

;;;;
;; Adore IDE reload
;;;;

(defun adore-mode-reload () (interactive)
  (unload-feature 'adore-mode)
  (load-file (concat (getenv "ADORE_HOME") "/adore.el"))
  (adore-mode)
  (message "Adore: Emacs major mode reloaded!"))

;;;;
;; ADORE Keymap
;;;;
(defvar adore-map nil "Enhanced keymap for adore-mode")
(setq adore-map (make-sparse-keymap "Adore KeyMap"))
;; Keynoard shortcuts
(define-key adore-map (kbd "C-c C-r") 'adore-run)
(define-key adore-map (kbd "C-c C-f") 'adore-facts)
(define-key adore-map (kbd "C-c C-k") 'adore-kill)
(define-key adore-map (kbd "C-c C-p") 'adore-gen-current-pict)
(define-key adore-map (kbd "C-c p") 'adore-pict)
(define-key adore-map (kbd "C-c C-g") 'adore-goal)
(define-key adore-map (kbd "C-c C-o") 'adore-navigate-to-required-file)
(define-key adore-map (kbd "C-c C-u") 'adore-show-universe)
;; Menu Bar
(let ((menuMap (make-sparse-keymap "Adore Menu KeyMap")))
  (define-key adore-map [menu-bar adore] (cons "Adore" menuMap))
  (define-key menuMap [reload] '("Reload Adore Major Mode" . adore-mode-reload))
  (define-key menuMap [s0] '("--")) 
  (define-key menuMap [universe] '("Show Universe" . adore-show-universe))
  (define-key menuMap [open-required] '("Naviguate to file" . 
					adore-navigate-to-required-file))
  (define-key menuMap [curr-pict] '("Gen. Current Pict." . 
				    adore-gen-current-pict))
  (define-key menuMap [s1] '("--"))  
  (let ((genMap (make-sparse-keymap "ADORE Gen KeyMap")))
    (define-key menuMap [generate] (cons "Export" genMap))
    (define-key genMap [complete-dgraph] 
      '("Global Dep. Graph" . adore-complete-dgraph))
    (define-key genMap [dgraph] '("Dependencies Graph..." . adore-dgraph))
    (define-key genMap [s0] '("--"))
    (define-key genMap [graphviz] '("Graphviz Code..." . adore-gen-dot-code))
    (define-key genMap [pict] '("PNG Picture..." . adore-pict))
    (define-key genMap [s1] '("--"))
    (define-key genMap [metrics] '("Process Metrics..." . adore-metrics))
    (define-key genMap [xml] '("As XML..." . adore-as-xml)))
  (let ((algoMap (make-sparse-keymap "ADORE Algo KeyMap")))
    (define-key menuMap [algo] (cons "Algorithms" algoMap))
    (define-key algoMap [norm] '("Composition Normalisation" . 
				 adore-normalize-compositions))
    (define-key algoMap [exec] '("Execution Semantic" . 
				 adore-get-exec-semantic)))
  (define-key menuMap [s2] '("--"))
  (let ((prologMap (make-sparse-keymap "Prolog KeyMap")))
    (define-key menuMap [prolog] (cons "Prolog" prologMap))
    (define-key prologMap [goal] '("Shout Universe ..." . adore-goal))
    (define-key prologMap [facts] '("Show Prolog Facts" . adore-facts)))
  (let ((engineMap (make-sparse-keymap "ADORE Engine KeyMap")))
    (define-key menuMap [engine] (cons "Adore Engine" engineMap))
    (let ((verbMap (make-sparse-keymap "ADORE Verb KeyMap")))
      (define-key engineMap [verbose] (cons "Verbose Mode" verbMap))
      (define-key verbMap [silent] '("Disabled" . adore-silent))
      (define-key verbMap [verbose] '("Enabled" . adore-verbose)))
    (define-key engineMap [kill] '("Kill Existing Session" . adore-kill))
    (define-key engineMap [run] '("Run Interactive Session" . adore-run))))

;;;;
;; ADORE Mode
;;;;
(define-derived-mode adore-mode c-mode
  "adore mode"
  "Major mode for editing ADORE textual descriptions"
  (setq mode-name "Adore Editor")
  (setq c-basic-offset 2)
  (use-local-map adore-map)

  (defvar adore-keywords 
    '("require" "orchestration" "fragment" "composition" "knowledge" 
      "depict" "using"))
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