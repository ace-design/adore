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
;; strongly inspired by http://xahlee.org/emacs/elisp_syntax_coloring.html
;;;;

(defvar adore-mode-map nil "Keymap for adore-mode")

(if adore-mode-map
    ()
  (setq adore-mode-map (make-sparse-keymap)) 
  (define-key adore-mode-map (kbd "C-c C-r") 'adore-run)
  (define-key adore-mode-map (kbd "C-c C-e") 'adore-run-empty)
  (define-key adore-mode-map (kbd "C-c C-f") 'adore-facts)
  (define-key adore-mode-map (kbd "C-c C-k") 'adore-kill)
  (define-key adore-mode-map (kbd "C-c C-a") 'adore-ask)
  
  
;  (define-key adore-mode-map [remap comment-dwim] 'adore-comment-dwim)
  (define-key adore-mode-map [menu-bar] (make-sparse-keymap))
  (let ((menuMap (make-sparse-keymap "ADORE"))) 
    (define-key adore-mode-map [menu-bar adore] (cons "ADORE" menuMap)) 
    (define-key menuMap [ask] '("Ask ADORE" . adore-ask))
    (define-key menuMap [kill] '("Kill subprocess" . adore-kill))
    (define-key menuMap [s0] '("--"))
    (define-key menuMap [facts] '("Generate facts from file" . adore-facts))
    (define-key menuMap [s1] '("---"))
    (define-key menuMap [run] '("Run engine using file" . adore-run))
    (define-key menuMap [run_empty] '("Run empty engine" . adore-run-empty))
    ))

(defun adore-facts ()
  (interactive)
  (start-process-shell-command "adore" "adore" 
			       "adore2facts.sh" buffer-file-name)
  (let ((old (current-buffer)))
    (pop-to-buffer "adore")
    (pop-to-buffer old)))

(defun adore-kill ()
  (interactive)
  (kill-buffer "adore")
  (switch-to-buffer (current-buffer)))

(defun ordinary-insertion-filter (proc string)
  (let ((old-buffer (current-buffer)))
    (display-buffer (process-buffer proc))
    (unwind-protect
        (let (moving)
          (set-buffer (process-buffer proc))
          (setq moving (= (point) (process-mark proc)))
          (save-excursion
            ;; Insert the text, moving the process-marker.
            (goto-char (process-mark proc))
            (insert string)
            (set-marker (process-mark proc) (point)))
          (goto-char (process-mark proc)))
      (set-buffer old-buffer))))

(defun adore-run-empty ()
  (interactive)
  (set-process-filter (start-process-shell-command "adore" "adore" 
						   "adore.sh")
		      'ordinary-insertion-filter))

;; (defun adore-run-empty ()
;;   (interactive)
;;   (start-process-shell-command "adore" "adore" 
;; 			       "adore.sh")
;;   (let ((old (current-buffer)))
;;     (pop-to-buffer "adore")
;;     (pop-to-buffer old)))

;; (defun adore-run ()
;;   (interactive)
;;   (start-process-shell-command "adore" "adore" 
;; 			       "adore.sh" buffer-file-name)
;;   (let ((old (current-buffer)))
;;     (pop-to-buffer "adore")
;;     (pop-to-buffer old)))

(defun adore-ask ()
  (interactive)
  (process-send-string "adore" (read-from-minibuffer "ADORE Command: ")))


(define-derived-mode adore-mode fundamental-mode
  "adore mode"
  "Major mode for editing ADORE textual descriptions"
  (c-mode)
  (use-local-map adore-mode-map)

  (defvar adore-keywords 
    '("require" "orchestration" "fragment" "composition" ))
  (defvar adore-keywords-regexp (regexp-opt adore-keywords 'words))
  (setq adore-keywords nil)

  (defvar adore-activities
    '("nop" "receive" "reply" "throw" "when" "as" "apply" "hook" "^" "$"))
  (defvar adore-activities-regexp (regexp-opt adore-activities 'words))
  (setq adore-activities nil)

  (defvar adore-internal
    '("variables" "activities" "relations"))
  (defvar adore-internal-regexp (regexp-opt adore-internal 'words))
  (setq adore-internal nil)

  (defvar adore-types
    '("integer" "boolean" "struct" "float" "string" "char" "time" "date" "dateTime"))
  (defvar adore-types-regexp (regexp-opt adore-types 'words))
  (setq adore-types nil)
  
  (setq adore-font-lock-keywords
	`((,adore-activities-regexp . font-lock-builtin-face)
	  (,adore-keywords-regexp .   font-lock-keyword-face)
	  (,adore-internal-regexp .   font-lock-constant-face)
	  (,adore-types-regexp .      font-lock-type-face)
	  ))

  (setq font-lock-defaults '((adore-font-lock-keywords)))

  (setq adore-keywords-regexp nil)
  (setq adore-activities-regexp nil)
  (setq adore-types-regexp nil)
  (setq adore-internal-regexp nil)
  

  (setq c-basic-offset 2)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'block-open 0)
  (c-set-offset 'statement-cont 0)
)

(setq auto-mode-alist (append '(("\\.adore$" . adore-mode)) auto-mode-alist))