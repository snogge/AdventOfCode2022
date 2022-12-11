;;; prog.el --- AOC 2022 DAY 10  -*- lexical-binding: t -*-

(defvar result 0)

(defun cycle-hook (cycle X)
  (when (member cycle '(20 60 100 140 180 220))
	(message "(%d * %d) => %d + %d => %d "
			 cycle X (* cycle X)
			 result (cl-incf result (* cycle X)))))

(progn
  (setq result 0)
  (find-file "input")
  (goto-char (point-min))
  (let ((X 1)
		(cycle 1))
	(while (not (eobp))
	  (pcase (split-string (buffer-substring (point) (line-end-position)))
		(`("addx" ,V)
		 (cycle-hook cycle X)
		 (cl-incf cycle)
		 (cycle-hook cycle X)
		 (cl-incf cycle)
		 (cl-incf X (string-to-number V)))
		(`("noop")
		 (cycle-hook cycle X)
		 (cl-incf cycle 1)
		 ))
	  (forward-line))))
