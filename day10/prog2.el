;;; prog.el --- AOC 2022 DAY 10  -*- lexical-binding: t -*-

(defvar crt (make-vector (* 40 6) (aref " " 0)))

(defun draw (cycle X)
  (setq cycle (1- cycle))
  (when (member (mod cycle 40) (list (1- X) X (1+ X)))
	(setf (aref crt cycle) ?#)))

(progn
  (setq crt (make-vector (* 40 6) (aref " " 0)))
  (find-file "input")
  (goto-char (point-min))
  (let ((X 1)
		(cycle 1))
	(while (not (eobp))
	  (pcase (split-string (buffer-substring (point) (line-end-position)))
		(`("addx" ,V)
		 (draw cycle X)
		 (cl-incf cycle)
		 (draw cycle X)
		 (cl-incf cycle)
		 (cl-incf X (string-to-number V)))
		(`("noop")
		 (draw cycle X)
		 (cl-incf cycle 1)
		 ))
	  (forward-line)))
  (cl-dolist (line (seq-partition crt 40))
	(message "%s" (seq-into line 'string))))

;;whetever
