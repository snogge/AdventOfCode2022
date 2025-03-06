;;; monkey-business.el --- AOC 2022 day 11           -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Axis Communications AB

;; Author: Ola x Nilsson <ola.x.nilsson@axis.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'rx)

(cl-defstruct monkey
  number
  items
  operation
  test
  true-target
  false-target)

(defun monkey-investigate (monkey item)
  (funcall (monkey-operation monkey) item))

(defun monkey-throw (monkey item)
  (if (mod item (monkey-test monkey))
	  (progn
		(message "Current worry level %d is not divisible by %d." item (monkey-test monkey)))
	(message "Current worry level %d is divisible by %d." item (monkey-test monkey))))

(defun monkey-turn (monkey monkeys)
  (message "Monkey %d:" (monkey-number monkey))
  (cl-loop for item in (monkey-items monkey)
		   do (message "Monkey inspects an item with a worry level of %d." item)
		   do (setq item (monkey-investigate monkey item))
		   do (message "Worry level is now %d." item)
		   do (setq item (/ item 3))
		   do (message "Monkey gets bored with item. Worry level is divided by 3 to %d." item)
		   do (monkey-throw monkey (monkey-target monkey item)))
  (setf (monkey-items monkey) nil))

(defun read-monkey ()
  "Read a monkey description starting at point."
  (cl-assert (not (looking-at "^\\s-*$")))
  (let ((monkey (make-monkey)))
	(while (not (looking-at "^\\s-*$"))
	  (pcase (buffer-substring (line-beginning-position) (line-end-position))
		((rx (seq "Monkey " (group-n 1 (+ digit)) ":"))
		 (setf (monkey-number monkey) (string-to-number (match-string 1))))
		((rx (seq "Starting items: " (group-n 1 (* (+ digit) ?, (+ blank)) (+ digit))))
		 (setf (monkey-items monkey) (split-string (match-string 1) "[,[:blank:]]" t)))
		((rx (seq "Operation: new = "
				  (group-n 1 (or "old" (+ digit)))
				  (+ blank)
				  (group-n 2 (any "*+"))
				  (+ blank)
				  (group-n 3 (or "old" (+ digit)))))
		 (setf (monkey-operation monkey)
			   (lambda (old) (funcall (intern (match-string 2))
									  (intern (match-string 1))
									  (intern (match-string 3))))))
		((rx (seq "Test: divisible by " (group-n 1 (+ digit))))
		 (setf (monkey-test monkey)
			   (string-to-number (match-string 1))))
		((rx (seq "If " (group-n 1 (or "true" "false")) ": throw to monkey " (group-n 2 (+ digit))))
		 (setf
		  (if (string= (match-string 1) "true") (monkey-true-target monkey) (monkey-false-target monkey))
		  (string-to-number (match-string 2))))
		(_ (error "Unmatched line %s"
				  (buffer-substring (line-beginning-position) (line-end-position))))
		)
	  (forward-line))
	monkey))

(defun read-monkey-file (filename)
  (find-file-read-only filename)
  (goto-char (point-min))
  (cl-loop until (eobp)
		   do (while (looking-at-p "^\\s-*$") (forward-line))
		   collect (read-monkey)))

(defun monkey-around (monkeys)
  (while 1
	(dolist (monkey monkeys)
	  (monkey-turn monkey monkeys))))


(monkey-around (read-monkey-file "example"))


(provide 'monkey-business)
;;; monkey-business.el ends here
