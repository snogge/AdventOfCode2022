;;; prog1.el ---                                     -*- lexical-binding: t; -*-

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

(cl-defstruct position
  "Position on the plane."
  (x 0)
  (y 0))

(defun position= (p1 p2)
  "Return t if two position objects are equal."
  (and
   (= (position-x p1) (position-x p2))
   (= (position-y p1) (position-y p2))))

(defun position- (p1 p2)
  (make-position
   :x (- (position-x p1) (position-x p2))
   :y (- (position-y p1) (position-y p2))))

(defun position-incf (p1 p2)
  (cl-incf (position-x p1) (position-x p2))
  (cl-incf (position-y p1) (position-y p2)))

(defun position-follow (head tail)
  (make-position
   :x (min 1 (max -1 (- (position-x head) (position-x tail))))
   :y (min 1 (max -1 (- (position-y head) (position-y tail))))))

(defun close-p (pos1 pos2)
  "Return t if POS1 and POS2 is no more than 1 step apart."
  (and
   (< (abs (- (position-x pos1) (position-x pos2))) 2)
   (< (abs (- (position-y pos1) (position-y pos2))) 2)))

(defun position-step (position direction)
  "Move POSITION 1 step in DIRECTION.
DIRECTION is ?U, ?R, ?D, ?L or one of the symbols `ur', `dr',
`dl', or `ul'."
  (cl-ecase direction
	(?U (cl-incf (position-y position) 1))
	(?R (cl-incf (position-x position) 1))
	(?D (cl-incf (position-y position) -1))
	(?L (cl-incf (position-x position) -1))
	(ur (cl-incf (position-x position) 1)
		(cl-incf (position-y position) 1))
	(dr (cl-incf (position-x position) 1)
		(cl-incf (position-y position) -1))
	(dl (cl-incf (position-x position) -1)
		(cl-incf (position-y position) -1))
	(ul (cl-incf (position-x position) -1)
		(cl-incf (position-y position) 1))
	))

(cl-defstruct (rope
			   (:constructor nil)
			   (:constructor new-rope (&key (len 2)
											 &aux
											 (length len)
											 (knots (cl-loop repeat len
															 collect (make-position))))))
  "A rope with head and tail."
  (length 2)
  knots
  (trail (list (make-position))))

(defun rope-head (rope)
  (car (rope-knots rope)))

(defun rope-tail (rope)
  (car (last (rope-knots rope))))

(defun move (rope direction count)
  "Move the head of ROPE one step in DIRECTION COUNT times."
  (dotimes (_ count)
	(position-step (rope-head rope) direction)
	(cl-loop for (lead follow _) on (rope-knots rope)
			 unless (position-p follow) return nil
			 do (unless (close-p lead follow)
				  (position-incf follow (position-follow lead follow))))
	(unless (cl-member (rope-tail rope) (rope-trail rope) :test 'position=)
	  (push (copy-position (rope-tail rope)) (rope-trail rope)))))

(let ((rope (new-rope :len 10)))
  (find-file "input")
  (goto-char (point-min))
  (while (not (eobp))
	(pcase-let ((`(,direction ,count)
				 (split-string (buffer-substring (point) (line-end-position)))))
	  (move rope (elt direction 0) (string-to-number count)))
	(forward-line))
  (message "%d should be 5981" (length (rope-trail rope)))
  )



(provide 'prog1)
;;; prog1.el ends here
