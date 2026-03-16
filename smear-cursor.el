;;; smear-cursor.el --- Renders smear trail behind the cursor  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/conao3/smear-cursor.el

;; Original Author: mattn <mattn.jp@gmail.com>
;; Original URL: https://github.com/mattn/vim-smear-cursor
;; Original License: MIT

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Renders smear trail behind the cursor.


;;; Code:

(require 'cl-lib)

(defgroup smear-cursor nil
  "Renders smear trail behind the cursor."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/smear-cursor.el"))

(defcustom smear-cursor-erase-interval 0.01
  "Interval in seconds between animation erase steps."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-min-distance 3
  "Minimum cursor movement distance to trigger the smear effect."
  :type 'number
  :group 'smear-cursor)

(defcustom smear-cursor-aspect 2.0
  "Aspect ratio correction: row distances are multiplied by this value."
  :type 'float
  :group 'smear-cursor)

(defcustom smear-cursor-head-width 0.9
  "Smear half-width at the head (new cursor position)."
  :type 'float
  :group 'smear-cursor)

(defcustom smear-cursor-tail-width 0.1
  "Smear half-width at the tail (previous cursor position)."
  :type 'float
  :group 'smear-cursor)

(defface smear-cursor-lv1
  '((t :foreground "#e0e0e0"))
  "Smear highlight level 1, head (brightest)."
  :group 'smear-cursor)

(defface smear-cursor-lv2
  '((t :foreground "#b0b0b0"))
  "Smear highlight level 2."
  :group 'smear-cursor)

(defface smear-cursor-lv3
  '((t :foreground "#888888"))
  "Smear highlight level 3."
  :group 'smear-cursor)

(defface smear-cursor-lv4
  '((t :foreground "#666666"))
  "Smear highlight level 4."
  :group 'smear-cursor)

(defface smear-cursor-lv5
  '((t :foreground "#484848"))
  "Smear highlight level 5."
  :group 'smear-cursor)

(defface smear-cursor-lv6
  '((t :foreground "#303030"))
  "Smear highlight level 6, tail (dimmest)."
  :group 'smear-cursor)

(defconst smear-cursor--eighth-blocks
  ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"]
  "Bottom-aligned eighth-block characters ordered by coverage.")

(defconst smear-cursor--faces
  [smear-cursor-lv1 smear-cursor-lv2 smear-cursor-lv3
   smear-cursor-lv4 smear-cursor-lv5 smear-cursor-lv6]
  "Face names for smear gradient, head (bright) to tail (dim).")

(defvar smear-cursor--prev-pos nil
  "Previous cursor position as (buffer line col), or nil.")

(defvar smear-cursor--entries nil
  "Alist of (overlay . t-val) pairs sorted ascending by t-val.")

(defvar smear-cursor--timer nil
  "Active timer for progressive smear erasure.")

(defun smear-cursor--stop ()
  "Cancel the animation timer and remove all smear overlays."
  (when smear-cursor--timer
    (cancel-timer smear-cursor--timer)
    (setq smear-cursor--timer nil))
  (dolist (entry smear-cursor--entries)
    (delete-overlay (car entry)))
  (setq smear-cursor--entries nil))

(defun smear-cursor--erase (_timer)
  "Erase approximately one third of remaining smear overlays."
  (if (null smear-cursor--entries)
      (smear-cursor--stop)
    (let* ((total (length smear-cursor--entries))
           (n     (min total (max 5 (ceiling (/ total 3.0))))))
      (dotimes (i n)
        (delete-overlay (car (nth i smear-cursor--entries))))
      (setq smear-cursor--entries (nthcdr n smear-cursor--entries))
      (when (null smear-cursor--entries)
        (smear-cursor--stop)))))

(defun smear-cursor--pos-at-line-col (buf line col)
  "Return position in BUF at LINE (1-based) and COL (0-based), or nil."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (when (= 0 (forward-line (1- line)))
        (let ((eol (line-end-position)))
          (move-to-column col)
          (when (and (< (point) eol)
                     (= (current-column) col))
            (point)))))))

(defun smear-cursor--on-cursor-moved ()
  "Detect cursor movement and render a smear trail."
  (let* ((buf      (current-buffer))
         (cur-line (line-number-at-pos))
         (cur-col  (current-column)))
    (when (and smear-cursor--prev-pos
               (eq buf (nth 0 smear-cursor--prev-pos)))
      (let* ((prev-line (nth 1 smear-cursor--prev-pos))
             (prev-col  (nth 2 smear-cursor--prev-pos))
             (dr        (- cur-line prev-line))
             (dc        (- cur-col  prev-col))
             (dist      (sqrt (+ (* dr dr 1.0) (* dc dc)))))
        (when (>= dist smear-cursor-min-distance)
          (smear-cursor--stop)
          (let* ((r0     (float prev-line))
                 (c0     (float prev-col))
                 (r1     (float cur-line))
                 (c1     (float cur-col))
                 (dra    (* (- r1 r0) smear-cursor-aspect))
                 (dca    (- c1 c0))
                 (len-sq (+ (* dra dra) (* dca dca))))
            (when (>= len-sq 0.001)
              (let* ((margin (1+ (ceiling smear-cursor-head-width)))
                     (rmin   (max 1 (- (min prev-line cur-line) margin)))
                     (rmax   (+    (max prev-line cur-line) margin))
                     (cmin   (max 0 (- (min prev-col  cur-col)  margin)))
                     (cmax   (+    (max prev-col  cur-col)  margin)))
                (cl-loop for row from rmin to rmax do
                  (cl-loop for col from cmin to cmax do
                    (unless (or (and (= row prev-line) (= col prev-col))
                                (and (= row cur-line)  (= col cur-col)))
                      (let* ((cr    (float row))
                             (cc    (float col))
                             (dqr   (* (- cr r0) smear-cursor-aspect))
                             (dqc   (- cc c0))
                             (t-val (/ (+ (* dqr dra) (* dqc dca)) len-sq)))
                        (when (and (>= t-val -0.05) (<= t-val 1.05))
                          (let* ((ct     (max 0.0 (min 1.0 t-val)))
                                 (proj-r (+ r0 (* ct (- r1 r0))))
                                 (proj-c (+ c0 (* ct (- c1 c0))))
                                 (pr     (* (- cr proj-r) smear-cursor-aspect))
                                 (pc     (- cc proj-c))
                                 (perp   (sqrt (+ (* pr pr) (* pc pc))))
                                 (hw     (+ smear-cursor-tail-width
                                            (* (- smear-cursor-head-width
                                                  smear-cursor-tail-width)
                                               ct))))
                            (when (< perp hw)
                              (let* ((coverage (* (- 1.0 (/ perp hw))
                                                  (/ hw smear-cursor-head-width)))
                                     (blk-idx  (max 0 (min 7 (round (* coverage 7.0)))))
                                     (n-faces  (length smear-cursor--faces))
                                     (lv       (max 0 (min (1- n-faces)
                                                           (round (* (- 1.0 ct)
                                                                     (1- n-faces))))))
                                     (ch       (aref smear-cursor--eighth-blocks blk-idx))
                                     (face     (aref smear-cursor--faces lv))
                                     (pos      (smear-cursor--pos-at-line-col buf row col)))
                                (when pos
                                  (let ((ov (make-overlay pos (1+ pos) buf)))
                                    (overlay-put ov 'display (propertize ch 'face face))
                                    (push (cons ov ct) smear-cursor--entries))))))))))))
              (setq smear-cursor--entries
                    (sort smear-cursor--entries (lambda (a b) (< (cdr a) (cdr b)))))
              (when smear-cursor--entries
                (setq smear-cursor--timer
                      (run-with-timer smear-cursor-erase-interval
                                      smear-cursor-erase-interval
                                      #'smear-cursor--erase
                                      nil))))))))
    (setq smear-cursor--prev-pos (list buf cur-line cur-col))))

;;;###autoload
(define-minor-mode smear-cursor-mode
  "Toggle smear trail effect for cursor movement."
  :global t
  :lighter " Smear"
  (if smear-cursor-mode
      (add-hook 'post-command-hook #'smear-cursor--on-cursor-moved)
    (remove-hook 'post-command-hook #'smear-cursor--on-cursor-moved)
    (smear-cursor--stop)
    (setq smear-cursor--prev-pos nil)))

(provide 'smear-cursor)

;;; smear-cursor.el ends here
