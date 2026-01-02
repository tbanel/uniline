;;; uniline-bench.el --- Regression tests for Uniline  -*- coding:utf-8; lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Thierry Banel

;; Author: Thierry Banel tbanelwebmin at free dot fr
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (hydra "0.15.0"))
;; URL: https://github.com/tbanel/uniline

;; Uniline is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Uniline is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Running regression tests
;; Just load this file:
;;   (load "uniline-bench.el")
;; or
;;   (eval-buffer)
;;
;; If OK, a message is displayed
;; If ERROR, two windows are displayed, the actual and the expected sketchs
;; with points on the first difference.

;; Creating a new bench
;; - Eval (uniline-bench-create)
;; - Draw a sketch
;; - When done, type $
;; A Lisp buffer implementing the new test is displayed
;; - Save it permanently in a file ending in *.el along with this one

;;; Code:

(defvar uniline-bench-result
  nil
  "Boolean where a bench result is stored.
t if the bench ran as expected.
nil if there was an error.")

(defun uniline-bench (commands result)
  "Run a bench.
COMMANDS is a string describing a sequence of keyboard strokes,
supposed to draw a sketch using uniline minor-mode.
Its format is the one used to store keyboard macros.
RESULT is a string representing the expected result."
  (ignore-errors (kill-buffer "*uniline-interactive*"))
  (switch-to-buffer "*uniline-interactive*")
  ;; (set-default 'uniline-hint-style 1)
  (let ((uniline-show-welcome-message nil))
    (uniline-mode 1))
  (if (fboundp 'hydra-keyboard-quit)
      (hydra-keyboard-quit)) ;; clear any left-over from previous bench
  (if (fboundp 'transient-quit-all)
      (transient-quit-all))  ;; clear any left-over from previous bench
  (setq uniline--which-quadrant (uniline--char-to-4quadb ?▘))
  (uniline-set-brush-1)
  (uniline-set-brush-0dots)
  (execute-kbd-macro (kbd commands))
  
  (setq uniline-bench-result
        (string-equal
         (buffer-substring (point-min) (point-max))
         result))
  
  (unless uniline-bench-result
    (delete-other-windows)
    (switch-to-buffer "*uniline-interactive*")
    (goto-char (point-min))
    (ignore-errors (kill-buffer "*uniline-expected*"))
    (switch-to-buffer-other-window "*uniline-expected*")
    (insert result)
    (goto-char (point-min))
    (compare-windows nil))

  uniline-bench-result)

(defun uniline-bench-create ()
  "Interactively create a bench.
An empty buffer is made available, with uniline mode active.
Draw a sketch.
When done, type $.
A Lisp buffer able to automatically re-run the drawing is presented.
Save it in a *.el file along with other benches."
  (interactive)
  (ignore-errors (kill-buffer "*uniline-interactive*"))
  (switch-to-buffer "*uniline-interactive*")
  (uniline-mode)
  (local-set-key "$" 'uniline-bench-collect)
  (message "draw a sketch, type $ whend done")
  (kmacro-start-macro nil))

(defun uniline-bench-collect ()
  "Called when typing $ to close the interactive drawing.
Do not call it directly."
  (interactive)
  (kmacro-end-macro 1)
  (ignore-errors (kill-buffer "b.el"))
  (switch-to-buffer "b.el")
  (insert "(uniline-bench\n\"")
  (insert (key-description (kmacro--keys (kmacro last-kbd-macro))))
  (insert "\"\n\"\\\n")
  (insert-buffer-substring "*uniline-interactive*")
  (insert "\")\n")
  (lisp-mode))

(defmacro uniline-bench-numcompact-n (n)
  "If N is 3, replace in the buffer <left> <left> <left> to 3*<left>
It is a macro, because N is a dynamic variable which must be
inserted into a regexp."
  `(save-excursion
     (replace-regexp
      (rx " "
          (group (+ (any "a-zA-Z<>-")))
          (= ,(1- n)
             (+ blank)
             (backref 1)))
      ,(format " %d*\\1" n))))

(defun uniline-bench-numcompact ()
  "Change <left> <left> <left> to 3*<left>.
The `kbd' function understand this notation.
And it is easier to read.
Put the cursor inside the string to modify."
  (interactive)
  (search-backward "\"")
  (narrow-to-region (point) (progn (forward-sexp) (point)))
  (goto-char (point-min))
  (uniline-bench-numcompact-n 27)
  (uniline-bench-numcompact-n 26)
  (uniline-bench-numcompact-n 25)
  (uniline-bench-numcompact-n 24)
  (uniline-bench-numcompact-n 23)
  (uniline-bench-numcompact-n 22)
  (uniline-bench-numcompact-n 21)
  (uniline-bench-numcompact-n 20)
  (uniline-bench-numcompact-n 19)
  (uniline-bench-numcompact-n 18)
  (uniline-bench-numcompact-n 17)
  (uniline-bench-numcompact-n 16)
  (uniline-bench-numcompact-n 15)
  (uniline-bench-numcompact-n 14)
  (uniline-bench-numcompact-n 13)
  (uniline-bench-numcompact-n 12)
  (uniline-bench-numcompact-n 11)
  (uniline-bench-numcompact-n 10)
  (uniline-bench-numcompact-n  9)
  (uniline-bench-numcompact-n  8)
  (uniline-bench-numcompact-n  7)
  (uniline-bench-numcompact-n  6)
  (uniline-bench-numcompact-n  5)
  (uniline-bench-numcompact-n  4)
  (uniline-bench-numcompact-n  3)
  (uniline-bench-numcompact-n  2)
  (widen))

(defun uniline-bench-run (&rest files)
  "Run all benches, or a specified list.
When FILES is nil, the benches are all files with *.el suffix.
Stops on the first error, presenting two buffers,
- one with the actual drawing,
- the other with the expected drawing,
with points on the first difference.
If there are no errors, a summary is presented."
  (interactive)
  (unless files
    (setq files
          (directory-files "." nil "^bench.*\\.el$")))
  (let* ((buf (current-buffer))
         (nbpassed 0)
         (nbfailed 0)
         (failed
          (cl-loop
           for file in files
           do
           (load (format "%s%s" default-directory file) nil nil t)
           (if uniline-bench-result
               (cl-incf nbpassed)
             (cl-incf nbfailed)
             (message "%s FAILED" file))
           unless uniline-bench-result
           collect file)))
    (switch-to-buffer buf)
    (message "%s PASSED / %s FAILED %s" nbpassed nbfailed failed)))

(pcase 0
  (0
   (uniline-bench-run))
  (1
   (garbage-collect)
   (profiler-start 'cpu+mem)
   (uniline-bench-run)
   (profiler-stop)
   (profiler-report))
  (2
   (garbage-collect)
   (elp-instrument-package "uniline")
   (uniline-bench-run)
   (elp-results)
   (elp-restore-all)))

(if nil
    (uniline-bench-run "bench26.el" "bench27.el"))

(provide 'uniline-bench)
;;; uniline-bench.el ends here
