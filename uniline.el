;;; uniline.el --- Add UNICODE based diagrams to text files -*- coding:utf-8; lexical-binding: t; -*-

;; Copyright (C) 2024  Thierry Banel

;; Author: Thierry Banel tbanelwebmin at free dot fr
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (hydra "0.15.0"))
;; Keywords: convenience, text
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
;;                ┏━━━━━━━┓
;;    ╭──────╮    ┃ thick ┣═◁═╗
;;    │ thin ┝◀━━━┫ box   ┃   ║
;;    │ box  │    ┗━━━━━━━┛   ║
;;    ╰───┬──╯         ╔══════╩═╗
;;        ↓            ║ double ║
;;        ╰────────────╢ box    ║
;;                     ╚════╤═══╝
;;      ▛▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▜   │
;;      ▌quadrant-blocks▐─◁─╯
;;      ▙▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▟
;;
;; UNICODE characters are available to draw nice boxes
;; and lines.
;; They come in 4 flavours: thin, thick, double, and quadrant-blocks.
;; Uniline makes it easy to draw and combine all 4 flavours.
;; Use the arrows on the keyboard to move around leaving a line behind.
;;
;; Uniline is a minor mode.  Enter it with:
;;   M-x uniline-mode
;; Leave it with:
;;   C-c C-c
;;
;; A font able to displays the needed UNICODE characters have to
;; be used.  It works well with the following families:
;; - DejaVu Sans Mono
;; - Unifont
;; - Hack
;; - JetBrains Mono
;; - Cascadia Mono
;; - Agave
;; - JuliaMono
;; - FreeMono
;; - Iosevka Comfy Fixed
;; - Source Code Pro
;;
;; Also, the encoding of the file must support UNICODE.
;; One way to do that, is to add a line like this one
;; at the top of your file:
;;   -*- coding:utf-8; -*-

;;; Requires:
(require 'cl-lib)
(require 'hydra)
(require 'rect)
(cl-proclaim '(optimize (speed 3) (safety 1)))

(eval-when-compile
  ;; temporarily fix a bug about Hydra generating too long docstrings
  (setq byte-compile-docstring-max-column 2000))

;;; Code:

;;;╭────────────────╮
;;;│Coding decisions│
;;;╰────────────────╯

;; Why so much use of (defmacro)?
;; Because we handle 4 directions: north, east, south, west.
;; We end up writing slightly the same algorithms 4 times.
;; It is easier to centralise the algorithms in a set
;; of (defmacro) to write them just once.

;; Isn't (defun) enough and more readable than (defmacro)?
;; Yes, absolutely!
;; We can centralize algorithms in a set of (defun).
;; However a (defun) generically handling all four directions
;; misses nice code-folding.
;; For instance somewhere we have:
;;   (ash 3 (* 2 dir))
;; If the compiler sees `dir' as `uniline--direction-dw↓',
;; for example, then it can fold this expression to just 48,
;; which is nice©

;; We call four times a (defmacro) with hard-coded directions.
;; The hard-coded parameter must go all the way down
;; to the last instructions needing that direction.
;; So we have (defmacro) calling other (defmacro).

;; What is the purpose `eval-when-compile'?
;; It folds down a numerical expression to just one number
;; for instance
;;   (eval-when-compile (ash 3 (* 2 uniline--direction-lf←)))
;; is converted to just 192
;; This happens both in interpreted and byte-compiled code
;; Otherwise the operations ash, multiplication,
;; and retrieval from `uniline--direction-lf←'
;; would be computed over and over at runtime,
;; with always the same 192 result.
;; We could put directly 192 in the source code,
;; but this would defeat maintenance and readability.

;; What is the purpose `eval-and-compile'?
;; The byte-compiler expands all defmacro' called in defun's.
;; In turn, those defmacro's need to access some defconst's,
;; notably `uniline--direction-up↑' and sisters.
;; So those defconst's are made available to the byte-compiler
;; as well as to the runtime, by embedding them in a
;; `eval-and-compile' declaration.

;; When a decision makes the byte-compiled code better
;; (faster to load and run), at the expense of a slower
;; interpreted counterpart, then go ahead, bias toward
;; byte-compiled code.

;;;╭────────────────────────────────────────────────────────╮
;;;│4 directions, infinite buffer in right & down directions│
;;;╰────────────────────────────────────────────────────────╯

(eval-and-compile
  (defconst uniline--direction-up↑ 0)
  (defconst uniline--direction-ri→ 1)
  (defconst uniline--direction-dw↓ 2)
  (defconst uniline--direction-lf← 3))

(eval-when-compile ; not needed at runtime
  (defun uniline--reverse-direction (dir)
    "Reverse DIR.
DIR is any of the 4 `uniline--direction-*'.
Exchange left with right, up with down."
    (pcase dir
      ('uniline--direction-up↑ 'uniline--direction-dw↓)
      ('uniline--direction-ri→ 'uniline--direction-lf←)
      ('uniline--direction-dw↓ 'uniline--direction-up↑)
      ('uniline--direction-lf← 'uniline--direction-ri→)
      (_ (error "Bad direction")))))

(defsubst uniline--turn-right (dir)
  "Return DIR turned 90° clockwise.
DIR & returned value are in [0,1,2,3]."
  (mod (1+ dir) 4))

(defsubst uniline--turn-left (dir)
  "Return DIR turned 90° anti-clockwise.
DIR & returned value are in [0,1,2,3]."
  (mod (1- dir) 4))

(defsubst uniline--move-to-column (x)
  "Move to column X staying on the same line.
Add blanks if line is too short.
Move to 0 if X negative."
  (move-to-column (max x 0) t))

(defun uniline--move-to-delta-column (x)
  "Move X characters, staying on the same line.
Add blanks if line is too short.
X may be negative to move backward.
Move to 0 if target is beyond the left border of buffer."
  (uniline--move-to-column (+ (current-column) x)))

(defsubst uniline--forward-line-force (y)
  "Helper function to move cursor Y lines.
Create lines at the end of the buffer if there
are not enough lines to honor Y.
Y may be negative.
Does not preserve current column."
  ;; here we fix a bug in the return of (forward-line):
  ;; when (forward-line) cannot move enough lines
  ;; because it is stuck at the end of buffer,
  ;; it erroneously returns one less missing forwards
  ;; but the bug does not appears if the end-of-buffer
  ;; is at the beginning-of-line
  ;; so here we get out of the corner-case of the
  ;; (forward-line) bug, by ensuring that there is an empty
  ;; line at the end of buffer
  (goto-char
   (prog1 (point)
     (goto-char (point-max))
     (or (bolp) (insert ?\n))))
  (let ((y (forward-line y))) ; faster than (setq y (forward-line y))
    (while (>= (setq y (1- y)) 0)
      (insert ?\n))))

(defun uniline--move-to-line (y)
  "Move to line Y, while staying on the same column.
Create blank lines at the end of the buffer if needed,
or blank characters at the end of target line.
Y=0 means first line in buffer."
  (move-to-column
   (prog1
       (current-column)
     (goto-char (point-min))
     (uniline--forward-line-force y))
   t))

(defun uniline--move-to-delta-line (y)
  "Move Y lines while staying on the same column.
Create blank lines at the end of the buffer if needed,
or blank characters at the end of target line.
Y may be negative to move backward."
  (move-to-column
   (prog1
       (current-column)
     (uniline--forward-line-force y))
   t))

(defun uniline--move-to-lin-col (y x)
  "Move to line Y and column X.
Create blank lines at the end of the buffer if needed,
or blank characters at the end of target line if needed.
Y=0 means first line of buffer.
X=0 means first column of buffer."
  (goto-char (point-min))
  (uniline--forward-line-force y)
  (uniline--move-to-column x))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--move-in-direction (dir &optional nb)
    "Move NB char in direction DIR.
NB defaults to 1.
This is a macro, therefore it is as if writing
directly (uniline--move-to-delta-line -1) and such,
with no overhead."
    (declare (debug (form)))
    (unless nb (setq nb 1))
    (pcase dir
      ('uniline--direction-up↑ `(uniline--move-to-delta-line   (- ,nb)))
      ('uniline--direction-ri→ `(uniline--move-to-delta-column    ,nb) )
      ('uniline--direction-dw↓ `(uniline--move-to-delta-line      ,nb) )
      ('uniline--direction-lf← `(uniline--move-to-delta-column (- ,nb)))
      (_ (error "Bad direction")))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--at-border-p (dir)
    "Test if at a non-trespass-able border of buffer.
This happens at the first line or at the first column,
when trying to go further when DIR is up or left:
`uniline--direction-up↑' or `uniline--direction-lf←'.
In the bottom & right directions the buffer is infinite."
    (declare (debug (form)))
    (pcase dir
      ('uniline--direction-up↑ '(eq (pos-bol) 1))
      ('uniline--direction-ri→ 'nil)
      ('uniline--direction-dw↓ 'nil)
      ('uniline--direction-lf← '(bolp))
      (_ (error "Bad direction")))))


(eval-when-compile ; not needed at runtime
  (defmacro uniline--neighbour-point (dir)
    "Return the (point) one char away from current (point) in DIR direction.
Return nil if no such point exists because it would fall
outside the buffer.
The buffer is not modified."
    (pcase dir
      ('uniline--direction-ri→ '(unless (eolp) (1+ (point))))
      ('uniline--direction-lf← '(unless (bolp) (1- (point))))
      ('uniline--direction-up↑
       '(save-excursion
          (let ((p (current-column)))
            (and (eq (forward-line -1) 0)
                 (eq (move-to-column p) p)
                 (point)))))
      ('uniline--direction-dw↓ '
       (save-excursion
         (let ((p (current-column)))
           (and (eq (forward-line 1) 0)
                (eq (move-to-column p) p)
                (point)))))
      (_ (error "Bad direction")))))

(defun uniline--char-after ()
  "Same as `char-after', except for right and bottom edges of buffer.
Outside the buffer, return a blank character."
  (let ((c (char-after)))
    (if (or (not c) (eq c ?\n))
        ?  ;; eol & eob are considered blank
      c)))

;;;╭─────────────────────────────────────────────────────╮
;;;│Reference tables of ┼ 4 half-lines UNICODE characters│
;;;╰─────────────────────────────────────────────────────╯

;; Hereafter `4halfs' means a representation of a UNICODE character
;; made of half-lines, like ┖ or ┶, as a single number.
;; There are 4 half-lines, one in each direction
;; (north, east, west, south).  Each half line may have one of
;; 4 styles: blank ( ), thin (─), thick (━), double (═).
;; So a `4halfs' number goes from 0 to 4x4x4x4 = 256.
;; This representation is great because it is easily handled by
;; the bits manipulation primitives: `logior', `logand', `ash'.

(eval-when-compile ; not needed at runtime
  (defun uniline--pack-4halfs (urld)
    "Encode a description of lines into a single number.
A character contains half lines upward, right, downward,
left.
Each of those half lines may have one of 4 styles:
  0: no line
  1: thin line
  2: thick line
  3: double line
Example: ╀ has description 2 1 1 1
thick=2 upward, and thin=1 in the other direction
The parameter URLD is a list of 4 numbers in [0..3]
for the 4 directions.
A single number encoding all possible combinations has a
range of [0..256).  It is handy to index vectors rather than
4 dimensions matrices."
    (logior
     (ash (car    urld) (eval-when-compile (* 2 uniline--direction-up↑)))
     (ash (cadr   urld) (eval-when-compile (* 2 uniline--direction-ri→)))
     (ash (caddr  urld) (eval-when-compile (* 2 uniline--direction-dw↓)))
     (ash (cadddr urld) (eval-when-compile (* 2 uniline--direction-lf←))))))

(eval-when-compile ; not used at runtime
  (defconst uniline--list-of-available-halflines
    '(;;╭──unicode char
      ;;│  ╭───4half description
      ;;▽  ▽
      ( ?  0 0 0 0 )
      ( ?╵ 1 0 0 0 )
      ( ?╹ 2 0 0 0 )
      ( ?╶ 0 1 0 0 )
      ( ?└ 1 1 0 0 )
      ( ?╰ 1 1 0 0 ) ;; prefer rounded corner
      ( ?┖ 2 1 0 0 )
      ( ?╺ 0 2 0 0 )
      ( ?┕ 1 2 0 0 )
      ( ?┗ 2 2 0 0 )
      ( ?╷ 0 0 1 0 )
      ;;( ?| 1 0 1 0 ) ;; recognize vertical ASCII pipe
      ( ?│ 1 0 1 0 )
      ( ?┃ 2 0 2 0 )
      ( ?╿ 2 0 1 0 )
      ( ?┌ 0 1 1 0 )
      ( ?╭ 0 1 1 0 ) ;; prefer rounded corner
      ( ?├ 1 1 1 0 )
      ( ?┞ 2 1 1 0 )
      ( ?┍ 0 2 1 0 )
      ( ?┝ 1 2 1 0 )
      ( ?┡ 2 2 1 0 )
      ( ?╻ 0 0 2 0 )
      ( ?╽ 1 0 2 0 )
      ( ?┎ 0 1 2 0 )
      ( ?┟ 1 1 2 0 )
      ( ?┠ 2 1 2 0 )
      ( ?┏ 0 2 2 0 )
      ( ?┢ 1 2 2 0 )
      ( ?┣ 2 2 2 0 )
      ( ?╴ 0 0 0 1 )
      ( ?┘ 1 0 0 1 )
      ( ?╯ 1 0 0 1 ) ;; prefer rounded corner
      ( ?┚ 2 0 0 1 )
      ;;( ?- 0 1 0 1 ) ;; recognize ASCII minus
      ( ?─ 0 1 0 1 )
      ( ?┴ 1 1 0 1 )
      ( ?┸ 2 1 0 1 )
      ( ?╼ 0 2 0 1 )
      ( ?┶ 1 2 0 1 )
      ( ?┺ 2 2 0 1 )
      ( ?┐ 0 0 1 1 )
      ( ?╮ 0 0 1 1 ) ;; prefer rounded corner
      ( ?┤ 1 0 1 1 )
      ( ?┦ 2 0 1 1 )
      ( ?┬ 0 1 1 1 )
      ;;( ?+ 1 1 1 1 ) ;; recognize ASCII plus
      ( ?┼ 1 1 1 1 )
      ( ?╀ 2 1 1 1 )
      ( ?┮ 0 2 1 1 )
      ( ?┾ 1 2 1 1 )
      ( ?╄ 2 2 1 1 )
      ( ?┒ 0 0 2 1 )
      ( ?┧ 1 0 2 1 )
      ( ?┨ 2 0 2 1 )
      ( ?┰ 0 1 2 1 )
      ( ?╁ 1 1 2 1 )
      ( ?╂ 2 1 2 1 )
      ( ?┲ 0 2 2 1 )
      ( ?╆ 1 2 2 1 )
      ( ?╊ 2 2 2 1 )
      ( ?╸ 0 0 0 2 )
      ( ?┙ 1 0 0 2 )
      ( ?┛ 2 0 0 2 )
      ( ?╾ 0 1 0 2 )
      ( ?┵ 1 1 0 2 )
      ( ?┹ 2 1 0 2 )
      ( ?━ 0 2 0 2 )
      ( ?┷ 1 2 0 2 )
      ( ?┻ 2 2 0 2 )
      ( ?┑ 0 0 1 2 )
      ( ?┥ 1 0 1 2 )
      ( ?┩ 2 0 1 2 )
      ( ?┭ 0 1 1 2 )
      ( ?┽ 1 1 1 2 )
      ( ?╃ 2 1 1 2 )
      ( ?┯ 0 2 1 2 )
      ( ?┿ 1 2 1 2 )
      ( ?╇ 2 2 1 2 )
      ( ?┓ 0 0 2 2 )
      ( ?┪ 1 0 2 2 )
      ( ?┫ 2 0 2 2 )
      ( ?┱ 0 1 2 2 )
      ( ?╅ 1 1 2 2 )
      ( ?╉ 2 1 2 2 )
      ( ?┳ 0 2 2 2 )
      ( ?╈ 1 2 2 2 )
      ( ?╋ 2 2 2 2 )

      ( ?╒ 0 3 1 0 )
      ( ?╞ 1 3 1 0 )
      ( ?║ 3 0 3 0 )
      ( ?╓ 0 1 3 0 )
      ( ?╟ 3 1 3 0 )
      ( ?╔ 0 3 3 0 )
      ( ?╠ 3 3 3 0 )
      ( ?╜ 3 0 0 1 )
      ( ?╨ 3 1 0 1 )
      ( ?╖ 0 0 3 1 )
      ( ?╢ 3 0 3 1 )
      ( ?╥ 0 1 3 1 )
      ( ?╫ 3 1 3 1 )
      ( ?╛ 1 0 0 3 )
      ( ?╝ 3 0 0 3 )
      ( ?═ 0 3 0 3 )
      ( ?╧ 1 3 0 3 )
      ( ?╩ 3 3 0 3 )
      ( ?╕ 0 0 1 3 )
      ( ?╡ 1 0 1 3 )
      ( ?╤ 0 3 1 3 )
      ( ?╪ 1 3 1 3 )
      ( ?╗ 0 0 3 3 )
      ( ?╣ 3 0 3 3 )
      ( ?╦ 0 3 3 3 )
      ( ?╬ 3 3 3 3 )
      ( ?╙ 3 1 0 0 )
      ( ?╘ 1 3 0 0 )
      ( ?╚ 3 3 0 0 ))))

(eval-when-compile ; not used at runtime
  (defconst uniline--list-of-double-halflines
    '(;;   ╭─missing ╭───replacement
      ;;   │         │         │
      ;;   ▽         ▽         ▽
      ((0 0 0 3) (0 0 0 2)) ;; ╸
      ((0 0 2 3) (0 0 3 3)) ;; ╗
      ((0 0 3 0) (0 0 2 0)) ;; ╻
      ((0 0 3 2) (0 0 3 3)) ;; ╗
      ((0 1 0 3) (0 3 0 3)) ;; ═
      ((0 1 1 3) (0 3 1 3)) ;; ╤
      ((0 1 2 3) (0 1 2 2)) ;; ┱
      ((0 1 3 2) (0 1 2 2)) ;; ┱
      ((0 1 3 3) (0 3 3 3)) ;; ╦
      ((0 2 0 3) (0 3 0 3)) ;; ═
      ((0 2 1 3) (0 3 1 3)) ;; ╤
      ((0 2 2 3) (0 3 3 3)) ;; ╦
      ((0 2 3 0) (0 3 3 0)) ;; ╔
      ((0 2 3 1) (0 1 3 1)) ;; ╥
      ((0 2 3 2) (0 3 3 3)) ;; ╦
      ((0 2 3 3) (0 3 3 3)) ;; ╦
      ((0 3 0 0) (0 2 0 0)) ;; ╺
      ((0 3 0 1) (0 3 0 3)) ;; ═
      ((0 3 0 2) (0 3 0 3)) ;; ═
      ((0 3 1 1) (0 3 1 3)) ;; ╤
      ((0 3 1 2) (0 3 1 3)) ;; ╤
      ((0 3 2 0) (0 3 3 0)) ;; ╔
      ((0 3 2 1) (0 2 2 1)) ;; ┲
      ((0 3 2 2) (0 3 3 3)) ;; ╦
      ((0 3 2 3) (0 3 3 3)) ;; ╦
      ((0 3 3 1) (0 3 3 3)) ;; ╦
      ((0 3 3 2) (0 3 3 3)) ;; ╦
      ((1 0 2 3) (1 0 1 3)) ;; ╡
      ((1 0 3 0) (3 0 3 0)) ;; ║
      ((1 0 3 1) (3 0 3 1)) ;; ╢
      ((1 0 3 2) (1 0 2 2)) ;; ┪
      ((1 0 3 3) (3 0 3 3)) ;; ╣
      ((1 1 0 3) (1 3 0 3)) ;; ╧
      ((1 1 1 3) (1 3 1 3)) ;; ╪
      ((1 1 2 3) (1 1 2 2)) ;; ╅
      ((1 1 3 0) (3 1 3 0)) ;; ╟
      ((1 1 3 1) (3 1 3 1)) ;; ╫
      ((1 1 3 2) (1 1 2 2)) ;; ╅
      ((1 1 3 3) (3 3 3 3)) ;; ╬
      ((1 2 0 3) (1 3 0 3)) ;; ╧
      ((1 2 1 3) (1 3 1 3)) ;; ╪
      ((1 2 2 3) (1 2 2 2)) ;; ╈
      ((1 2 3 0) (1 2 2 0)) ;; ┢
      ((1 2 3 1) (1 2 2 1)) ;; ╆
      ((1 2 3 2) (1 2 2 2)) ;; ╈
      ((1 2 3 3) (3 3 3 3)) ;; ╬
      ((1 3 0 1) (1 3 0 3)) ;; ╧
      ((1 3 0 2) (1 3 0 3)) ;; ╧
      ((1 3 1 1) (1 3 1 3)) ;; ╪
      ((1 3 1 2) (1 3 1 3)) ;; ╪
      ((1 3 2 0) (1 2 2 0)) ;; ┢
      ((1 3 2 1) (1 2 2 1)) ;; ╆
      ((1 3 2 2) (1 2 2 2)) ;; ╈
      ((1 3 2 3) (1 3 1 3)) ;; ╪
      ((1 3 3 0) (3 3 3 0)) ;; ╠
      ((1 3 3 1) (3 3 3 3)) ;; ╬
      ((1 3 3 2) (3 3 3 3)) ;; ╬
      ((1 3 3 3) (3 3 3 3)) ;; ╬
      ((2 0 0 3) (3 0 0 3)) ;; ╝
      ((2 0 1 3) (1 0 1 3)) ;; ╡
      ((2 0 2 3) (3 0 3 3)) ;; ╣
      ((2 0 3 0) (3 0 3 0)) ;; ║
      ((2 0 3 1) (3 0 3 1)) ;; ╢
      ((2 0 3 2) (3 0 3 3)) ;; ╣
      ((2 0 3 3) (3 0 3 3)) ;; ╣
      ((2 1 0 3) (2 1 0 2)) ;; ┹
      ((2 1 1 3) (2 1 1 2)) ;; ╃
      ((2 1 2 3) (2 1 2 2)) ;; ╉
      ((2 1 3 0) (3 1 3 0)) ;; ╟
      ((2 1 3 1) (3 1 3 1)) ;; ╫
      ((2 1 3 2) (2 1 2 2)) ;; ╉
      ((2 1 3 3) (3 3 3 3)) ;; ╬
      ((2 2 0 3) (3 3 0 3)) ;; ╩
      ((2 2 1 3) (2 2 1 2)) ;; ╇
      ((2 2 2 3) (2 2 2 2)) ;; ╋
      ((2 2 3 0) (3 3 3 0)) ;; ╠
      ((2 2 3 1) (2 2 2 1)) ;; ╊
      ((2 2 3 2) (2 2 2 2)) ;; ╋
      ((2 2 3 3) (3 3 3 3)) ;; ╬
      ((2 3 0 0) (3 3 0 0)) ;; ╚
      ((2 3 0 1) (2 2 0 1)) ;; ┺
      ((2 3 0 2) (3 3 0 3)) ;; ╩
      ((2 3 0 3) (3 3 0 3)) ;; ╩
      ((2 3 1 0) (1 3 1 0)) ;; ╞
      ((2 3 1 1) (2 2 1 1)) ;; ╄
      ((2 3 1 2) (2 2 1 2)) ;; ╇
      ((2 3 1 3) (1 3 1 3)) ;; ╪
      ((2 3 2 0) (3 3 3 0)) ;; ╠
      ((2 3 2 1) (2 2 2 1)) ;; ╊
      ((2 3 2 2) (2 2 2 2)) ;; ╋
      ((2 3 2 3) (3 3 3 3)) ;; ╬
      ((2 3 3 0) (3 3 3 0)) ;; ╠
      ((2 3 3 1) (3 3 3 3)) ;; ╬
      ((2 3 3 2) (3 3 3 3)) ;; ╬
      ((2 3 3 3) (3 3 3 3)) ;; ╬
      ((3 0 0 0) (2 0 0 0)) ;; ╹
      ((3 0 0 2) (3 0 0 3)) ;; ╝
      ((3 0 1 0) (3 0 3 0)) ;; ║
      ((3 0 1 1) (3 0 3 1)) ;; ╢
      ((3 0 1 2) (2 0 1 2)) ;; ┩
      ((3 0 1 3) (3 0 3 3)) ;; ╣
      ((3 0 2 0) (3 0 3 0)) ;; ║
      ((3 0 2 1) (3 0 3 1)) ;; ╢
      ((3 0 2 2) (3 0 3 3)) ;; ╣
      ((3 0 2 3) (3 0 3 3)) ;; ╣
      ((3 0 3 2) (3 0 3 3)) ;; ╣
      ((3 1 0 2) (2 1 0 2)) ;; ┹
      ((3 1 0 3) (3 3 0 3)) ;; ╩
      ((3 1 1 0) (3 1 3 0)) ;; ╟
      ((3 1 1 1) (3 1 3 1)) ;; ╫
      ((3 1 1 2) (2 1 1 2)) ;; ╃
      ((3 1 1 3) (3 3 3 3)) ;; ╬
      ((3 1 2 0) (3 1 3 0)) ;; ╟
      ((3 1 2 1) (3 1 3 1)) ;; ╫
      ((3 1 2 2) (2 1 2 2)) ;; ╉
      ((3 1 2 3) (3 3 3 3)) ;; ╬
      ((3 1 3 2) (3 1 3 1)) ;; ╫
      ((3 1 3 3) (3 3 3 3)) ;; ╬
      ((3 2 0 0) (3 3 0 0)) ;; ╚
      ((3 2 0 1) (2 2 0 1)) ;; ┺
      ((3 2 0 2) (3 3 0 3)) ;; ╩
      ((3 2 0 3) (3 3 0 3)) ;; ╩
      ((3 2 1 0) (2 2 1 0)) ;; ┡
      ((3 2 1 1) (2 2 1 1)) ;; ╄
      ((3 2 1 2) (2 2 1 2)) ;; ╇
      ((3 2 1 3) (3 3 3 3)) ;; ╬
      ((3 2 2 0) (3 3 3 0)) ;; ╠
      ((3 2 2 1) (2 2 2 1)) ;; ╊
      ((3 2 2 2) (2 2 2 2)) ;; ╋
      ((3 2 2 3) (3 3 3 3)) ;; ╬
      ((3 2 3 0) (3 3 3 0)) ;; ╠
      ((3 2 3 1) (3 1 3 1)) ;; ╫
      ((3 2 3 2) (3 3 3 3)) ;; ╬
      ((3 2 3 3) (3 3 3 3)) ;; ╬
      ((3 3 0 1) (3 3 0 3)) ;; ╩
      ((3 3 0 2) (3 3 0 3)) ;; ╩
      ((3 3 1 0) (3 3 3 0)) ;; ╠
      ((3 3 1 1) (3 3 3 3)) ;; ╬
      ((3 3 1 2) (3 3 3 3)) ;; ╬
      ((3 3 1 3) (3 3 3 3)) ;; ╬
      ((3 3 2 0) (3 3 3 0)) ;; ╠
      ((3 3 2 1) (3 3 3 3)) ;; ╬
      ((3 3 2 2) (3 3 3 3)) ;; ╬
      ((3 3 2 3) (3 3 3 3)) ;; ╬
      ((3 3 3 1) (3 3 3 3)) ;; ╬
      ((3 3 3 2) (3 3 3 3)) ;; ╬
      )))

(defconst uniline--4halfs-to-char
  (eval-when-compile
    (let ((table (make-vector (* 4 4 4 4) nil)))
      (cl-loop
       for x in uniline--list-of-available-halflines
       do
       (aset table
             (uniline--pack-4halfs (cdr x))
             (car x)))
      (cl-loop
       for x in uniline--list-of-double-halflines
       do
       (aset table
             (uniline--pack-4halfs (car x))
             (aref table
                   (uniline--pack-4halfs (cadr x)))))
      table))
  "Convert a 4halfs description to a UNICODE character.
The 4halfs description is (UP RI DW LF)
packed into a single integer.
As there are no UNICODE character for every combination,
the visually closest UNICODE character is retrieved.
So for instance
  up=1 (thin up),
  ri=2 (thick right),
  dw=0 (blank down),
  lf=0 (blank left),
is encoded into up +4*ri +16*dw +64*lf = 9,
which in turn is converted to ┕.")

(defconst uniline--char-to-4halfs
  (eval-when-compile
    (let ((table (make-char-table 'uniline--char-to-4halfs)))
      (cl-loop
       for x in uniline--list-of-available-halflines
       do
       (aset table
             (car x)
             (uniline--pack-4halfs (cdr x))))
      table))
  "Convert a UNICODE character to a 4halfs description.
The UNICODE character is supposed to represent
a combination of half lines in 4 directions
and in 4 brush styles.
The retrieved value is a4halfs description is (UP RI DW LF)
packed into a single integer.
If the UNICODE character is not a box-drawing one, nil
is returned.
So for instance, the character ┸ is converted to (2 1 0 1)
meaning:
  2 = thick up
  1 = thin right
  0 = blank down
  1 = thin left
Values (2 1 0 1) are encoded into 2 + 4*1 + 0*16 + 1*64 = 70
This table is the reverse of `uniline--4halfs-to-char'
without the fall-back characters.")

(when nil

  ;; This inactive code was used to generate the
  ;;   `uniline--list-of-double-halflines' list above.

  ;; As the ╬ double line glyphs combinations were not all defined
  ;; in the UNICODE standard, a penalty system was applied to look
  ;; for the closest possible alternate glyph.

  ;; For example, ├ and ╠ exist, but a mixture of both do no exit,
  ;; hence a line like this one
  ;;   ((3 3 1 0)  (3 3 3 0)) ;; ╠
  ;; which says that, when this ├ is needed, with the upward and
  ;; rightward branches in double line style, then fallback to ╠

  ;; There is no need to re-run it, except if one wants to change
  ;; the penalties applied.
  ;;
  ;; To run it re-compute `uniline--4halfs-to-char'
  ;; - but with only the first cl-loop over
  ;;   `uniline--list-of-available-halflines'
  ;; - not the loop over
  ;;   `uniline--list-of-double-halflines'

  (defun uniline--penalty1 (a b)
    (pcase a
      (0 (if (eq b 0) 0 1000))
      (1 (pcase b
           (0 1000)
           (1 0)
           (2 3)
           (3 2)))
      (2 (pcase b
           (0 1000)
           (1 3)
           (2 0)
           (3 1)))
      (3 (pcase b
           (0 1000)
           (1 4)
           (2 3)
           (3 0)))))

  (defun uniline--penalty (u1 r1 d1 l1 u2 r2 d2 l2)
    (+
     (uniline--penalty1 u1 u2)
     (uniline--penalty1 r1 r2)
     (uniline--penalty1 d1 d2)
     (uniline--penalty1 l1 l2)))

  (switch-to-buffer "replace.el")
  (dotimes (u1 4)
    (dotimes (r1 4)
      (dotimes (d1 4)
        (dotimes (l1 4)
          (unless (aref uniline--4halfs-to-char
                        (uniline--pack-4halfs (list u1 r1 d1 l1)))
            (let ((m 9999)
                  m0
                  u3 r3 d3 l3)
              (dotimes (u2 4)
                (dotimes (r2 4)
                  (dotimes (d2 4)
                    (dotimes (l2 4)
                      (when (aref uniline--4halfs-to-char
                                  (uniline--pack-4halfs (list u2 r2 d2 l2)))
                        (setq m0 (uniline--penalty u1 r1 d1 l1 u2 r2 d2 l2))
                        (if (< m0 m)
                            (setq m m0
                                  u3 u2
                                  r3 r2
                                  d3 d2
                                  l3 l2)))))))
              (insert
               (format "((%d %d %d %d) (%d %d %d %d)) ;; %c\n"
                       u1 r1 d1 l1
                       u3 r3 d3 l3
                       (aref uniline--4halfs-to-char
                             (uniline--pack-4halfs (list u3 r3 d3 l3))))))))))))

;;;╭────────────────────────────────────────────────────────╮
;;;│Reference tables of △▶↓□◆● arrows & other UNICODE glyphs│
;;;╰────────────────────────────────────────────────────────╯

(eval-when-compile ; helper constant not needed at runtime
  (defconst uniline--glyphs-tmp
    '(
      ;; arrows
      (a   ?△ ?▷ ?▽ ?◁)      ;; white *-pointing triangle
      (a   ?▲ ?▶ ?▼ ?◀)      ;; black *-pointing triangle
      (a   ?↑ ?→ ?↓ ?←)      ;; *wards arrow
      (a   ?▵ ?▹ ?▿ ?◃)      ;; white *-pointing small triangle"
      (a   ?▴ ?▸ ?▾ ?◂)      ;; black *-pointing small triangle
      (a   ?↕ ?↔ ?↕ ?↔)      ;; up down arrow, left right arrow

      ;; Those commented-out arrows are monospaces and supported
      ;; by the 6 fonts.  But they do not have 4 directions.
      ;;(a   ?‹ ?› ?› ?‹)      ;; single *-pointing angle quotation mark

      ;; squares
      (s   ?□)      ;; white square
      (s   ?■)      ;; black square
      (s   ?▫)      ;; white small square
      (s   ?▪)      ;; black small square
      (s   ?◇)      ;; white diamond
      (s   ?◆)      ;; black diamond
      (s   ?◊)      ;; lozenge

      ;; o shapes
      (o   ?·)      ;; middle dot
      (o   ?∙)      ;; bullet operator
      (o   ?•)      ;; bullet
      (o   ?●)      ;; black circle
      (o   ?◦)      ;; white bullet
      (o   ?Ø)      ;; latin capital letter o with stroke
      (o   ?ø)      ;; latin small letter o with stroke

      ;; crosses
      (x   ?╳)      ;; box drawings light diagonal cross
      (x   ?÷)      ;; division sign
      (x   ?×)      ;; multiplication sign
      (x   ?±)      ;; plus-minus sign
      (x   ?¤)      ;; currency sign

      ;; The following commented-out glyphs are possible additions
      ;; when using the DejaVu Sans Mono font
      ;; Other fonts either do not support those glyphs
      ;; or do not make them monospaced

      ;;(t   ?⋏ ?≻ ?⋎ ?≺) ;; precedes
      ;;(t   ?⊥ ?⊢ ?⊤ ?⊣) ;; * tack
      ;;(t   ?⋂ ?⊃ ?⋃ ?⊂) ;; subset of

      ;;(a   ?⇡ ?⇢ ?⇣ ?⇠) ;; *wards dashed arrow
      ;;(a   ?⇑ ?⇒ ?⇓ ?⇐) ;; *wards double arrow
      ;;(a   ?⇧ ?⇨ ?⇩ ?⇦) ;; *wards white arrow

      ;;(b   ?↥ ?↦ ?↧ ?↤) ;; *wards arrow from bar
      ;;(b   ?↟ ?↠ ?↡ ?↞) ;; *wards two headed arrow
      ;;(b   ?⇈ ?⇉ ?⇊ ?⇇) ;; *wards paired arrows
      ;;(b   ?☝ ?☞ ?☟ ?☜) ;; white * pointing index
      ;;(b   ?⇞ ?⇻ ?⇟ ?⇺) ;; *wards arrow with double vertical stroke

      ;;(c   ?⬘ ?⬗ ?⬙ ?⬖) ;; diamond with * half black
      ;;(c   ?◓ ?◑ ?◒ ?◐) ;; circle with * half black
      ;;(b   ?⍐ ?⍈ ?⍗ ?⍇) ;; apl functional symbol quad *wards arrow

      ;;(s   ?▢) ;; white square with rounded corners
      ;;(s   ?▣) ;; white square containing black small square
      ;;(s   ?▩) ;; square with diagonal crosshatch fill
      ;;(s   ?▤) ;; square with horizontal fill
      ;;(s   ?▥) ;; square with vertical fill
      ;;(s   ?▦) ;; square with orthogonal crosshatch fill
      ;;(s   ?▧) ;; square with upper left to lower right fill
      ;;(s   ?▨) ;; square with upper right to lower left fill

      ;;(o   ?○) ;; white circle
      ;;(o   ?◎) ;; bullseye
      ;;(o   ?✪) ;; circled white star
      ;;(o   ?o) ;; latin small letter o
      ;;(o   ?O) ;; latin capital letter o
      ;;(o   ?◍) ;; circle with vertical fill
      ;;(o   ?◉) ;; fisheye
      ;;(o   ?❂) ;; circled open centre eight pointed star
      ;;(o   ?⚙) ;; gear
      ;;(o   ?☹) ;; white frowning face
      ;;(o   ?☺) ;; white smiling face
      ;;(o   ?✆) ;; telephone location sign
      ;;(o   ?✇) ;; tape drive

      ;;(x   ?☓) ;; saltire
      ;;(x   ?+) ;; plus sign
      ;;(x   ?✔) ;; heavy check mark
      ;;(x   ?✖) ;; heavy multiplication x
      ;;(x   ?✚) ;; heavy greek cross
      ;;(x   ?✜) ;; heavy open centre cross
      ;;(x   ?x) ;; latin small letter x
      ;;(x   ?X) ;; latin capital letter x
      ;;(x   ?✙) ;; outlined greek cross
      ;;(x   ?✛) ;; open centre cross

      ;;(d   ?⚀) ;; die face-1
      ;;(d   ?⚁) ;; die face-2
      ;;(d   ?⚂) ;; die face-3
      ;;(d   ?⚃) ;; die face-4
      ;;(d   ?⚄) ;; die face-5
      ;;(d   ?⚅) ;; die face-6
      ;;(d   ?⊡) ;; squared dot operator
      ;;(d   ?☒) ;; ballot box with x
      ;;(d   ?☑) ;; ballot box with check
      ;;(d   ?⊞) ;; squared plus

      ;;(l   ?◇) ;; white diamond
      ;;(l   ?◆) ;; black diamond
      ;;(l   ?✦) ;; black four pointed star
      ;;(l   ?✧) ;; white four pointed star
      ;;(l   ?◈) ;; white diamond containing black small diamond
      ;;(l   ?♠) ;; black spade suit
      ;;(l   ?♥) ;; black heart suit
      ;;(l   ?♦) ;; black diamond suit
      ;;(l   ?♣) ;; black club suit
      ;;(l   ?♤) ;; white spade suit
      ;;(l   ?♡) ;; white heart suit
      ;;(l   ?♢) ;; white diamond suit
      ;;(l   ?♧) ;; white club suit

      ;;(f   ?✿) ;; black florette
      ;;(f   ?❀) ;; white florette
      ;;(f   ?❄) ;; snowflake
      ;;(f   ?✾) ;; six petalled black and white florette
      ;;(f   ?❁) ;; eight petalled outlined black florette
      ;;(f   ?❅) ;; tight trifoliate snowflake
      ;;(f   ?❆) ;; heavy chevron snowflake

      ;;(w   ?☼) ;; white sun with rays
      ;;(w   ?☀) ;; black sun with rays
      ;;(w   ?✫) ;; open centre black star
      ;;(w   ?✭) ;; outlined black star
      ;;(w   ?✩) ;; stress outlined white star
      ;;(w   ?✪) ;; circled white star
      ;;(w   ?✬) ;; black centre white star
      ;;(w   ?✮) ;; heavy outlined black star
      ;;(w   ?✯) ;; pinwheel star
      ;;(w   ?✱) ;; heavy asterisk
      ;;(w   ?✲) ;; open centre asterisk
      ;;(w   ?✳) ;; eight spoked asterisk
      ;;(w   ?✴) ;; eight pointed black star
      ;;(w   ?✵) ;; eight pointed pinwheel star
      ;;(w   ?✶) ;; six pointed black star
      ;;(w   ?✷) ;; eight pointed rectilinear black star
      ;;(w   ?✸) ;; heavy eight pointed rectilinear black star
      ;;(w   ?✹) ;; twelve pointed black star
      ;;(w   ?❂) ;; circled open centre eight pointed star

      ;;(k   ?✻) ;; teardrop-spoked asterisk
      ;;(k   ?✽) ;; heavy teardrop-spoked asterisk
      ;;(k   ?❉) ;; balloon-spoked asterisk
      ;;(k   ?❊) ;; eight teardrop-spoked propeller asterisk
      ;;(k   ?✺) ;; sixteen pointed asterisk
      ;;(k   ?✼) ;; open centre teardrop-spoked asterisk
      ;;(k   ?❃) ;; heavy teardrop-spoked pinwheel asterisk
      ;;(k   ?❋) ;; heavy eight teardrop-spoked propeller asterisk
      ;;(k   ?❇) ;; sparkle
      ;;(k   ?❈) ;; heavy sparkle
      ))
  "List of good looking UNICODE glyphs.
Those are:
- arrows in 4 directions,
- lists of symmetric-in-4-directions characters.
There are hundred of UNICODEs, but most of them are not
fixed-width or height, even in mono-spaced fonts.
Here we selected only the fixed-size ones.")

(eval-and-compile
  (defconst uniline--glyphs-bw
    (eval-when-compile
      (let ((r (reverse uniline--glyphs-tmp)))
        ;; nconc is used to create a circular list on purpose
        (nconc r r)))
    "List of good looking UNICODE glyphs.
Those are:
- arrows in 4 directions,
- lists of symmetric-in-4-directions characters.
There are hundred of UNICODEs, but most of them are not
fixed-width or height, even in mono-spaced fonts.
Here we selected only the fixed-size ones.
This list is ciurcular in backward order."))

(eval-and-compile
  (defconst uniline--glyphs-fw
    (eval-when-compile
      ;; nconc is used to create a circular list on purpose
      (nconc uniline--glyphs-tmp uniline--glyphs-tmp))
    "List of good looking UNICODE glyphs.
Those are:
- arrows in 4 directions,
- lists of symmetric-in-4-directions characters.
There are hundred of UNICODEs, but most of them are not
fixed-width or height, even in mono-spaced fonts.
Here we selected only the fixed-size ones.
This list is ciurcular in forward order."))

(eval-when-compile                      ; not needed at runtime

  (defun uniline--duplicate (list)
    "Return not-nil if LIST is duplicate-free.
Using `eq'."
    (while
        (and
         (cdr list)
         (not (memq (car list) (cdr list))))
      (pop list))
    (cdr list))

  (defun uniline--make-hash (list)
    "Helper function to build `uniline--glyphs-reverse-hash-*'.
Used only at package initialization.
LIST is `uniline--glyphs-fbw'."
    (let ((hh (make-hash-table)))
      (cl-loop
       for ll on list
       do
       (if (cddar ll)
           ;; glyph is directional, like ▲ ▶ ▼ ◀
           (cl-loop
            for cc in (cdar ll)
            for i from 0
            do (puthash
                cc
                (cons
                 (if (uniline--duplicate (car ll))
                     t    ; special case ↕↔↕↔ is NOT fully directional
                   i)     ; fully directional, i gives the direction
                 ll)
                hh))
         ;; glyph is not directional, like ■ ● ◆
         (puthash (cadar ll) (cons nil ll) hh))
       ;; explicitly break out of circular list
       if (eq (cdr ll) list)
       return nil)
      hh)))

(defconst uniline--glyphs-reverse-hash-fw
  (eval-when-compile
    (uniline--make-hash uniline--glyphs-fw))
  "Same as `uniline--glyphs-fw' reversing keys & values.")

(defconst uniline--glyphs-reverse-hash-bw
  (eval-when-compile
    (uniline--make-hash uniline--glyphs-bw))
  "Same as `uniline--glyphs-bw' reversing keys & values.")

;;;╭───────────────────────────────────────────────────────────╮
;;;│Reference tables of ▙▄▟▀ quadrant-blocks UNICODE characters│
;;;╰───────────────────────────────────────────────────────────╯

;; Hereafter `4quadb' means a representation of a quadrant-block
;; UNICODE character as a single number.  This number must hold
;; all combinations of the 4 quarter-of-a-blocks.  Each of the 4
;; may be present or absent.  Therfore `4quadb' is a number from 0
;; to 2x2x2x2 = 16.

(eval-and-compile
  (defconst uniline--4quadb-to-char
    [   ?  ?▘ ?▝ ?▀
        ?▖ ?▌ ?▞ ?▛
        ?▗ ?▚ ?▐ ?▜
        ?▄ ?▙ ?▟ ?█
        ]
    "Convert a quadrant bitmap into a UNICODE character.
A quadrant bitmap is a number in [0..16) made of 4 bits.
Each bit says whether or not there is a quadrant-block
at a position.
Reverse of `uniline--char-to-4quadb'"))

(eval-and-compile
  (defconst uniline--char-to-4quadb
    (eval-when-compile
      (let ((table (make-char-table 'uniline--char-to-4quadb)))
        (cl-loop
         for c across uniline--4quadb-to-char
         for i from 0
         do (aset table c i))
        table))
    "Convert a UNICODE character to a quadrant bitmap.
Reverse of `uniline--4quadb-to-char'"))

(defvar-local uniline--which-quadrant 0
  "Where is the quadrant cursor.
To draw lines with quadrant-blocks like this ▙▄▟▀,
it is required to keep track where is the
quadrant-cursor.  It can be at 4 sub-locations:
north-east, south-west, etc.

1: here──→───╮
           ╭─┴╮
0: here──→─┤▘▝│
2: here──→─┤▖▗│
           ╰─┬╯
3: here──→───╯")

;;;╭──────────────────────╮
;;;│Inserting a character │
;;;╰──────────────────────╯

;; Formerly, `self-insert-command' was used directly
;; along with the `post-self-insert-hook' to avoid the cursor
;; moving right.
;;
;; But `self-insert-command' has too many side-effects.
;; Besides, other utilities like `electric-pair-mode' assume that
;; `last-command-event' is equal to the parameter given to
;; `self-insert-command'.
;;
;; A side effect of using `post-self-insert-hook' was that inserting
;; an ordinary character did not move the cursor.
;;
;; For those reasons, direct usage of `self-insert-command' was
;; replaced by a simple call to `insert' with `delete-char' to
;; simulate the `overwrite-mode'.

(defun uniline--insert-char (char)
  "Insert CHAR in overwrite mode avoiding cursor moving."
  ;; `insert' before `delete-char' to preserve `point-marker'
  (insert (if (eq char ?\n) ?  char))
  (or (eolp) (delete-char 1))
  (backward-char))

;;;╭────────────────────────────────────────────────────────╮
;;;│Low level management of ┏╾╯half-lines UNICODE characters│
;;;╰────────────────────────────────────────────────────────╯

(defvar-local uniline--brush 1
  "Controls the style of line.
Possible values are as follows:
nil: no action except cursor movements
  0: erase,
  1: simple line     like ╰─┤
  2: thick  line     like ┗━┫
  3: double line     like ╚═╣
:block block drawing like ▙▄▟▀")

(eval-when-compile ; not needed at runtime
  (defmacro uniline--get-4halfs (&optional char)
    "Return a bit pattern (a 4halfs).
It represents a UNICODE character like ┬ found at (point).
If CHAR is given, use this CHAR instead of the character
found at (point).
Return nil if the character is not a 4halfs character."
    `(aref
      uniline--char-to-4halfs
      ,(or char '(uniline--char-after)))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--get-4quadb (&optional char)
    "Return a bit pattern (a 4quadb).
It represents a UNICODE character like ▙ found at (point).
If CHAR is given, use this CHAR instead of the character
found at (point).
Return nil if the character is not a 4quadb character."
    `(aref
      uniline--char-to-4quadb
      ,(or char '(uniline--char-after)))))

(eval-when-compile ; not needed at runtime
  (defsubst uniline--insert-4halfs (4halfs)
    "Insert at (point) a UNICODE like ┬.
The UNICODE character is described by the 4HALFS bits pattern.
The (point) does not move."
    (uniline--insert-char
     (aref uniline--4halfs-to-char 4halfs))))

(eval-when-compile ; not needed at runtime
  (defsubst uniline--insert-4quadb (4quadb)
    "Insert at (point) a UNICODE like ▙.
The UNICODE character is described by the 4QUADB bits pattern.
The (point) does not move."
    (uniline--insert-char
     (aref uniline--4quadb-to-char 4quadb))))

;;;╭───────────────────────────────────────────────────────────────╮
;;;│Low level management of ▙▄▟▀ quadrant-blocks UNICODE characters│
;;;╰───────────────────────────────────────────────────────────────╯

(defun uniline--quadrant-undo (i)
  "Re-position `uniline--which-quadrant' on undo.
This function sets the cursor to I.
It is called from the deep intricacies of the standard
undo machinery."
  (setq uniline--which-quadrant i))

(defun uniline--store-undo-quadrant-cursor ()
  "Helper function to store the quandrant-cursor in the undo history.
The standard Emacs undo mechanism already restores the point location.
The quadrant cursor is 1/4 smaller than the regular cursor, and
needs a specific action to restore it."
  (setq buffer-undo-list
        `((apply
           uniline--quadrant-undo
           ,uniline--which-quadrant)
          ,(point)
          ,@buffer-undo-list)))

(defun uniline--write-one-4quadb (force)
  "Helper function to write the quadrant cursor at point.
It adds the quadrant-block described by `uniline--which-quadrant'
at `point', preserving already present quadrant-blocks.
When FORCE is not nil, overwrite a possible non quadrant-block
character at point."
  (if (eolp)
      (uniline--insert-char ? ))
  (let ((bits
         (or
          (uniline--get-4quadb)
          (and force 0))))
    (if bits
        (uniline--insert-4quadb
         (logior bits (ash 1 uniline--which-quadrant))))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--clear-two-4quadb (dir)
    "Helper function to clear half a quadrant-block at point.
Assume that point is on a quadrant-block character.
Clear the half of this character pointing in DIR direction."
    `(let ((bits (uniline--get-4quadb)))
       (if bits
           (uniline--insert-4quadb
            (logand
             bits
             (eval-when-compile
               (uniline--get-4quadb
                (pcase ',dir
                  ('uniline--direction-up↑ ?▄)
                  ('uniline--direction-ri→ ?▌)
                  ('uniline--direction-dw↓ ?▀)
                  ('uniline--direction-lf← ?▐))))))))))

;;;╭────────────────────────────╮
;;;│Test blanks in the neighbour│
;;;╰────────────────────────────╯

(defun uniline--blank-at-point (p)
  "Return non-nil if P points to a 4halfs character.
This includes
- a blank character,
- or a new line
- or nil
The last two cases will be changed to an actual blank character by
virtue of the infinite buffer."
  (or
   (not p)
   (= (point-max) p) ;; corner case
   (eq (char-after p) ?\n)
   (uniline--get-4halfs (char-after p))
   (uniline--get-4quadb (char-after p))))

(defun uniline--blank-neighbour1 (dir)
  "Return non-nil if the neighbour of current point in DIR is blank.
The neighbour is one character away in the DIR direction.
Blank include:
- actual blank
- new line
- outside buffer in the right→ or down↓ DIRections"
  (uniline--blank-at-point
   (cond
    ((eq dir (eval-when-compile uniline--direction-up↑))
     (uniline--neighbour-point  uniline--direction-up↑))
    ((eq dir (eval-when-compile uniline--direction-ri→))
     (uniline--neighbour-point  uniline--direction-ri→))
    ((eq dir (eval-when-compile uniline--direction-dw↓))
     (uniline--neighbour-point  uniline--direction-dw↓))
    ((eq dir (eval-when-compile uniline--direction-lf←))
     (uniline--neighbour-point  uniline--direction-lf←)))))

(defun uniline--blank-neighbour4 (dir)
  "Return non-nil if the neighbour of current quarter point in DIR is blank.
The neighbour is half a character away in the DIR direction.
Blank include:
- actual blank
- new line
- outside buffer in the right→ or down↓ DIRections
- point is on character containing a quarter block, and the quarter-cursor
  can further move in DIR direction while point stay still."
  (cond
   ((eq dir (eval-when-compile uniline--direction-up↑))
    (or
     (memq uniline--which-quadrant '(2 3))
     (uniline--blank-at-point
      (uniline--neighbour-point uniline--direction-up↑))))
   ((eq dir (eval-when-compile uniline--direction-ri→))
    (or
     (memq uniline--which-quadrant '(0 2))
     (uniline--blank-at-point
      (uniline--neighbour-point uniline--direction-ri→))))
   ((eq dir (eval-when-compile uniline--direction-dw↓))
    (or
     (memq uniline--which-quadrant '(0 1))
     (uniline--blank-at-point
      (uniline--neighbour-point uniline--direction-dw↓))))
   ((eq dir (eval-when-compile uniline--direction-lf←))
    (or
     (memq uniline--which-quadrant '(1 3))
     (uniline--blank-at-point
      (uniline--neighbour-point uniline--direction-lf←))))))

(defun uniline--blank-neighbour (dir)
  "Return non-nil if the neighbour in DIR direction is blank.
Depending on `uniline--brush', the neighbour may be one character away,
or half a character away."
  (if (eq uniline--brush :block)
      (uniline--blank-neighbour4 dir)
    (uniline--blank-neighbour1 dir)))

;;;╭──────────────────────────────────────────────────╮
;;;│High level drawing in half-lines & quadrant-blocks│
;;;╰──────────────────────────────────────────────────╯

(defvar-local uniline--arrow-direction
  uniline--direction-up↑
  "Where the next arrow should point to.
This might be 0, 1, 2, 3, as defined by the four constants
`uniline--direction-up↑', `uniline--direction-lf←', ...")

(eval-when-compile ; not needed at runtime
  (defmacro uniline--write-one-4halfs (dir force)
    "Draw half a line in the direction DIR.
If there are too few characters on the row where the line
will be drawn, fill it with blank characters.
Cursor does not move.
When FORCE is not nil, overwrite a possible non-4halfs character."
    `(progn
       (if (eolp)
           (uniline--insert-char ? ))
       (if uniline--brush
           (let ((bits (or (uniline--get-4halfs) (and ,force 0))))
             (cond
              ;; 1st case: (char-after) is a line-character like ╶┤,
              ;; or any character if FORCE
              ;; then change a half-line of this character
              ;; for example changing it from ├ to ┽
              (bits
               (uniline--insert-4halfs
                (logior
                 (logand
                  bits
                  (eval-when-compile (lognot (ash 3 (* 2 ,dir)))))
                 (ash uniline--brush (eval-when-compile (* 2 ,dir))))))
              ;; 2nd case: (char-after) is a block character like ▜,
              ;; and the brush is the eraser
              ;; then clear only half of this character
              ((eq uniline--brush 0)
               (uniline--clear-two-4quadb ,dir))))))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--write-impl (dir blockbit compbit)
    "Move cursor in direction DIR drawing or erasing lines.
Or extending region.
This is an implementation function for the 4 actual functions.
- If region is already active, just extend it without drawing.
- If the brush is set to `:block', draw one quadrant-block.
  Then move cursor half a character.
  This could move (point) one char, or leave it where it is.
- Otherwise, draw or erase pairs of half-lines.
  `uniline--brush' gives the style of line (it may be an eraser).
REPEAT is the length of the line to draw or erase,
or the number of quadrant-blocks to draw,
or the length to extend region.
REPEAT defaults to 1.
When FORCE is not nil, overwrite characters which are not lines.
BLOCKBIT and COMPBIT are bit-patterns used to manage the
quadrant-blocks cursor `uniline--which-quadrant'.
BLOCKBIT and COMPBIT could be deduced from DIR,
but that would be overkill."
    `(progn
       (unless repeat (setq repeat 1))
       (setq uniline--arrow-direction ,dir)
       (handle-shift-selection)
       (cond
        ((region-active-p)
         ;; region is marked, continue extending it
         (uniline--move-in-direction ,dir repeat)
         (setq deactivate-mark nil))
        ((eq uniline--brush :block)
         ;; draw quadrant-blocks ▝▙▄▌
         (cl-loop
          repeat repeat
          do
          (uniline--store-undo-quadrant-cursor)
          (if (eq (logand uniline--which-quadrant ,blockbit) ,compbit)
              (uniline--move-in-direction ,dir))
          (setq uniline--which-quadrant
                (logxor uniline--which-quadrant ,blockbit))
          (uniline--write-one-4quadb force)))
        (t
         ;; draw lines ╰──╮
         (cl-loop
          repeat repeat
          do
          (uniline--write-one-4halfs ,dir force)
          until (uniline--at-border-p ,dir)
          do
          (uniline--move-in-direction ,dir)
          (uniline--write-one-4halfs
           ,(uniline--reverse-direction dir)
           force)))))))

(defun uniline-write-up↑ (repeat &optional force)
  "Move cursor up drawing or erasing glyphs, or extending region.
- If region is already active, just extend it without drawing.
- If the brush is set to blocks, draw one quadrant-block.
  Then move cursor half a character.
- Otherwise, draw or erase pairs of half-lines.
  `uniline--brush' gives the style of line (it may be an eraser).
REPEAT is the length of the line to draw or erase,
or the number of quadrant-blocks to draw,
or the length to extend region.
REPEAT defaults to 1.
When FORCE is not nil, overwrite characters which are not lines."
  (interactive "P")
  (uniline--write-impl uniline--direction-up↑ 2 0))

(defun uniline-write-ri→ (repeat &optional force)
  "Move cursor right drawing or erasing glyphs, or extending region.
- If region is already active, just extend it without drawing.
- If the brush is set to blocks, draw one quadrant-block.
  Then move cursor half a character.
- Otherwise, draw or erase pairs of half-lines.
  `uniline--brush' gives the style of line (it may be an eraser).
REPEAT is the length of the line to draw or erase,
or the number of quadrant-blocks to draw,
or the length to extend region.
REPEAT defaults to 1.
When FORCE is not nil, overwrite characters which are not lines."
  (interactive "P")
  (uniline--write-impl uniline--direction-ri→ 1 1))

(defun uniline-write-dw↓ (repeat &optional force)
  "Move cursor down drawing or erasing glyphs, or extending region.
- If region is already active, just extend it without drawing.
- If the brush is set to blocks, draw one quadrant-block.
  Then move cursor half a character.
- Otherwise, draw or erase pairs of half-lines.
  `uniline--brush' gives the style of line (it may be an eraser).
REPEAT is the length of the line to draw or erase,
or the number of quadrant-blocks to draw,
or the length to extend region.
REPEAT defaults to 1.
When FORCE is not nil, overwrite characters which are not lines."
  (interactive "P")
  (uniline--write-impl uniline--direction-dw↓ 2 2))

(defun uniline-write-lf← (repeat &optional force)
  "Move cursor left drawing or erasing glyphs, or extending region.
- If region is already active, just extend it without drawing.
- If the brush is set to blocks, draw one quadrant-block.
  Then move cursor half a character.
- Otherwise, draw or erase pairs of half-lines.
  `uniline--brush' gives the style of line (it may be an eraser).
REPEAT is the length of the line to draw or erase,
or the number of quadrant-blocks to draw,
or the length to extend region.
REPEAT defaults to 1.
When FORCE is not nil, overwrite characters which are not lines."
  (interactive "P")
  (uniline--write-impl uniline--direction-lf← 1 0))

(defun uniline-overwrite-up↑ (repeat)
  "Like `uniline-write-up↑' but overwriting.
REPEAT is the length of the line to draw, defaulting to 1."
  (interactive "P")
  (uniline-write-up↑ repeat t))

(defun uniline-overwrite-ri→ (repeat)
  "Like `uniline-write-ri→' but overwriting.
REPEAT is the length of the line to draw, defaulting to 1."
  (interactive "P")
  (uniline-write-ri→ repeat t))

(defun uniline-overwrite-dw↓ (repeat)
  "Like `uniline-write-dw↓' but overwriting.
REPEAT is the length of the line to draw, defaulting to 1."
  (interactive "P")
  (uniline-write-dw↓ repeat t))

(defun uniline-overwrite-lf← (repeat)
  "Like `uniline-write-lf←' but overwriting.
REPEAT is the length of the line to draw, defaulting to 1."
  (interactive "P")
  (uniline-write-lf← repeat t))

(defun uniline--write (dir &optional force)
  "Move cursor one char in DIR direction.
Doing so, draw or erase glyphs, or extend region.
- If region is already active, just extend it without drawing.
- If the brush is set to blocks, draw one quadrant-block.
  Then move cursor half a character.
- Otherwise, draw or erase pairs of half-lines.
  `uniline--brush' gives the style of line (it may be an eraser).
When FORCE is not nil, overwrite whatever is in the buffer."
  (cond
   ((eq dir (eval-when-compile uniline--direction-up↑)) (uniline-write-up↑ 1 force))
   ((eq dir (eval-when-compile uniline--direction-ri→)) (uniline-write-ri→ 1 force))
   ((eq dir (eval-when-compile uniline--direction-dw↓)) (uniline-write-dw↓ 1 force))
   ((eq dir (eval-when-compile uniline--direction-lf←)) (uniline-write-lf← 1 force))
   (t (error "Bad DIR %s" dir))))

;;;╭────╮
;;;│Fill│
;;;╰────╯

(defun uniline--choose-fill-char ()
  "Interactively choose a character to fill a shape.
This character will replace the character the cursor is on
throughout the shape.
Some values of the input character are replaced by computed values:
- `C-y' chooses the first character in the kill ring
- `SPC' selects a darker  shade of grey than the character the point is on.
- `DEL' selects a lighter shade of grey than the character the point is on.
  there are 5 shades of grey in the UNICODΕ standard: \" ░▒▓█\".
- `RET' means abort filling."
  (let ((char (read-char "Fill with (any char, C-y, SPC, DEL, RET)? ")))
    (cond
     ((eq char 13) nil)              ; RET: abort filling
     ((eq char ?)                  ; yank: 1st char of the kill ring
      (aref (car kill-ring) 0))
     ((eq char ? )                      ; SPC: next shade of grey
      (or (cadr (memq (uniline--char-after) '(?  ?░ ?▒ ?▓ ?█ )))
          ? ))
     ((eq char ?)                     ; DEL: prev shade of grey
      (or (cadr (memq (uniline--char-after) '(?█ ?▓ ?▒ ?░ ? )))
          ?█))
     (t char))))

(defun uniline-fill (char)
  "Fill a hollow shape with character CHAR.
The hole is the set of contiguous identical characters.
The character at point is used as reference for other
identical characters."
  (interactive)
  ;; why is stack initialized with twice the current point?
  ;; the first is for starting the filling process
  ;; the second is for returning to the starting point after filling
  (let ((currentchar (uniline--char-after))
        (stack (list (point) (point)))
        p)
    (if (and
         char
         (not (eq char currentchar)))
        (while stack
          (goto-char (pop stack))
          (when (eq (char-after) currentchar) ; not (uniline--char-after) !
            (uniline--insert-char char)
            (if (setq p (uniline--neighbour-point uniline--direction-lf←))
                (push p stack))
            (if (setq p (uniline--neighbour-point uniline--direction-ri→))
                (push p stack))
            (if (setq p (uniline--neighbour-point uniline--direction-up↑))
                (push p stack))
            (if (setq p (uniline--neighbour-point uniline--direction-dw↓))
                (push p stack)))))))

;;;╭───────────────────────────────────╮
;;;│High level management of rectangles│
;;;╰───────────────────────────────────╯

(eval-when-compile ; not needed at runtime
  (defmacro uniline--operate-on-rectangle (&rest body)
    "Execute BODY with some variables describing selection.
This is a helper macro to write actual functions.
Assumes that a selection is active.
The variables
  BEG BEGX BEGY  (point, column, line of beginning)
  END ENDX ENDY  (point, column, line of ending)
  WIDTH HEIGHT   (of selection)
are made available to BODY for easy handling of the selection.
The selection may be reversed in any way, the variables
are sets as if the selection was made from
the upper-most, left-most to the lower-most, right-most points.
It works even when in `rectangle-mark-mode'.
Note that ENDX & ENDY point outside the selection in such a way that
WIDTH=ENDX-BEGX, HEIGHT=ENDY-BEGY
After execution of the body, selection is activated
from BEGX,BEGY inclusive to ENDX,ENDY exclusive
in `rectangle-mark-mode'."
    (declare (debug (body)))
    `(when (region-active-p)
       (rectangle-mark-mode -1)     ; otherwise sometimes end is wrong
       (let* ((deactivate-mark) ; kludge needed to avoid deactivating the mark
              (beg (region-beginning))
              (end (region-end))
              (begx (progn (goto-char beg) (current-column)))
              (begy (1- (line-number-at-pos)))
              (endx (progn (goto-char end) (current-column)))
              (endy (line-number-at-pos))
              (height (- endy begy))
              (width  (- endx begx)))
         (when (< endx begx)
           (setq endx (prog1 begx (setq begx endx)))
           (setq width (- width))
           (setq beg (- beg width))
           (setq end (+ end width)))
         ,@body
         (uniline--move-to-lin-col (1- endy) endx)
         (set-mark (point))
         (uniline--move-to-lin-col begy begx)
         (rectangle-mark-mode 1)))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--translate-1xsize-slice (dir size)
    "Translate a rectangle 1 x SIZE rectangle one char.
This is a helper function called several times to move
a rectangle one slice at a time.
DIR is the direction to move.
The cursor (point) is one end of the slice.
When moved, the 1 x SIZE rectangle leaves a blank char.
This char is filled with leakage from its two neighbours.
For instance consider a situation like this:
   ╶┬╴
    E
    ┗╸
where E is the char leaved empty after translation.
Then the leakage of the two glyphs fills in E:
   ╶┬╴
    ╽
    ┗╸"
    (let ((odir (uniline--reverse-direction dir)))
      `(let ((here (or (uniline--get-4halfs) 0))
             (prev    ; char preceding (point) as a 4halfs-bit-pattern
              (let ((p (uniline--neighbour-point ,odir)))
                (or
                 (and p (uniline--get-4halfs (char-after p)))
                 0))))
         ;; mask pairs of bits in the desired direction
         (setq
          here (logand here (eval-when-compile (ash 3 (* 2 ,odir))))
          prev (logand prev (eval-when-compile (ash 3 (* 2 ,dir)))))

         ;; rotate pairs of bits 180°
         (setq
          here (ash here (eval-when-compile (* 2 (- ,dir ,odir))))
          prev (ash prev (eval-when-compile (* 2 (- ,odir ,dir)))))

         ;; initialize `hand' with the computed leakage from neighbours
         ;; then iterate to move one char at a time, keeping it in `hand'
         (let ((hand (aref uniline--4halfs-to-char (logior here prev))))
           (cl-loop
            repeat ,size
            do
            (setq hand
                  (prog1 (uniline--char-after)
                    (uniline--insert-char hand)))
            (uniline--move-in-direction ,dir))
           (uniline--insert-char hand))))))

(defun uniline-move-rect-up↑ (repeat)
  "Move the rectangle marked by selection one line upper.
Truncate the selection if it touches the upper side of the buffer.
REPEAT tells how many characters the rectangle should move,
defaulting to 1."
  (interactive "P")
  (cl-loop
   repeat (or repeat 1)
   do
   (uniline--operate-on-rectangle
    (if (eq begy 0) (setq height (1- height)))
    (setq
     begy (max (1- begy) 0)
     endy (max (1- endy) 0))
    (cl-loop
     repeat width
     for x from begx
     do
     (uniline--move-to-lin-col endy x)
     (uniline--translate-1xsize-slice uniline--direction-up↑ height)))))

(defun uniline-move-rect-ri→ (repeat)
  "Move the rectangle marked by selection one char to the left.
The buffer is infinite at its right side.
REPEAT tells how many characters the rectangle should move,
defaulting to 1."
  (interactive "P")
  (cl-loop
   repeat (or repeat 1)
   do
   (uniline--operate-on-rectangle
    (cl-loop
     repeat height
     for y from begy
     do
     (uniline--move-to-lin-col y begx)
     (uniline--translate-1xsize-slice uniline--direction-ri→ width))
    (setq
     begx (1+ begx)
     endx (1+ endx)))))

(defun uniline-move-rect-dw↓ (repeat)
  "Move the rectangle marked by selection one line down.
The buffer is infinite at the bottom.
REPEAT tells how many characters the rectangle should move,
defaulting to 1."
  (interactive "P")
  (cl-loop
   repeat (or repeat 1)
   do
   (uniline--operate-on-rectangle
    (cl-loop
     repeat width
     for x from begx
     do
     (uniline--move-to-lin-col begy x)
     (uniline--translate-1xsize-slice uniline--direction-dw↓ height))
   (setq
    begy (1+ begy)
    endy (1+ endy)))))

(defun uniline-move-rect-lf← (repeat)
  "Move the rectangle marked by selection one char to the left.
Truncate the selection if it touches the left side of the buffer.
REPEAT tells how many characters the rectangle should move,
defaulting to 1."
  (interactive "P")
  (cl-loop
   repeat (or repeat 1)
   do
   (uniline--operate-on-rectangle
    (if (eq begx 0) (setq width (1- width)))
    (setq
     begx (max (1- begx) 0)
     endx (max (1- endx) 0))
    (cl-loop
     repeat height
     for y from begy
     do
     (uniline--move-to-lin-col y endx)
     (uniline--translate-1xsize-slice uniline--direction-lf← width)))))

(defun uniline-fill-rectangle ()
  "Fill the rectangle marked by selection.
Interactively choose the filling character.
See `uniline--choose-fill-char'."
  (interactive)
  (let ((char (uniline--choose-fill-char)))
    (uniline--operate-on-rectangle
     (cl-loop
      repeat height
      for y from begy
      do
      (uniline--move-to-lin-col y begx)
      (cl-loop
       repeat width
       do
       (uniline--insert-char char)
       (uniline--move-to-delta-column 1))))))

(defun uniline-draw-inner-rectangle (&optional force)
  "Draws a rectangle inside a rectangular selection.
Use the current brush style, which may be thin, thick,
double line, block, or eraser.
When FORCE is not nil, overwrite whatever is there."
  (interactive)
  (uniline--operate-on-rectangle
   (setq
    width  (1- width)
    height (1- height))
   (goto-char beg)
   (if (eq uniline--brush :block)
       (setq
        width  (+ width  width  1)
        height (+ height height 1)
        uniline--which-quadrant 0))
   (let ((mark-active nil)) ;; otherwise brush would be inactive
     (uniline-write-ri→ width  force)
     (uniline-write-dw↓ height force)
     (uniline-write-lf← width  force)
     (uniline-write-up↑ height force))))

(defun uniline-overwrite-inner-rectangle ()
  "Draws a rectangle inside a rectangular selection.
Use the current brush style, which may be thin, thick,
double line, block, or eraser.
Overwrite whatever is there."
  (interactive)
  (uniline-draw-inner-rectangle t))

(defun uniline-draw-outer-rectangle (&optional force)
  "Draws a rectangle outside a rectangular selection.
Use the current brush style, which may be thin, thick,
double line, block, or eraser.
When FORCE is not nil, overwrite whatever is there."
  (interactive)
  (uniline--operate-on-rectangle
   (goto-char beg)
   (if (<= begx 0)                      ; at leftmost side of buffer
       (setq width (1- width))
     (uniline--move-to-delta-column -1))
   (when (eq begy 0)                     ; at the top of buffer
     (goto-char (point-min))
     (insert ?\n))
   (forward-line -1)
   (if (eq uniline--brush :block)
       (setq
        width  (+ width  width )
        height (+ height height)
        uniline--which-quadrant 3))
   (setq
    width  (1+ width)
    height (1+ height))
   (uniline--move-to-column (1- begx))
   (let ((mark-active nil))             ; otherwise brush is inactive
     (uniline-write-ri→ width  force)
     (uniline-write-dw↓ height force)
     (uniline-write-lf← width  force)
     (if (> begx 0)
         (uniline-write-up↑ height force)))
   (when (eq begy 0)
     (goto-char (point-min))
     (delete-line))))

(defun uniline-overwrite-outer-rectangle ()
  "Draws a rectangle outside a rectangular selection.
Use the current brush style, which may be thin, thick,
double line, block, or eraser.
Overwrite whatever is there."
  (interactive)
  (uniline-draw-outer-rectangle t))

(defun uniline-copy-rectangle ()
  "Copy the selected rectangle in the kill storage."
  (interactive)
  (copy-rectangle-as-kill (region-beginning) (region-end)))

(defun uniline-kill-rectangle ()
  "Kill the selected rectangle.
It differs from the standard Emacs `kill-rectangle'
in that it leaves a rectangle of space characters."
  (interactive)
  (copy-rectangle-as-kill (region-beginning) (region-end))
  (clear-rectangle        (region-beginning) (region-end)))

(defun uniline-yank-rectangle ()
  "Insert a previously cut rectangle.
It differs from the standard Emacs `yank-rectangle'
in that it overwrites the rectangle."
  (interactive)
  (uniline--operate-on-rectangle
   height ;; suppress warning
   (goto-char beg)
   (cl-loop
    for line in killed-rectangle
    do
    (cl-loop
     for char across line
     do
     (uniline--insert-char char)
     (uniline--move-to-delta-column 1))
    (uniline--move-to-column begx)
    (uniline--move-to-delta-line 1))
   (setq endx (+ begx (length (car killed-rectangle))))
   (setq endy (+ begy (length killed-rectangle)))))

;;;╭──────────────╮
;;;│Text direction│
;;;╰──────────────╯

(defvar-local uniline--text-direction
    uniline--direction-ri→
  "Direction of text insertion.
It can be any of the 4 values of
`uniline--direction-up↑' `-ri→' `-dw↓' `-lf←'
which means that typing a key on the keyboard moves the cursor
in this direction.
It can also be nil, which means that uniline makes no tweaking of
the natural cursor movement upon insertion.")

(defun uniline--post-self-insert ()
  "Change the cursor movement after `self-insert-command'.
Usually the cursor moves to the right.
Sometimes to the left for some locales, but this is not currently handled.
This hook fixes the cursor movement according to `uniline--text-direction'"
  (let ((n
         (cond
          ((numberp current-prefix-arg)
           current-prefix-arg)
          ((and
            (consp current-prefix-arg)
            (numberp (car current-prefix-arg)))
           (car current-prefix-arg))
          ((null current-prefix-arg)
           1)
          (t (error "current-prefix-arg = %S" current-prefix-arg)))))
    (cond
     ((eq uniline--text-direction (eval-when-compile uniline--direction-ri→)))
     ((eq uniline--text-direction (eval-when-compile uniline--direction-dw↓))
      (forward-char (- n))
      (uniline--move-to-delta-line n))
     ((eq uniline--text-direction (eval-when-compile uniline--direction-up↑))
      (forward-char (- n))
      (uniline--move-to-delta-line (- n)))
     ((eq uniline--text-direction (eval-when-compile uniline--direction-lf←))
      (forward-char (- n))
      (uniline--move-to-delta-column (- n)))
     ((eq uniline--text-direction nil))
     (t (error "Bad uniline--text-direction %S" uniline--text-direction)))))

(defun uniline-text-direction-up↑ ()
  "Set text insertion direction up↑."
  (interactive)
  (setq uniline--text-direction uniline--direction-up↑))
(defun uniline-text-direction-ri→ ()
  "Set text insertion direction right→."
  (interactive)
  (setq uniline--text-direction uniline--direction-ri→))
(defun uniline-text-direction-dw↓ ()
  "Set text insertion direction down↓."
  (interactive)
  (setq uniline--text-direction uniline--direction-dw↓))
(defun uniline-text-direction-lf← ()
  "Set text insertion direction left←."
  (interactive)
  (setq uniline--text-direction uniline--direction-lf←))

;;;╭───────────────────────────╮
;;;│Macro calls in 4 directions│
;;;╰───────────────────────────╯

(defvar-local uniline--directional-macros
    (make-vector 8 nil)
  "A cache handling 4 versions of the current macro in 4 directions.
It is needed to avoid repeatidly re-creating a new directional macro
from the recorded macro.
There are 4 entries indexed by
`uniline--direction-up↑' `-ri→' `-dw↓' `-lf←'
Each entry has 2 slots:
- the recorded macro (a vector of key strokes)
- the twisted macro (the same vector with <up> <right> and sisters twisted).")

(eval-when-compile ; not used at runtime
  (defconst uniline--directional-keystrokes-table
    `(
      ;;   ╭─keystroke as used in keyboard macros
      ;;   │      ╭─direction of the keystroke
      ;;   │      │      shift-control modifiers╮
      ;;   │      │                      ╭──────╯
      ;;   ▽      ▽                      ▽
      (  right ,uniline--direction-ri→)
      (  down  ,uniline--direction-dw↓)
      (  left  ,uniline--direction-lf←)
      (  up    ,uniline--direction-up↑)
      (S-right ,uniline--direction-ri→ . S)
      (S-down  ,uniline--direction-dw↓ . S)
      (S-left  ,uniline--direction-lf← . S)
      (S-up    ,uniline--direction-up↑ . S)
      (C-right ,uniline--direction-ri→ . C)
      (C-down  ,uniline--direction-dw↓ . C)
      (C-left  ,uniline--direction-lf← . C)
      (C-up    ,uniline--direction-up↑ . C))
    "Temporary table of conversion between keystrokes and uniline directions.
It will be converted into 2 hashtables for both conversions."))

(defconst uniline--keystroke-to-dir-shift
  (eval-when-compile
    (let ((table (make-hash-table)))
      (cl-loop
       for entry in uniline--directional-keystrokes-table
       do
       (puthash (car entry) (cdr entry) table))
      table))
  "Hashtable to convert a directional keystroke into Uniline constants.")

(defconst uniline--dir-shift-to-keystroke
  (eval-when-compile
    (let ((table (make-hash-table :test #'equal)))
      (cl-loop
       for entry in uniline--directional-keystrokes-table
       do
       (puthash (cdr entry) (car entry) table))
      table))
  "Hashtable to convert Uniline directional constants into keystrokes.")

(defun uniline-call-macro-in-direction (dir)
  "Call last keybord macro twisted in DIR direction.
A twisted version of the last keybord macro is created, unless
it is already present in the `uniline--directional-macros' cache"
  (interactive)
  (let*
      ((uniline--text-direction dir)
       (dir2 (* 2 dir))
       (last-kbd-macro
        (or (and (eq (aref uniline--directional-macros dir2) last-kbd-macro)
                 (aref uniline--directional-macros (1+ dir2)))
            (progn
              (aset uniline--directional-macros dir2 last-kbd-macro)
              (aset uniline--directional-macros (1+ dir2)
                    (vconcat
                     (mapcar
                      (lambda (x)
                        (let ((y (gethash x uniline--keystroke-to-dir-shift)))
                          (if y
                              (gethash
                               (cons
                                (mod (+ (car y) dir 3) 4)
                                (cdr y))
                               uniline--dir-shift-to-keystroke)
                            x)))
                      last-kbd-macro)))))))
    (kmacro-end-and-call-macro 1)))

;;;╭───────────────────────────╮
;;;│High level brush management│
;;;╰───────────────────────────╯

(defun uniline--set-brush-nil ()
  "Change the current style of line to nothing.
It means that cursor movements do not trace anything."
  (interactive)
  (setq uniline--brush nil)
  (force-mode-line-update))

(defun uniline--set-brush-0 ()
  "Change the current style of line to the eraser.
It means that moving the cursor horizontally erase horizontal
lines, and moving vertically erase vertical lines.  Characters
other than lines or arrows are not touched."
  (interactive)
  (setq uniline--brush 0)
  (force-mode-line-update))

(defun uniline--set-brush-1 ()
  "Change the current style of line to a single thin line╶─╴."
  (interactive)
  (setq uniline--brush 1)
  (force-mode-line-update))

(defun uniline--set-brush-2 ()
  "Change the current style of line to a single thick line╺━╸."
  (interactive)
  (setq uniline--brush 2)
  (force-mode-line-update))

(defun uniline--set-brush-3 ()
  "Change the current style of line to a double line╺═╸."
  (interactive)
  (setq uniline--brush 3)
  (force-mode-line-update))

(defun uniline--set-brush-block ()
  "Change the current style of line to blocks ▙▄▟▀."
  (interactive)
  (setq uniline--brush :block)
  (force-mode-line-update))

;;;╭─────────────────────────────────────╮
;;;│High level arrows & glyphs management│
;;;╰─────────────────────────────────────╯

(defun uniline--insert-glyph (letter back repeat)
  "Insert or modify a glyph.
LETTER is any of
 `a' for arrows
 `s' for squares
 `x' for crosses
 `o' for o-shapes.
If a glyph of that category is already there under (point),
it is modified with the next glyph in the same category.
If not, whatever character under (point) is overwritten
with the first glyph in the required category.
BACK is t for backward scan,
nil for forward scan in the same category.
REPEAT is the usual universal argument for repetition
of the same command."
  (if (and repeat (< repeat 0))
      (setq repeat (- repeat)
            back (not back)))
  (let ((line
         ;; line is something like:
         ;; (3 ((a ?↑ ?→ ?↓ ?←) (a ?▲ ?▶ ?▼ ?◀) …))
         ;;  △      △  △  △  △  current character is
         ;;  │      ╰──┴──┴──┴─╴one of those arrows
         ;;  ╰─────────────────╴oriented in this direction
         ;;
         ;; line can also be like:
         ;; (t ((a ?↕ ?↔ ?↕ ?↔) (a ?↑ ?→ ?↓ ?←) …))
         ;;  △      △  △  △  △  current character is
         ;;  │      ╰──┴──┴──┴─╴one of those arrows
         ;;  ╰─────────────────╴with no definite orientation
         ;;
         ;; or line may be like:
         ;; (nil ((s ?■) (s ?▫) …))
         ;;   △       △         current character is
         ;;   │       ╰────────╴this one
         ;;   ╰────────────────╴and it has NO orientation
         (gethash (uniline--char-after)
                  (if back
                      uniline--glyphs-reverse-hash-bw
                    uniline--glyphs-reverse-hash-fw))))
    (if (and
         line                  ; current character is one the known glyphs
         (fixnump (car line))) ; it has a north-south-east-west orientation
        (setq uniline--arrow-direction (car line)))
    (setq line
          (if line
              (cddr line)
            (if back uniline--glyphs-bw uniline--glyphs-fw)))
    (setq line
          (cl-loop
           for line2 on line
           if (or (null (caar line2)) ; currently useless
                  (eq (caar line2) letter))
           return line2
           if (eq (cdr line2) line) return nil))
    (when line
      (setq line (nthcdr (1- (or repeat 1)) line))
      (uniline--insert-char
       (if (cddar line)
           (nth uniline--arrow-direction (cdar line))
         (cadar line))))))

;; Run the following cl-loop to automatically write a bunch
;; of 8 interactive functions
;; They have little value, except to be an interface between
;; `easy-menu-define', `defhydra',
;; and the real function `uniline--insert-glyph'

(when nil
  (insert "\n;; BEGIN -- Automatically generated\n")
  (cl-loop
   for fb in '(f b)
   do
   (cl-loop
    for shape in
    '( (a . "arrow" )
       (s . "square")
       (o . "oshape")
       (x . "cross" ))
    do
    (cl-prettyprint
     `(defun ,(intern (format "uniline-insert-%sw-%s" fb (cdr shape))) (repeat)
        ,(format "Insert or modify a glyph of type %s in %s mode.
REPEAT cycles through glyphs list that many times, default to 1.
See `uniline--insert-glyph'."
                 (cdr shape)
                 (pcase fb ('f "forward") ('b "backward")))
        (interactive "P")
        (uniline--insert-glyph ',(car shape) ,(eq fb 'b) repeat)))))
  (insert "\n;; END -- Automatically generated\n"))

;; BEGIN -- Automatically generated

(defun uniline-insert-fw-arrow (repeat)
  "Insert or modify a glyph of type arrow in forward mode.
REPEAT cycles through glyphs list that many times, default to 1.
See `uniline--insert-glyph'."
  (interactive "P")
  (uniline--insert-glyph 'a nil repeat))
(defun uniline-insert-fw-square (repeat)
  "Insert or modify a glyph of type square in forward mode.
REPEAT cycles through glyphs list that many times, default to 1.
See `uniline--insert-glyph'."
  (interactive "P")
  (uniline--insert-glyph 's nil repeat))
(defun uniline-insert-fw-oshape (repeat)
  "Insert or modify a glyph of type oshape in forward mode.
REPEAT cycles through glyphs list that many times, default to 1.
See `uniline--insert-glyph'."
  (interactive "P")
  (uniline--insert-glyph 'o nil repeat))
(defun uniline-insert-fw-cross (repeat)
  "Insert or modify a glyph of type cross in forward mode.
REPEAT cycles through glyphs list that many times, default to 1.
See `uniline--insert-glyph'."
  (interactive "P")
  (uniline--insert-glyph 'x nil repeat))
(defun uniline-insert-bw-arrow (repeat)
  "Insert or modify a glyph of type arrow in backward mode.
REPEAT cycles through glyphs list that many times, default to 1.
See `uniline--insert-glyph'."
  (interactive "P")
  (uniline--insert-glyph 'a t repeat))
(defun uniline-insert-bw-square (repeat)
  "Insert or modify a glyph of type square in backward mode.
REPEAT cycles through glyphs list that many times, default to 1.
See `uniline--insert-glyph'."
  (interactive "P")
  (uniline--insert-glyph 's t repeat))
(defun uniline-insert-bw-oshape (repeat)
  "Insert or modify a glyph of type oshape in backward mode.
REPEAT cycles through glyphs list that many times, default to 1.
See `uniline--insert-glyph'."
  (interactive "P")
  (uniline--insert-glyph 'o t repeat))
(defun uniline-insert-bw-cross (repeat)
  "Insert or modify a glyph of type cross in backward mode.
REPEAT cycles through glyphs list that many times, default to 1.
See `uniline--insert-glyph'."
  (interactive "P")
  (uniline--insert-glyph 'x t repeat))
;; END -- Automatically generated

(defun uniline--rotate-arrow (dir)
  "Rotate an arrow so it points toward DIR."
  (let ((ligne
         (gethash
          (uniline--char-after)
          uniline--glyphs-reverse-hash-fw)))
    (when (car ligne) ;; if (point) is on a directional arrow
      (uniline--insert-char ;; then change its direction
       (nth (1+ dir) (cadr ligne)))
      (setq uniline--arrow-direction dir)
      )))

;; Run the following cl-loop to automatically write a bunch
;; of 4 interactive functions
;; They have little value, except to be an interface between
;; `easy-menu-define', `defhydra',
;; and the real function `uniline--rotate-arrow'

(when nil
  (insert "\n;; BEGIN -- Automatically generated\n")
  (cl-loop
   for dir in '("up↑" "ri→" "dw↓" "lf←")
   do
    (cl-prettyprint
     `(defun ,(intern (format "uniline-rotate-%s" dir)) ()
        ,(format "Rotate an arrow so it points toward %s." dir)
        (interactive)
        (uniline--rotate-arrow ,(intern (format "uniline--direction-%s" dir))))))
  (insert "\n;; END -- Automatically generated\n"))

;; BEGIN -- Automatically generated

(defun uniline-rotate-up↑ nil
  "Rotate an arrow so it points toward up↑."
  (interactive)
  (uniline--rotate-arrow uniline--direction-up↑))
(defun uniline-rotate-ri→ nil
  "Rotate an arrow so it points toward ri→."
  (interactive)
  (uniline--rotate-arrow uniline--direction-ri→))
(defun uniline-rotate-dw↓ nil
  "Rotate an arrow so it points toward dw↓."
  (interactive)
  (uniline--rotate-arrow uniline--direction-dw↓))
(defun uniline-rotate-lf← nil
  "Rotate an arrow so it points toward lf←."
  (interactive)
  (uniline--rotate-arrow uniline--direction-lf←))

;; END -- Automatically generated

;;;╭───────╮
;;;│Contour│
;;;╰───────╯

(defun uniline-contour (&optional force)
  "Draw a contour arround a block of characters.
A block of characters is a contiguous set of non-blank characters.
For the sake of the contour, a non-blank character is any character
not in the 4halfs set.
The style of the contour is determined by the current brush.
This includes possibly the eraser, which erases an actual contour.
When FORCE is not nil, overwrite whatever is in the buffer."
  (interactive)
  (while (not (uniline--blank-at-point (point)))
    (uniline--move-to-delta-column 1))

  (let ((dir uniline--direction-dw↓)
        (e)
        (start (point-marker))
        (q uniline--which-quadrant)
        (n 0))
    (and
     (uniline--blank-neighbour1 (setq e (uniline--turn-right       dir)))
     (uniline--blank-neighbour1 (setq e (uniline--turn-right (setq dir e))))
     (uniline--blank-neighbour1 (setq e (uniline--turn-right (setq dir e))))
     (uniline--blank-neighbour1         (uniline--turn-right (setq dir e)))
     (error "No border of any shape found around point"))
    (if (eq uniline--brush :block)
        (setq
         q
         (setq
          uniline--which-quadrant
          (cond
           ((eq dir (eval-when-compile uniline--direction-up↑)) 3)
           ((eq dir (eval-when-compile uniline--direction-ri→)) 2)
           ((eq dir (eval-when-compile uniline--direction-dw↓)) 0)
           ((eq dir (eval-when-compile uniline--direction-lf←)) 1)))))
    (while
        (progn
          (cond
           ((uniline--blank-neighbour (setq e (uniline--turn-right dir)))
            (setq dir e))
           ((uniline--blank-neighbour dir))
           ((uniline--blank-neighbour (setq dir (uniline--turn-left dir))))
           ((uniline--blank-neighbour (setq dir (uniline--turn-left dir))))
           (t (error "Cursor is surrounded by walls")))
          (cond
           ((and
             (eq dir (eval-when-compile uniline--direction-lf←))
             (uniline--at-border-p uniline--direction-lf←)
             (if (eq uniline--brush :block)
                 (memq uniline--which-quadrant '(0 2))
               t))
            (while
                (and
                 (= (forward-line -1) 0)
                 (not (uniline--blank-at-point (point)))))
            (if (uniline--at-border-p uniline--direction-up↑)
                (setq dir uniline--direction-up↑
                      uniline--which-quadrant 1)
              (setq dir uniline--direction-ri→
                    uniline--which-quadrant 2)))
           ((and
             (eq dir (eval-when-compile uniline--direction-up↑))
             (uniline--at-border-p uniline--direction-up↑)
             (if (eq uniline--brush :block)
                 (memq uniline--which-quadrant '(0 1))
               t))
            (while
                (progn
                  (forward-char 1)
                  (not (uniline--blank-at-point (point)))))
            (setq dir uniline--direction-dw↓)
            (setq uniline--which-quadrant 0))
           (t
            (uniline--write dir force)
            (setq n (1+ n))))
          (and
           (not
            (and (eq (point) (marker-position start))
                 (or (not (eq uniline--brush :block))
                     (eq uniline--which-quadrant q))))
           (< n 10000))))
    (set-marker start nil)
    (message "drew a %s steps contour" n)))

;;;╭────────────────╮
;;;│Hydra interfaces│
;;;╰────────────────╯

(defun uniline-customize-face ()
  "Customize a temporary font to may-be set it for future sessions."
  (interactive)
  (customize-face-other-window 'default))

(defun uniline--is-font (letter)
  "Check if current font is the one presented by LETTER."
  (cond
   ((eq letter ?d) (string-match "DejaVu"      (frame-parameter nil 'font)))
   ((eq letter ?u) (string-match "Unifont"     (frame-parameter nil 'font)))
   ((eq letter ?h) (string-match "Hack"        (frame-parameter nil 'font)))
   ((eq letter ?b) (string-match "JetBrain"    (frame-parameter nil 'font)))
   ((eq letter ?c) (string-match "Cascadia"    (frame-parameter nil 'font)))
   ((eq letter ?a) (string-match "Agave"       (frame-parameter nil 'font)))
   ((eq letter ?j) (string-match "Julia"       (frame-parameter nil 'font)))
   ((eq letter ?f) (string-match "FreeMono"    (frame-parameter nil 'font)))
   ((eq letter ?i) (string-match "Iosevka"     (frame-parameter nil 'font)))
   ((eq letter ?s) (string-match "Source Code" (frame-parameter nil 'font)))))

(defun uniline--is-font-str (letter)
  "Return a glyph if current font is the one presented by LETTER."
  (if (uniline--is-font letter) "▶" " "))

(defhydra uniline-hydra-fonts
  (:hint nil
   :exit nil)
  ;; Docstring MUST begin with an empty line to benefit from substitutions
  (concat
   (replace-regexp-in-string
    "_\\([dhcjbfsiua]\\)_ "
    "_\\1_%s(uniline--is-font-str ?\\1)"
    "\
╭^─Try a font^─╮^ ^           ╭^─^──────────────╮╭^─^───^─^──────╮
│_d_ DejaVu    ╰^─^───────────╯_i_ Iosevka Comfy││_*_ ^^configure│
│_h_ Hack       _b_ JetBrains  _u_ Unifont      ││_TAB_^^ sh hint│
│_c_ Cascadia   _f_ FreeMono   _a_ Agave        ││_RET_ _q_ exit │
│_j_ JuliaMono  _s_ Source Code Pro^^╭──────────╯╰^─^───^─^──────╯
╰^─^────────────^─^────────────^─^───╯"))
  ("d" (set-frame-font "DejaVu Sans Mono"))
  ("u" (set-frame-font "Unifont"         ))
  ("h" (set-frame-font "Hack"            ))
  ("b" (set-frame-font "JetBrains Mono"  ))
  ("c" (set-frame-font "Cascadia Mono"   ))
  ("a" (set-frame-font "Agave"           ))
  ("j" (set-frame-font "JuliaMono"       ))
  ("f" (set-frame-font "FreeMono"        ))
  ("i" (set-frame-font "Iosevka Comfy Fixed"))
  ("s" (set-frame-font "Source Code Pro" ))
  ("*" uniline-customize-face :exit t)
  ("TAB" uniline-toggle-hydra-hints)
  ("q"   () :exit t)
  ("RET" () :exit t))

(defun uniline--self-insert-+ ()
  "Wrapper over `self-insert-command' <kp-add>."
  (interactive)
  (self-insert-command 1 ?+))

(defun uniline--self-insert-- ()
  "Wrapper over `self-insert-command' <kp-subtract>."
  (interactive)
  (self-insert-command 1 ?-))

(defun uniline--text-direction-str ()
  "Return a textual representation of current text direction."
  (interactive)
  (cond
   ((eq uniline--text-direction (eval-when-compile uniline--direction-up↑)) "↑")
   ((eq uniline--text-direction (eval-when-compile uniline--direction-ri→)) "→")
   ((eq uniline--text-direction (eval-when-compile uniline--direction-dw↓)) "↓")
   ((eq uniline--text-direction (eval-when-compile uniline--direction-lf←)) "←")
   (t " ")))

(defhydra uniline-hydra-arrows
  (:hint nil
   :exit nil)
  ;; Docstring MUST begin with an empty line to benefit from substitutions
  (concat
   (string-replace
    "Text dir────"
    "Text dir─╴%s(uniline--text-direction-str)╶"
  "\
╭^─^─^Insert glyph^─────╮╭^Rotate arrow^╮╭^Text dir────^╮╭^─Contour─^╮╭^─^─^─^─^─^─^─^────────────╮
│_a_,_A_rrow ▷ ▶ → ▹ ▸ ↔││_S-<left>_  ← ││_C-<left>_  ← ││_c_ contour││_-_ _+_ _=_ _#_ self-insert│
│_s_,_S_quare  □ ■ ◇ ◆ ◊││_S-<right>_ → ││_C-<right>_ → ││_C_ ovwrt  ││_f_ ^^^^^^      choose font│
│_o_,_O_-shape · ● ◦ Ø ø││_S-<up>_    ↑ ││_C-<up>_    ↑ │╭^─Fill────^╮│_TAB_   ^^^^^^  short hint │
│_x_,_X_-cross ╳ ÷ × ± ¤││_S-<down>_  ↓ ││_C-<down>_  ↓ ││_i_ fill   ││_q_ _RET_ ^^^^  exit       │
╰^─^─^─^────────────────╯╰^─^───────────╯╰^─^───────────╯╰^─^────────╯╰^─^─^─^─^─^─^─^────────────╯"))
  ("a" uniline-insert-fw-arrow )
  ("A" uniline-insert-bw-arrow )
  ("s" uniline-insert-fw-square)
  ("S" uniline-insert-bw-square)
  ("o" uniline-insert-fw-oshape)
  ("O" uniline-insert-bw-oshape)
  ("x" uniline-insert-fw-cross )
  ("X" uniline-insert-bw-cross )
  ("S-<left>"  uniline-rotate-lf←)
  ("S-<right>" uniline-rotate-ri→)
  ("S-<up>"    uniline-rotate-up↑)
  ("S-<down>"  uniline-rotate-dw↓)
  ("C-<right>" uniline-text-direction-ri→ :exit t)
  ("C-<up>"    uniline-text-direction-up↑ :exit t)
  ("C-<down>"  uniline-text-direction-dw↓ :exit t)
  ("C-<left>"  uniline-text-direction-lf← :exit t)
  ("<kp-subtract>" uniline--self-insert--)
  ("<kp-add>"      uniline--self-insert-+)
  ("-" self-insert-command)
  ("+" self-insert-command)
  ("=" self-insert-command)
  ("#" self-insert-command)
  ("f" uniline-hydra-fonts/body :exit t)
  ("c" uniline-contour          :exit t)
  ("C" (uniline-contour t)      :exit t)
  ("i" (uniline-fill (uniline--choose-fill-char)) :exit t)
  ("q"   ()                     :exit t)
  ("TAB" uniline-toggle-hydra-hints)
  ("RET" ()                     :exit t))

(defun uniline--hydra-rect-undo ()
  "Make undo work outside selection."
  (interactive)
  (deactivate-mark)
  (undo))

(defun uniline--hydra-rect-quit ()
  "Quit this hydra."
  (interactive)
  (deactivate-mark))

(defhydra uniline-hydra-moverect
  (:pre (rectangle-mark-mode 1)
   :hint nil
   :exit nil)
  ;; Docstring MUST begin with an empty line to benefit from substitutions
  "
╭^Move ^rect╮╭────^Draw^ rect────╮╭^─Rect^─╮╭^Brush^╮╭──^Misc^───────╮
│_<left>_  ←││_r_     trace inner││_c_ copy││_-_ ╭─╯││_C-/_ undo     │
│_<right>_ →││_R_     trace outer││_k_ kill││_+_ ┏━┛││_TAB_ sort hint│
│_<up>_    ↑││_C-r_   ovewr inner││_y_ yank││_=_ ╔═╝││_RET_ exit     │
│_<down>_  ↓││_C-S-R_ ovewr outer│╰^^┬─────╯╯_#_ ▄▄▟│╰^───^──────────╯
╰^─────^────╯│_i_     fill       │ ^^│_<delete>_ DEL│
 ^     ^     ╰^────^─────────────╯ ^^╰^────────^────╯"
  ("<left>"  uniline-move-rect-lf←)
  ("<right>" uniline-move-rect-ri→)
  ("<up>"    uniline-move-rect-up↑)
  ("<down>"  uniline-move-rect-dw↓)
  ("r"     uniline-draw-inner-rectangle)
  ("R"     uniline-draw-outer-rectangle)
  ("C-r"   uniline-overwrite-inner-rectangle)
  ("C-S-R" uniline-overwrite-outer-rectangle)
  ("i"     uniline-fill-rectangle)
  ("c"   uniline-copy-rectangle)
  ("k"   uniline-kill-rectangle)
  ("y"   uniline-yank-rectangle)
  ("<delete>"       uniline--set-brush-0)
  ("<deletechar>"   uniline--set-brush-0)
  ("C-<delete>"     uniline--set-brush-0)
  ("C-<deletechar>" uniline--set-brush-0)
  ("-"              uniline--set-brush-1)
  ("<kp-subtract>"  uniline--set-brush-1)
  ("+"              uniline--set-brush-2)
  ("<kp-add>"       uniline--set-brush-2)
  ("="              uniline--set-brush-3)
  ("#"              uniline--set-brush-block)
  ("TAB" uniline-toggle-hydra-hints)
  ("C-/"   uniline--hydra-rect-undo :exit t)
  ("C-_"   uniline--hydra-rect-undo :exit t)
  ("C-x u" uniline--hydra-rect-undo :exit t)
  ("C-x C-x" rectangle-exchange-point-and-mark)
  ("RET"   uniline--hydra-rect-quit :exit t))

(defun uniline-hydra-choose-body ()
  "Choose between two Hydras based on selection.
When selection is active, most likely user wants to act
on a rectangle.
Therefore the rectangle hydra is launched.
Otherwise, the arrows & shapes hydra is invoked."
  (interactive)
  (let ((message-log-max)) ; avoid hint copied in *Messages*
    (if (region-active-p)
        (uniline-hydra-moverect/body)
      (uniline-hydra-arrows/body))))

(defhydra uniline-hydra-macro-exec
  (:hint nil
   :exit nil)
  ;; Docstring MUST begin with an empty line to benefit from substitutions
  "
╭^^Call macro in direction╶^^^^──────╮
│_<right>_ call → │_e_ usual call^^  │
│_<up>_    call ↑ │^ ^ ^   ^         │
│_<down>_  call ↓ │_TAB_^^ short hint│
│_<left>_  call ← │_q_ _RET_ exit    │
╰^^───────────────┴^─^─^───^─────────╯"
  ("e"       (kmacro-end-and-call-macro 1))
  ("<right>" (uniline-call-macro-in-direction uniline--direction-ri→))
  ("<up>"    (uniline-call-macro-in-direction uniline--direction-up↑))
  ("<down>"  (uniline-call-macro-in-direction uniline--direction-dw↓))
  ("<left>"  (uniline-call-macro-in-direction uniline--direction-lf←))
  ("TAB" uniline-toggle-hydra-hints)
  ("q"   () :exit t)
  ("RET" () :exit t))

;;;╭───────────────────╮
;;;│Smaller hydra hints│
;;;╰───────────────────╯
;; toggle between normal hydra hints, and one-liners

(defvar-local uniline-hint-style t
  "Which kind of hint message should the Hydras display?
t: large and detailed messages
1: one-line non-disturbing messages in the echo area
0: no message
Those values are loosely in sync with those defined by the
`:verbosity' Hydra property.")

(eval-when-compile ; not needed at runtime
  (defun uniline--color-hint (hint)
    "Return a colored message mimicking the Hydra way.
HINT is the message string. It  contains pairs of ^xxx^
carets which are to be removed from the message, while the
text within will be colored."
    (interactive)
    (replace-regexp-in-string
     "\\^.*?\\^"
     (lambda (x)
       (setq x (substring x 1 (1- (length x))))
       (add-face-text-property
        0 (length x)
        'hydra-face-red
        nil x)
       x)
     hint
     t)))

;; Pack 2 hints in the usual uniline-hydra-*/hint variables
;; one is the standard hint created by `defhydra'
;; the other is a one-liner
(setq
 uniline-hydra-arrows/hint
 `(if (eq uniline-hint-style t)
      ,uniline-hydra-arrows/hint
    ,(eval-when-compile
       (uniline--color-hint
        "glyph:^aAsSoOxX-+=#^ arr-dir:^S-←→↑↓^ text-dir:^C-←→↑↓^ ^c^-ontour f-^i^-ll ^f^-onts ^TAB^")))
 uniline-hydra-fonts/hint
 `(if (eq uniline-hint-style t)
      ,uniline-hydra-fonts/hint
    ,(eval-when-compile
       (uniline--color-hint
        "choose font: ^dhcjbfsiua^  config font: ^*^  hint: ^TAB^")))
 uniline-hydra-moverect/hint
 `(if (eq uniline-hint-style t)
      ,uniline-hydra-moverect/hint
    ,(eval-when-compile
       (uniline--color-hint
        "move: ^←→↑↓^ trace: ^rR C-rR^  copy-paste: ^cky^  fill: ^i^  brush: ^-+=# DEL^  hint: ^TAB^")))
 uniline-hydra-macro-exec/hint
 `(if (eq uniline-hint-style t)
      ,uniline-hydra-macro-exec/hint
    ,(eval-when-compile
       (uniline--color-hint
        "macro exec usual: ^e^  directional: ^←→↑↓^  hint: ^TAB^"))))

(defun uniline-toggle-hydra-hints (&optional notoggle)
  "Toggle between styles of hydra hints.
When NOTOGGLE is t, do not toggle `uniline-hint-style',
just put everything in sync."
  (interactive)
  (unless notoggle
    (setq uniline-hint-style
          (cond
           ((eq uniline-hint-style t) 1)
           ((eq uniline-hint-style 1) t)
           (t t))))
  (cl-loop
   for hydra in
   '(uniline-hydra-arrows
     uniline-hydra-fonts
     uniline-hydra-moverect
     uniline-hydra-macro-exec)
   do
   (hydra-set-property
    hydra :verbosity uniline-hint-style)))

;;;╭──────────────────╮
;;;│Uniline minor mode│
;;;╰──────────────────╯

(defvar-local uniline--remember-settings
    nil
  "Remember settings before entering uniline minor-mode.
It is a list containing:
  - `overwrite-mode'
  - `indent-tabs-mode'
  - `truncate-lines'
  - `cursor-type'")

(defun uniline--welcome-message ()
  "Display a message giving the main key-bindings of the minor mode."
  (interactive)
  (let ((message-log-max))
    (message
     (cond
      ((eq uniline-hint-style t)
       (eval-when-compile
         (uniline--color-hint
          "\
 ╭─^^────────────╴Uniline╶╴mode╶────────────────────────╮
 │      ^→ ↓ ← ↑^          draw lines with current brush│
 │^Ctrl  → ↓ ← ↑^          overwrite                    │
 │^Shift → ↓ ← ↑^          extend selection             │
 │^- + = # DEL RET^        change brush style           │
 │^INS^ without selection  insert glyphs, change font   │
 │^INS^ with    selection  handle rectangles            │
 │^C-h TAB^                switch small-large hints     │
 │^C-c C-c^                quit uniline                 │
 ╰─^^───────────────────────────────────────────────────╯")))
      ((eq uniline-hint-style 1)
       (eval-when-compile
         (uniline--color-hint
          "trace: ^←→↑↓^  ovwr: ^C-←→↑↓^  selec: ^C-←→↑↓^  brush: ^-+=# DEL RET^  menu: (sel)^INS^  hint: ^C-h TAB^")))
      (t nil)))))

(defun uniline-toggle-hydra-hints-welcome ()
  "Toggle between styles of hydra hints, and display welcome message."
  (interactive)
  (uniline-toggle-hydra-hints)
  (uniline--welcome-message))

(defun uniline--mode-pre ()
  "Change settings when entering uniline mode.
And backup previous settings."
  (setq uniline--remember-settings
        (list
         overwrite-mode
         indent-tabs-mode
         truncate-lines
         cursor-type
         post-self-insert-hook))
  (overwrite-mode 1)
  (indent-tabs-mode 0)
  (setq truncate-lines t)
  (setq cursor-type 'hollow)
  (add-hook
   'post-self-insert-hook
   #'uniline--post-self-insert
   nil 'local)
  (uniline-toggle-hydra-hints t)
  (uniline--welcome-message))

(defun uniline--mode-post ()
  "Restore settings when exiting uniline mode."
  (overwrite-mode    (if (nth 0 uniline--remember-settings) 1 0))
  (indent-tabs-mode  (if (nth 1 uniline--remember-settings) 1 0))
  (setq
   truncate-lines        (nth 2 uniline--remember-settings)
   cursor-type           (nth 3 uniline--remember-settings)
   post-self-insert-hook (nth 4 uniline--remember-settings)))

(defun uniline--mode-line ()
  "Computes the string which appears in the mode-line."
  (format
   " %cUniline%c"
   (cond
    ((null uniline--text-direction) ? )
    ((eq uniline--text-direction uniline--direction-up↑) ?↑)
    ((eq uniline--text-direction uniline--direction-ri→) ?→)
    ((eq uniline--text-direction uniline--direction-dw↓) ?↓)
    ((eq uniline--text-direction uniline--direction-lf←) ?←))
   (pcase uniline--brush
     ('nil   ? )
     (0      ?/)
     (1      ?┼)
     (2      ?╋)
     (3      ?╬)
     (:block ?▞))))

;; This `unintern' instruction is useful during development
;; to ensure that M-x eval-buffer reloads 100% of the Lisp code
;; (unintern 'uniline-mode-map nil)

;;;###autoload
(define-minor-mode uniline-mode
  "Minor mode to draw lines, boxes, & arrows using UNICODE characters.

                ┏━━━━━━━┓
        ┏━━━━◀━━┫ thick ┣═◁═╗
    ╭───┸──╮    ┃ box   ┃   ║
    │ thin │    ┗━━┯━━━━┛   ║
    │ box  ├───●───╯ ╔══════╩═╗
    ╰───┬──╯   ╰─────╢ double ║
        ╰───────▷────╢ box    ║
                     ╚════════╝
     here╶──────────────╮
                        ↓
     △ ╭────────╮ ┏━━━━━┷━━━━━┓
   A │ │ A+X    │ ┃ A+Y       ┃
     │ │        │ ┃           ┃
     ▽ ╰────────╯ ┗━━━━━━━━━━━┛
     △ ╭────────╮ ╭───────────╮
   B │ │ B+X    │ │ B+Y       │
     ▽ ╰────────╯ ╰───────────╯
       ◀━━━━━━━━▶◀━━━━━━━━━━━━▶
           X           Y

    v △      ▗▖         ▗
    a │   ▗▟▄▟██▖   ▗▄▄▟█
    l │ ▐▄███████▄ ▟█████▙ ▄▖
    u─┴╴▝▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    e   ├───time────────────▷

╭─Keyboard arrows────────────╴
│ Use keyboard arrows to draw lines ╭─┲━╦═╗
│ Use control-arrows to overwrite whatever was there
│ Use shift-arrows to extend the selection (or start a selection)
╰────────────────────────────╴
╭─Brush style────────────────╴\\<uniline-mode-map>
│ \\[uniline--set-brush-1]	for thin   lines	╭─┬─╮
│ \\[uniline--set-brush-2]	for thick  lines	┏━┳━┓
│ \\[uniline--set-brush-3]	for double lines	╔═╦═╗
│ \\[uniline--set-brush-block]	for blocks		▙▄▟▀
│ \\[uniline--set-brush-0]	to erase lines
│ \\[uniline--set-brush-nil]	to move cursor without drawing
╰────────────────────────────╴
╭─Glyphs (region inactive)───╴\\<uniline-mode-map>
│ \\[uniline-hydra-choose-body] when there is NO region highlighted,
│ enter a sub-mode to draw a single character glyph,
│ and change its orientation.
├─Intersection glyphs────────╴\\<uniline-hydra-arrows/keymap>
│ \\[uniline-hydra-arrows/uniline-insert-fw-arrow]	arrows ▷ ▶ → ▹ ▸ ↔
│ \\[uniline-hydra-arrows/uniline-insert-fw-square]	squares  □ ■ ◇ ◆ ◊
│ \\[uniline-hydra-arrows/uniline-insert-fw-oshape]	circles  · ● ◦ Ø ø
│ \\[uniline-hydra-arrows/uniline-insert-fw-cross]	crosses  ╳ ÷ × ± ¤
│ Shifting the key cycles backward
├─Arrow direction────────────╴
│ \\[uniline-hydra-arrows/uniline-rotate-lf←]	point arrow ← left
│ \\[uniline-hydra-arrows/uniline-rotate-ri→]	point arrow → right
│ \\[uniline-hydra-arrows/uniline-rotate-up↑]	point arrow ↑ up
│ \\[uniline-hydra-arrows/uniline-rotate-dw↓]	point arrow ↓ down
├─Text direction─────────────╴
│ Usually when typing text, cursor moves to the right.
│ \\[uniline-hydra-arrows/uniline-text-direction-up↑-and-exit]	text goes up   ↑
│ \\[uniline-hydra-arrows/uniline-text-direction-ri→-and-exit]	text goes right→
│ \\[uniline-hydra-arrows/uniline-text-direction-dw↓-and-exit]	text goes down ↓
│ \\[uniline-hydra-arrows/uniline-text-direction-lf←-and-exit]	text goes left ←
├─Insert characters──────────╴
│ In this sub-mode, the keys `- + = #' recover their
│ basic meaning, which is to insert this character.
├─Other──────────────────────╴
│ \\[uniline-hydra-arrows/uniline-hydra-fonts/body-and-exit] enter the fonts sub-menu
│ \\[uniline-hydra-arrows/nil] (or q) exits the sub-mode
│ Any other key exits the sub-mode and do whatever they
│ are intended for.
╰────────────────────────────╴
╭─Rectangles (region active)─╴\\<uniline-mode-map>
│ \\[uniline-hydra-choose-body] when region IS highlighted,
│ enter a sub-mode to handle rectangles,
│ marked by the highlighted region.
├─Move rectangle─────────────╴\\<uniline-hydra-moverect/keymap>
│ \\[uniline-hydra-moverect/uniline-move-rect-lf←]	move the rectangle ←
│ \\[uniline-hydra-moverect/uniline-move-rect-ri→]	move the rectangle →
│ \\[uniline-hydra-moverect/uniline-move-rect-up↑]		move the rectangle ↑
│ \\[uniline-hydra-moverect/uniline-move-rect-dw↓]	move the rectangle ↓
├─Draw rectangle─────────────╴
│ \\[uniline-hydra-moverect/uniline-draw-inner-rectangle]	draw      an inner rectangle
│ \\[uniline-hydra-moverect/uniline-draw-outer-rectangle]	draw      an outer rectangle
│ \\[uniline-hydra-moverect/uniline-overwrite-inner-rectangle]	overwrite an inner rectangle
│ \\[uniline-hydra-moverect/uniline-overwrite-outer-rectangle]	overwrite an outer rectangle
├─Fill───────────────────────╴
│ \\[uniline-hydra-moverect/uniline-fill-rectangle]	fill region with a character
├─Other──────────────────────╴
│ \\[uniline-hydra-moverect/uniline--hydra-rect-undo-and-exit] undo works outside selection
│ \\[uniline-hydra-moverect/uniline--hydra-rect-quit-and-exit] exit the rectangle sub-mode
│ Any other key exits the sub-mode and do whatever they
│ are intended for.
╰────────────────────────────╴
╭╴Macros─────────────────────╴\\<uniline-mode-map>
│ Usual Emacs macros recording works as usual
│ Last keybord macro can be twisted in any of the 4 directions
│ \\[uniline-hydra-macro-exec/body] then → ↓ ← ↑ : directional call of last keyboard macro
╰────────────────────────────╴
╭─Fonts──────────────────────╴
│ Try out some mono-spaced fonts with support for the
│ required UNICODE characters.
│ \\<uniline-mode-map>\\[uniline-hydra-choose-body] \\<uniline-hydra-arrows/keymap>\\[uniline-hydra-arrows/uniline-hydra-fonts/body-and-exit] enters a sub-menu to change the font
│ type the first letter of the font name.
│ This setting is just for the current Emacs session.\\<uniline-hydra-fonts/keymap>
│ \\[uniline-hydra-fonts/uniline-customize-face-and-exit] customize default font for future sessions.
╰────────────────────────────╴
╭─Toggle hint sizes──────────╴
│ \\<uniline-mode-map>\\[uniline-toggle-hydra-hints-welcome] in base uniline mode
│ \\<uniline-hydra-arrows/keymap>\\[uniline-hydra-arrows/uniline-toggle-hydra-hints] in a INS-activated menu
╰────────────────────────────╴
╭─Quit───────────────────────╴\\<uniline-mode-map>
│ \\[uniline-mode] quit the uniline minor mode.
│ the state of the buffer (ex: `overwrite-mode') will return to
│ what it was prior to entering `uniline-mode'
╰────────────────────────────╴"
  :init-value nil
  :lighter (:eval (uniline--mode-line))
  :keymap
  `(
    ([right]   . uniline-write-ri→)
    ([down ]   . uniline-write-dw↓)
    ([left ]   . uniline-write-lf←)
    ([up   ]   . uniline-write-up↑)
    ([C-right] . uniline-overwrite-ri→)
    ([C-down ] . uniline-overwrite-dw↓)
    ([C-left ] . uniline-overwrite-lf←)
    ([C-up   ] . uniline-overwrite-up↑)
    ([insert]      . uniline-hydra-choose-body)
    ([insertchar]  . uniline-hydra-choose-body)
    ([?\r]           . uniline--set-brush-nil)
    ([delete]        . uniline--set-brush-0)
    ([deletechar]    . uniline--set-brush-0)
    ("-"             . uniline--set-brush-1)
    ([kp-subtract]   . uniline--set-brush-1)
    ("+"             . uniline--set-brush-2)
    ([kp-add]        . uniline--set-brush-2)
    ("="             . uniline--set-brush-3)
    ("#"             . uniline--set-brush-block)
    ([?\C-x ?e]      . uniline-hydra-macro-exec/body)
    ([?\C-h ?\t]     . uniline-toggle-hydra-hints-welcome)
    ([?\C-c ?\C-c]   . uniline-mode))
  :after-hook (if uniline-mode (uniline--mode-pre) (uniline--mode-post)))

(easy-menu-define
  uniline-menu
  uniline-mode-map
  "Uniline mode menu."
  '("Uniline"
    :visible t
    :active t
    ["write right"     uniline-write-ri→     t]
    ["write left"      uniline-write-lf←     t]
    ["write up"        uniline-write-up↑     t]
    ["write down"      uniline-write-dw↓     t]
    "----"
    ["overwrite right" uniline-overwrite-ri→ t]
    ["overwrite left"  uniline-overwrite-lf← t]
    ["overwrite up"    uniline-overwrite-up↑ t]
    ["overwrite down"  uniline-overwrite-dw↓ t]
    "----"
    ["─ light brush"   uniline--set-brush-1     :style radio :selected (eq uniline--brush 1     )]
    ["━ heavy brush"   uniline--set-brush-2     :style radio :selected (eq uniline--brush 2     )]
    ["═ double brush"  uniline--set-brush-3     :style radio :selected (eq uniline--brush 3     )]
    ["▞ blocks brush"  uniline--set-brush-block :style radio :selected (eq uniline--brush :block)]
    ["eraser brush"    uniline--set-brush-0     :style radio :selected (eq uniline--brush 0     )]
    ["inactive brush"  uniline--set-brush-nil   :style radio :selected (eq uniline--brush nil   )]
    "----"
    ("Insert glyph"
     ["insert arrow ▷ ▶ → ▹ ▸ ↔" uniline-insert-fw-arrow  :keys "INS a"]
     ["insert square □ ■ ◇ ◆ ◊"  uniline-insert-fw-square :keys "INS s"]
     ["insert oshape · ● ◦ Ø ø"  uniline-insert-fw-oshape :keys "INS o"]
     ["insert cross ╳ ÷ × ± ¤"   uniline-insert-fw-cross  :keys "INS x"]
     ["rotate arrow → right"     uniline-rotate-ri→ :keys "INS S-<right>"]
     ["rotate arrow ↑ up"        uniline-rotate-up↑ :keys "INS S-<up>   "]
     ["rotate arrow ← left"      uniline-rotate-lf← :keys "INS S-<left> "]
     ["rotate arrow ↓ down"      uniline-rotate-dw↓ :keys "INS S-<down> "])
    ("Rectangular region" :active (region-active-p)
     ["move selection right" uniline-move-rect-ri→ :keys "INS <right>"]
     ["move selection left"  uniline-move-rect-lf← :keys "INS <left> "]
     ["move selection up"    uniline-move-rect-up↑ :keys "INS <up>   "]
     ["move selection down"  uniline-move-rect-dw↓ :keys "INS <down> "]
     ["trace rectangle inside selection" uniline-draw-inner-rectangle :keys "INS r"]
     ["trace rectangle around selection" uniline-draw-outer-rectangle :keys "INS R"]
     ["overwrite rectangle inside selection" uniline-overwrite-inner-rectangle :keys "INS C-r"]
     ["overwrite rectangle around selection" uniline-overwrite-outer-rectangle :keys "INS C-R"]
     ["fill" uniline-fill-rectangle :keys "INS i"])
    ("Fill & contour"
     ["contour" uniline-contour]
     ["contour overw" (uniline-contour t)]
     ["fill" uniline-fill])
    ("Text insertion direction"
     ["→ right" uniline-text-direction-ri→ :keys "INS C-<right>" :style radio :selected (eq uniline--text-direction uniline--direction-ri→)]
     ["↑ up"    uniline-text-direction-up↑ :keys "INS C-<up>   " :style radio :selected (eq uniline--text-direction uniline--direction-up↑)]
     ["← left"  uniline-text-direction-lf← :keys "INS C-<left> " :style radio :selected (eq uniline--text-direction uniline--direction-lf←)]
     ["↓ down"  uniline-text-direction-dw↓ :keys "INS C-<down> " :style radio :selected (eq uniline--text-direction uniline--direction-dw↓)])
    "----"
    ["large hints sizes" uniline-toggle-hydra-hints :keys "TAB or C-h TAB" :style toggle :selected (eq uniline-hint-style t)]
    ("Font"
     ["DejaVu Sans Mono"    (set-frame-font "DejaVu Sans Mono"   ) :keys "INS f d" :style radio :selected (uniline--is-font ?d)]
     ["Hack"                (set-frame-font "Hack"               ) :keys "INS f h" :style radio :selected (uniline--is-font ?h)]
     ["Cascadia Mono"       (set-frame-font "Cascadia Mono"      ) :keys "INS f c" :style radio :selected (uniline--is-font ?c)]
     ["JuliaMono"           (set-frame-font "JuliaMono"          ) :keys "INS f j" :style radio :selected (uniline--is-font ?j)]
     ["JetBrains Mono"      (set-frame-font "JetBrains Mono"     ) :keys "INS f b" :style radio :selected (uniline--is-font ?b)]
     ["FreeMono"            (set-frame-font "FreeMono"           ) :keys "INS f f" :style radio :selected (uniline--is-font ?f)]
     ["Source Code Pro"     (set-frame-font "Source Code Pro"    ) :keys "INS f s" :style radio :selected (uniline--is-font ?s)]
     ["Iosevka Comfy Fixed" (set-frame-font "Iosevka Comfy Fixed") :keys "INS f i" :style radio :selected (uniline--is-font ?i)]
     ["Unifont"             (set-frame-font "Unifont"            ) :keys "INS f u" :style radio :selected (uniline--is-font ?u)]
     ["Agave"               (set-frame-font "Agave"              ) :keys "INS f a" :style radio :selected (uniline--is-font ?a)]
     ["permanently configure" uniline-customize-face               :keys "INS f *"])
    ["quit Uniline Mode" uniline-mode t] ))

(provide 'uniline)
;;; uniline.el ends here
