;;; uniline.el --- Add UNICODE based diagrams to text files -*- coding:utf-8; lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Thierry Banel

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
;; If the compiler sees `dir' as `uniline-direction-dw↓',
;; for example, then it can fold this expression to just 48,
;; which is nice©

;; We call four times a (defmacro) with hard-coded directions.
;; The hard-coded parameter must go all the way down
;; to the last instructions needing that direction.
;; So we have (defmacro) calling other (defmacro).

;; What is the purpose `eval-when-compile'?
;; It folds down a numerical expression to just one number
;; for instance
;;   (eval-when-compile (ash 3 (* 2 uniline-direction-lf←)))
;; is converted to just 192
;; This happens both in interpreted and byte-compiled code
;; Otherwise the operations ash, multiplication,
;; and retrieval from `uniline-direction-lf←'
;; would be computed over and over at runtime,
;; with always the same 192 result.
;; We could put directly 192 in the source code,
;; but this would defeat maintenance and readability.

;; What is the purpose `eval-and-compile'?
;; The byte-compiler expands all defmacro' called in defun's.
;; In turn, those defmacro's need to access some defconst's,
;; notably `uniline-direction-up↑' and sisters.
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
  (defconst uniline-direction-up↑ 0) (defmacro uniline-direction-up↑ () 0)
  (defconst uniline-direction-ri→ 1) (defmacro uniline-direction-ri→ () 1)
  (defconst uniline-direction-dw↓ 2) (defmacro uniline-direction-dw↓ () 2)
  (defconst uniline-direction-lf← 3) (defmacro uniline-direction-lf← () 3))
;;                  △       △                          △
;; usual constant╶──╯       ╰──────────────────────╮   │
;; when we insist in having a numeric byte-code:   │   │
;;   constant 2╶───────────────────────────────────╯   │
;;   varref uniline-direction-dw↓╶─────────────────────╯

(eval-when-compile ; not needed at runtime

  (defmacro uniline--switch-with-cond (dir &rest body)
    "Macro to simplify `cond' applied to possible values of DIR.
BODY is ((CASE1 EXPR1...) (CASE2 EXPR2...) ...)
DIR is an expression which is compared to each of the CASE1, CASE2, ...
until it matches one. Then the corresponding EXPRN is executed.
It expands to a (cond) form.
It is somehow similar to `cl-case', except that the cases are evaluated
while `cl-case' quotes them."
    (declare (indent 1))
    `(cond
      ,@(cl-loop
         for c in body
         collect
         `((eq ,dir (eval-when-compile ,(car c)))
           ,@(cdr c)))
      (t (error "direction not known"))))

  (defmacro uniline--switch-with-table (dir &rest body)
    "Macro to return a value by looking in a table at the DIR entry.
BODY is ((CASE1 EXPR1) (CASE2 EXPR2) ...)
It is similar to `uniline--switch-with-cond', but instead of executing
an EXPRN, it just returns it as is.
It is expanded into a lookup into a vector or a plist, depending on the CASES.
It it faster than an equivalent (cond) form."
    (declare (indent 1))
    (let* ((lambda
            (if (eq (caar body) 'lambda)
                (pop body)))
           (max
            (cl-loop
             for c in body
             for e = (eval (car c))
             always (and (fixnump e) (<= 0 e 30))
             maximize e)))
      (if max
          ;; create a vector for fast lookup
          (let ((vec (make-vector (1+ max) nil)))
            (cl-loop
             for c in body
             do (aset
                 vec
                 (eval (car c))
                 (if lambda
                     (funcall lambda (eval (car c)))
                   (eval (cadr c)))))
            `(aref ,vec ,dir))
        ;; create a plist for versatile lookup
        `(plist-get
          ',(cl-loop
             for c in body
             collect (eval (car c))
             collect (if lambda
                         (funcall lambda (eval (car c)))
                       (eval (cadr c))))
          ,dir)))))

(eval-when-compile ; not needed at runtime
  (defsubst uniline--reverse-direction (dir)
    "Reverse DIR.
DIR is any of the 4 `uniline-direction-*'.
Exchange left with right, up with down."
    (% (+ 2 dir) 4))

  (defsubst uniline--turn-right (dir)
    "Return DIR turned 90° clockwise.
DIR & returned values are in [0,1,2,3]."
    (% (1+ dir) 4))

  (defsubst uniline--turn-left (dir)
    "Return DIR turned 90° anti-clockwise.
DIR & returned values are in [0,1,2,3]."
    (% (+ 3 dir) 4)))

(defsubst uniline-move-to-column (x)
  "Move to column X staying on the same line.
Add blanks if line is too short.
Move to 0 if X negative."
  (move-to-column (max x 0) t))

(defun uniline-move-to-delta-column (x)
  "Move X characters, staying on the same line.
Add blanks if line is too short.
X may be negative to move backward.
Move to 0 if target is beyond the left border of buffer."
  (uniline-move-to-column (+ (current-column) x)))

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

(defun uniline-move-to-line (y)
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

(defun uniline-move-to-delta-line (y)
  "Move Y lines while staying on the same column.
Create blank lines at the end of the buffer if needed,
or blank characters at the end of target line.
Y may be negative to move backward."
  (move-to-column
   (prog1
       (current-column)
     (uniline--forward-line-force y))
   t))

(defun uniline-move-to-lin-col (y x)
  "Move to line Y and column X.
Create blank lines at the end of the buffer if needed,
or blank characters at the end of target line if needed.
Y=0 means first line of buffer.
X=0 means first column of buffer."
  (goto-char (point-min))
  (uniline--forward-line-force y)
  (uniline-move-to-column x))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--move-in-direction (dir &optional nb)
    "Move NB char in direction DIR.
NB defaults to 1.
This is a macro, therefore it is as if writing
directly (uniline-move-to-delta-line -1) and such,
with no overhead."
    (declare (debug (form)))
    (unless nb (setq nb 1))
    (let ((mnb (if (fixnump nb) (- nb) `(- ,nb))))
      (uniline--switch-with-cond dir
        (uniline-direction-up↑ `(uniline-move-to-delta-line   ,mnb))
        (uniline-direction-ri→ `(uniline-move-to-delta-column , nb))
        (uniline-direction-dw↓ `(uniline-move-to-delta-line   , nb))
        (uniline-direction-lf← `(uniline-move-to-delta-column ,mnb))))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--at-border-p (dir)
    "Test if at a non-trespass-able border of buffer.
This happens at the first line or at the first column,
when trying to go further when DIR is up or left:
`uniline-direction-up↑' or `uniline-direction-lf←'.
In the bottom & right directions the buffer is infinite."
    (declare (debug (form)))
    (setq dir (eval dir))
    (uniline--switch-with-table dir
      (uniline-direction-up↑ '(eq (pos-bol) 1))
      (uniline-direction-ri→ nil)
      (uniline-direction-dw↓ nil)
      (uniline-direction-lf← '(bolp)))))

(defun uniline--char-after (&optional point)
  "Same as `char-after', except for right and bottom edges of buffer.
Outside the buffer, return a blank character.
POINT default to `(point)', as for `char-after'"
  (let ((c (char-after point)))
    (if (or (not c) (eq c ?\n))
        ?  ;; eol & eob are considered blank
      c)))

;;;╭───────────────────╮
;;;│Perfect hash tables│
;;;╰───────────────────╯

;; When an hash-table is constant, with entries known at compile-time,
;; then a perfect hash-table can be built. Perfect means that there
;; are no collisions: each bucket contains one entry, or none, never
;; two entries. This makes the hash-table very efficient.
;; The usual multiple entries scanning can be entirely bypassed.
;;
;; Uniline has 6 constant hash-tables. We want to make them
;; perfect, collision-less. 3 Options:
;;
;; 1- Define a custom hash-function like
;;    (% key 191)
;;    by looking for the right constant like 191, we can get a
;;    collision-less hash-table
;;    as shown by (internal--hash-table-histogram)
;;  - Unfortunately, this makes the resulting hash-table roughly
;;    3 times slower than a regular (make-hash-table), even one
;;    with collisions.
;;
;; 2- Use a custom vector instead of a hash-table, along with a
;;    hashing function like
;;    (% key (length custom-vector))
;;    This amounts to re-creating hash-tables with vectors.
;;  - The result is slightly slower than (make-hash-table), even
;;    with collisions, about 30% slower.
;;
;; 3- Use a standard hash-table and tweak its size
;;    (make-hash-table :size 114)
;;  - Unfortunately, the specified size is not retrained in the
;;    compiled *.elc file
;;    Beside, the specified size gets rounded to the next power of 2
;;
;; 4- Create the constant hash-table with
;;    (make-hash-table :size 114)
;;    when loading the compiled *.elc file
;;    Store in the *.elc a list of pairs (key . value)
;;  - This bypasses the #s(hash-table …) form that was handy in the
;;    *.elc, but at no cost in the *.elc size and almost nothing
;;    for the hash-table creation process at load-time
;;
;; This 4th solution is implemented hereafter through a macro
;; which bundles:
;;   - (defconst …)
;;   - (make-hash-table :size my-preferred-size)
;;   - populating the hash-table from a list of pairs loaded from *.elc

(eval-when-compile ;; not needed at runtime
  (defmacro uniline--defconst-hash-table (name pairs size test doc)
    "Bundles construction of a constant hash-table in one place.
NAME is the name of the hash-table to be passed to `defconst'
PAIRS is a list of (key . value) pairs to populate the table
SIZE is the desired number of buckets
TEST is the comparison function between 2 keys, like `eq' or `equal'"
    `(defconst ,name
       (let ((table (make-hash-table :size ,size :test ,test)))
         (dolist (pair ,pairs)
           (puthash (car pair) (cdr pair) table))
         table)
       ,doc)))

;;;╭─────────────────────────────────────────────────────╮
;;;│Reference tables of ┼ 4 half-lines UNICODE characters│
;;;╰─────────────────────────────────────────────────────╯

;; Hereafter `4halfs' means a representation of a UNICODE character
;; made of half-lines, like ┖ or ┶, as a single number.
;;  ←← ↓↓ →→ ↑↑   directions
;;  □□ □□ □□ ■■
;;  76 54 32 10   bits
;; There are 4 half-lines, one in each direction
;; (north, east, west, south).  Each half line may have one of
;; 4 styles:
;;         ╭────────╴style of line
;;         │   ╭┬────two bits
;;         ▼   ▼▼
;; blank  ( )  □□
;; thin   (─)  □■
;; thick  (━)  ■□
;; double (═)  ■■
;; So a `4halfs' number goes from 0 to 4x4x4x4 = 256.
;; This representation is great because it is easily handled by
;; the bits manipulation primitives: `logior', `logand', `ash'.

(eval-when-compile ; not needed at runtime
  (defmacro uniline--shift-4half (4half dir)
    "Shift 4HALF bits in DIR direction.
4HALF is a number made of 2 bits, in the range [0..3]
  0: no line
  1: thin line
  2: thick line
  3: double line
DIR is 1 of 4 directions:
  0: uniline-direction-up↑
  1: uniline-direction-ri→
  2: uniline-direction-dw↓
  3: uniline-direction-lf←
The result is the bit pattern 4HALF << (2*DIR)"
    `(ash ,4half
          ,(if (fixnump dir)
               (* 2 dir)
             `(* 2 ,dir))))

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
                           ▲ ▲ ▲ ▲
thick=2 upward╶────────────╯ ╰─┴─┴──╮
and thin=1 in the other directions╶─╯
The parameter URLD is a list of 4 numbers in [0..3]
for the 4 directions.
A single number encoding all possible combinations has a
range of [0..256).  It is handy to index vectors rather than
4 dimensions matrices."
    (logior
     (uniline--shift-4half (car    urld) (uniline-direction-up↑))
     (uniline--shift-4half (cadr   urld) (uniline-direction-ri→))
     (uniline--shift-4half (caddr  urld) (uniline-direction-dw↓))
     (uniline--shift-4half (cadddr urld) (uniline-direction-lf←))))

  (defun uniline--unpack-4halfs (4halfs)
    (list
     (logand (ash 4halfs (* -2 uniline-direction-up↑)) 3)
     (logand (ash 4halfs (* -2 uniline-direction-ri→)) 3)
     (logand (ash 4halfs (* -2 uniline-direction-dw↓)) 3)
     (logand (ash 4halfs (* -2 uniline-direction-lf←)) 3))))

(eval-when-compile ; not used at runtime
  (defconst uniline--list-of-available-halflines
    '(;;╭───────────────unicode char
      ;;│  ╭─┬─┬─┬──────4half description
      ;;▽  ▽ ▽ ▽ ▽
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
      ( ?┆ 1 0 1 0 )
      ( ?┊ 1 0 1 0 )
      ( ?│ 1 0 1 0 ) ;; prefer plain lines
      ( ?┇ 2 0 2 0 )
      ( ?┋ 2 0 2 0 )
      ( ?┃ 2 0 2 0 ) ;; prefer plain lines
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
      ( ?┄ 0 1 0 1 )
      ( ?┈ 0 1 0 1 )
      ( ?╌ 0 1 0 1 )
      ( ?─ 0 1 0 1 ) ;; prefer plain lines
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
      ( ?┉ 0 2 0 2 )
      ( ?┅ 0 2 0 2 )
      ( ?╍ 0 2 0 2 )
      ( ?━ 0 2 0 2 ) ;; prefer plain lines
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
    '(;;   ╭─missing ╭─┬─replacement
      ;;   │         │ ╰───────╮
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

(eval-and-compile
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
which in turn is converted to ┕."))

(eval-and-compile
  (uniline--defconst-hash-table
   uniline--char-to-4halfs
   (eval-when-compile
     (cl-loop
      for x in uniline--list-of-available-halflines
      collect
      (cons (car x)
            (uniline--pack-4halfs (cdr x)))))
   255 'eq
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
without the fall-back characters."))

;; is it collision-less?
;; (internal--hash-table-histogram uniline--char-to-4halfs)

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

  (defun uniline--make-glyph-hash (list)
    "Helper function to build `uniline--glyphs-reverse-hash-*'.
Used only at package initialization.
LIST is `uniline--glyphs-fbw'."
    (let ((pairs ()))
      (cl-loop
       for ll on list
       do
       (if (cddar ll)
           ;; glyph is directional, like ▲ ▶ ▼ ◀
           (cl-loop
            for cc in (cdar ll)
            for i from 0
            do (push
                (cons
                 cc
                 (cons
                  (if (uniline--duplicate (car ll))
                      t      ; special case ↕↔↕↔ is NOT fully directional
                    i)       ; fully directional, i gives the direction
                  ll))
                pairs))
         ;; glyph is not directional, like ■ ● ◆
         (push (cons (cadar ll) (cons nil ll)) pairs))
       ;; explicitly break out of circular list
       if (eq (cdr ll) list)
       return nil)
      pairs)))

(uniline--defconst-hash-table
 uniline--glyphs-reverse-hash-fw
 (eval-when-compile
   (uniline--make-glyph-hash uniline--glyphs-fw))
 127 'eq
 "Same as `uniline--glyphs-fw' reversing keys & values.")

(uniline--defconst-hash-table
 uniline--glyphs-reverse-hash-bw
 (eval-when-compile
   (uniline--make-glyph-hash uniline--glyphs-bw))
 127 'eq
 "Same as `uniline--glyphs-bw' reversing keys & values.")

;; are they collision-less?
;; (internal--hash-table-histogram uniline--glyphs-reverse-hash-fw)
;; (internal--hash-table-histogram uniline--glyphs-reverse-hash-bw)

;;;╭───────────────────────────────────────────────────────────╮
;;;│Reference tables of ▙▄▟▀ quadrant-blocks UNICODE characters│
;;;╰───────────────────────────────────────────────────────────╯

;; Hereafter `4quadb' means a representation of a quadrant-block
;; UNICODE character as a single number.  This number must hold
;; all combinations of the 4 quarter-of-a-blocks.
;; Each of the 4 quarters may be present or absent.
;; Therfore `4quadb' is a number from 0 to 2x2x2x2 = 16.
;; Hereafter, the arbitrary choosen bits allocation is as follow:
;;
;;  2^1: here──→───╮
;;               ╭─┴╮
;;  2^0: here──→─┤▘▝│
;;  2^2: here──→─┤▖▗│
;;               ╰─┬╯
;;  2^3: here──→───╯
;;
;; For instance, the character ▚ is made of two quarter blocks
;;  - one in the up-left corner           → constant 2^0
;;  - the other in the down-right corner  → constant 2^3
;; The position in the `uniline--4quadb-to-char' of ▚
;; will be 2^0 + 2^3 = 9

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
The order of characters in this table is not important,
provided that a particular quarter is always represented
by the same bit, and this bit is 1 when this quarter is black.
Everything in the code hereafter follow the choosen ordering
of this table."))

(eval-and-compile
  (uniline--defconst-hash-table
   uniline--char-to-4quadb1
   (eval-when-compile
     (cl-loop
      for c across uniline--4quadb-to-char
      for i from 0
      collect (cons c i)))
   63 'eq
   "Convert a UNICODE character to a quadrant bitmap.
Reverse of `uniline--4quadb-to-char'"))

;; is it collision-less?
;; (internal--hash-table-histogram uniline--char-to-4quadb1)

(eval-and-compile
  (defmacro uniline--char-to-4quadb (char)
    "Return a bit pattern (a 4quadb).
It represents a UNICODE character like ?▙ in CHAR.
Return nil if CHAR is not a 4quadb character."
    (if (fixnump char)
        (gethash  char uniline--char-to-4quadb1)
      `( gethash ,char uniline--char-to-4quadb1))))

(eval-and-compile
  (defconst uniline--4quadb-pushed
    (eval-when-compile
      (let ((table (make-vector 4 nil))) ;        ╭─╴fill with zero because many
        (cl-loop for i from 0 to 3 ;        ▽  entries will be zero anyway
                 do (aset table i (make-vector 16 0)))
        (cl-flet
            ((fill-dir (subtable &rest keyvals)
               ;; first seed the TABLE entries for single-bit quadrant blocks
               ;; and what they become when pushed in DIR direction
               (cl-loop for (k v) on keyvals by #'cddr
                        do
                        (aset subtable
                              (uniline--char-to-4quadb k)
                              (uniline--char-to-4quadb v)))
               ;; then fill in entries for all 16 quadrant blocks, by logically
               ;; composing their bits from single-bits
               (cl-loop
                for i from 0 to 15
                do                  ;  ╭╴consider each of the 4 bits
                (aset subtable      ;  │ and if bit=1, get entry╶────╮
                      i             ;  ╰──────╮                      │
                      (logior       ;         ▽                      ▽
                       (if (eq (logand i 1) 0) 0 (aref subtable 1))
                       (if (eq (logand i 2) 0) 0 (aref subtable 2))
                       (if (eq (logand i 4) 0) 0 (aref subtable 4))
                       (if (eq (logand i 8) 0) 0 (aref subtable 8)))))))
          (fill-dir (aref table uniline-direction-up↑)
                    ?▖ ?▘  ?▗ ?▝)
          (fill-dir (aref table uniline-direction-ri→)
                    ?▘ ?▝  ?▖ ?▗)
          (fill-dir (aref table uniline-direction-dw↓)
                    ?▘ ?▖  ?▝ ?▗)
          (fill-dir (aref table uniline-direction-lf←)
                    ?▝ ?▘  ?▗ ?▖))
        table))
    "For each of the 16 quadrant blocks, this table tells what it becomes
when pushed half-a-char-width in all 4 directions.
For instance [▞] pushed right→ becomes [▗], pushed up↑ becomes [▘]
Access it with this snippet:
(uniline--4quadb-pushed dir 4quadb)")

  (defmacro uniline--4quadb-pushed (dir 4quadb)
    "Accessor to the `uniline--4quadb-pushed' array.
Folds to a single number if DIR & 4QUADB are themselves numbers."
    (if (and (fixnump dir)
             (fixnump 4quadb))
        (aref (aref uniline--4quadb-pushed  dir)  4quadb)
      ` (aref (aref uniline--4quadb-pushed ,dir) ,4quadb))))

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
  (insert char)
  (or (eolp) (delete-char 1))
  (backward-char))

;;;╭────────────────────────────────────────────────────────╮
;;;│Low level management of ┏╾╯half-lines UNICODE characters│
;;;╰────────────────────────────────────────────────────────╯

(defvar-local uniline-brush 1
  "Controls the style of line.
Possible values are as follows:
nil: no action except cursor movements
  0: erase,
  1: simple line     like ╰─┤
  2: thick  line     like ┗━┫
  3: double line     like ╚═╣
:block block drawing like ▙▄▟▀")

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

(defvar-local uniline--which-quadrant (uniline--char-to-4quadb ?▘)
  "Where is the quadrant cursor.
To draw lines with quadrant-blocks like this ▙▄▟▀,
it is required to keep track where is the
quadrant-cursor.  It can be at 4 sub-locations:
north-east, south-west, etc.")

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
          (uniline--char-to-4quadb (uniline--char-after))
          (and force 0))))
    (if bits
        (uniline--insert-4quadb
         (logior bits uniline--which-quadrant)))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--clear-two-4quadb (dir)
    "Helper function to clear half a quadrant-block at point.
Assume that point is on a quadrant-block character.
Clear the half of this character pointing in DIR direction."
    (setq dir (eval dir))
    `(let ((bits (uniline--char-to-4quadb (uniline--char-after))))
       (if bits
           (uniline--insert-4quadb
            (logand
             bits
             ,(uniline--4quadb-pushed
               dir
               (uniline--char-to-4quadb ?█) ; this is constant 15 = 0b1111
               )))))))

;;;╭────────────────────────────╮
;;;│Test blanks in the neighbour│
;;;╰────────────────────────────╯

(defun uniline--blank-after (p)
  "Return non-nil if P points to a 4halfs or 4quadb character.
This includes
- a blank
- a 4half drawing character like ┴ ┻ ╩
- a 4quad drawing character like ▙
- a new line
- P is nil
The last two cases will be changed to an actual blank character by
virtue of the infinite buffer."
  (or
   (not p)
   (<= (point-max) p) ;; corner case
   (let ((c (char-after p)))
     (or
      (eq c ?\n)
      (gethash c uniline--char-to-4halfs)
      (uniline--char-to-4quadb c)))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--neighbour-point (dir)
    "Return the (point) one char away from current (point) in DIR direction.
Return nil if no such point exists because it would fall outside the buffer.
The buffer is not modified."
    (setq dir (eval dir))
    (uniline--switch-with-table dir
      (uniline-direction-ri→
       '(unless (eolp) (1+ (point))))
      (uniline-direction-lf←
       '(unless (bolp) (1- (point))))
      (uniline-direction-up↑
       '(let ((here (point))
              (c (current-column)))
          (prog1
              (and (eq (forward-line -1) 0)
                   (eq (move-to-column c) c)
                   (point))
            (goto-char here))))
      (uniline-direction-dw↓
       '(let ((here (point))
              (c (current-column)))
          (prog1
              (and (eq (forward-line 1) 0)
                   (eq (move-to-column c) c)
                   (point))
            (goto-char here)))))))

(defun uniline--blank-neighbour1 (dir)
  "Return non-nil if the neighbour of current point in DIR is blank.
The neighbour is one character away in the DIR direction.
Blank include:
- actual blank
- 4half drawing character like ┴ ┻ ╩
- 4quad drawing character like ▙
- new line
- neighbour is outside buffer."
  (uniline--blank-after
   (uniline--switch-with-cond dir
     (uniline-direction-up↑ (uniline--neighbour-point uniline-direction-up↑))
     (uniline-direction-ri→ (uniline--neighbour-point uniline-direction-ri→))
     (uniline-direction-dw↓ (uniline--neighbour-point uniline-direction-dw↓))
     (uniline-direction-lf← (uniline--neighbour-point uniline-direction-lf←)))))

(defsubst uniline--blank-neighbour4 (dir)
  "Return non-nil if the quarter point cursor can move in DIR
while staying on the same (point)."
  (eq
   (logand
    uniline--which-quadrant
    (uniline--switch-with-table dir
      (lambda (dir)
        (uniline--4quadb-pushed
         dir
         (uniline--char-to-4quadb ?█) ; this is constant 15 = 0b1111
         ))
      (uniline-direction-up↑)
      (uniline-direction-ri→)
      (uniline-direction-dw↓)
      (uniline-direction-lf←)))
   0))

(defun uniline--blank-neighbour (dir)
  "Return non-nil if the neighbour in DIR direction is blank.
Blank include:
- actual blank
- 4half drawing character like ┴ ┻ ╩
- 4quad drawing character like ▙
- new line
- neighbour is outside buffer
- when the cursor is :block and there is still room to move in DIRection
  while staying on the same (point)."
  (or
   (and (eq uniline-brush :block)
        (uniline--blank-neighbour4 dir))
   (and (uniline--blank-neighbour1 dir))))

;;;╭──────────────────────────────────────────────────╮
;;;│High level drawing in half-lines & quadrant-blocks│
;;;╰──────────────────────────────────────────────────╯

(defvar-local uniline--arrow-direction
  (uniline-direction-up↑)
  "Where the next arrow should point to.
This might be 0, 1, 2, 3, as defined by the four constants
`uniline-direction-up↑', `uniline-direction-lf←', ...")

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
       (if uniline-brush
           (let ((bits
                  (or
                   (gethash (uniline--char-after) uniline--char-to-4halfs)
                   (and ,force 0))))
             (cond
              ;; 1st case: (char-after) is a line-character like ├,
              ;; or any character if FORCE
              ;; then change a half-line of this character
              ;; for example changing it from ├ to ┽
              (bits
               (uniline--insert-4halfs
                (logior
                 (logand
                  bits
                  ,(lognot (uniline--shift-4half 3 dir)))
                 (uniline--shift-4half uniline-brush ,dir))))
              ;; 2nd case: (char-after) is a block character like ▜,
              ;; and the brush is the eraser
              ;; then clear only half of this character
              ((eq uniline-brush 0)
               (uniline--clear-two-4quadb ,dir))))))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--write-impl (dir force)
    "Move cursor in direction DIR drawing or erasing lines.
Or extending region.
This is an implementation function for the 4 actual functions.
- If region is already active, just extend it without drawing.
- If the brush is set to `:block', draw one quadrant-block.
  Then move cursor half a character.
  This could move (point) one char, or leave it where it is.
- Otherwise, draw or erase pairs of half-lines.
  `uniline-brush' gives the style of line (it may be an eraser).
REPEAT is the length of the line to draw or erase,
or the number of quadrant-blocks to draw,
or the length to extend region.
REPEAT defaults to 1.
When FORCE is not nil, overwrite characters which are not lines."
    (setq dir (eval dir)) ;; to convert 'uniline-direction-dw↓ into 2
    `(progn
       (unless repeat (setq repeat 1))
       (setq uniline--arrow-direction ,dir)
       (handle-shift-selection)
       (cond
        ((region-active-p)
         ;; region is marked, continue extending it
         (uniline--move-in-direction ,dir repeat)
         (setq deactivate-mark nil))

        ((eq uniline-brush :block)
         ;; draw quadrant-blocks ▝▙▄▌
         (cl-loop
          repeat repeat
          do
          (uniline--store-undo-quadrant-cursor)

          (if (eq
               (logand
                uniline--which-quadrant
                ,(uniline--4quadb-pushed
                  (uniline--reverse-direction dir)
                  (uniline--char-to-4quadb ?█)))
               0)
              (uniline--move-in-direction ,dir))

          (setq uniline--which-quadrant
                ;; this huge expression is evaluated only at compile time
                ;; and folded to a mere, fast reference to a constant vector
                ;; for instance:
                ;; (aref [nil 4 8 nil 1 nil nil nil 2] uniline--which-quadrant)
                ,(cond
                  ((memq dir (list uniline-direction-up↑ uniline-direction-dw↓))
                   '(uniline--switch-with-table uniline--which-quadrant
                      ((uniline--char-to-4quadb ?▘) (uniline--char-to-4quadb ?▖))
                      ((uniline--char-to-4quadb ?▖) (uniline--char-to-4quadb ?▘))
                      ((uniline--char-to-4quadb ?▗) (uniline--char-to-4quadb ?▝))
                      ((uniline--char-to-4quadb ?▝) (uniline--char-to-4quadb ?▗))))
                  ((memq dir (list uniline-direction-ri→ uniline-direction-lf←))
                   '(uniline--switch-with-table uniline--which-quadrant
                      ((uniline--char-to-4quadb ?▘) (uniline--char-to-4quadb ?▝))
                      ((uniline--char-to-4quadb ?▖) (uniline--char-to-4quadb ?▗))
                      ((uniline--char-to-4quadb ?▗) (uniline--char-to-4quadb ?▖))
                      ((uniline--char-to-4quadb ?▝) (uniline--char-to-4quadb ?▘))))))

          (uniline--write-one-4quadb ,force)))

        (t
         ;; draw lines ╰──╮
         (cl-loop
          repeat repeat
          do
          (uniline--write-one-4halfs ,dir ,force)
          until (uniline--at-border-p ,dir)
          do
          (uniline--move-in-direction ,dir)
          (uniline--write-one-4halfs
           ,(uniline--reverse-direction dir)
           ,force)))))))

(defun uniline-write-up↑ (repeat &optional force)
  "Move cursor up drawing or erasing glyphs, or extending region.
- If region is already active, just extend it without drawing.
- If the brush is set to blocks, draw one quadrant-block.
  Then move cursor half a character.
- Otherwise, draw or erase pairs of half-lines.
  `uniline-brush' gives the style of line (it may be an eraser).
REPEAT is the length of the line to draw or erase,
or the number of quadrant-blocks to draw,
or the length to extend region.
REPEAT defaults to 1.
When FORCE is not nil, overwrite characters which are not lines."
  (interactive "P")
  (uniline--write-impl uniline-direction-up↑ force))

(defun uniline-write-ri→ (repeat &optional force)
  "Move cursor right drawing or erasing glyphs, or extending region.
- If region is already active, just extend it without drawing.
- If the brush is set to blocks, draw one quadrant-block.
  Then move cursor half a character.
- Otherwise, draw or erase pairs of half-lines.
  `uniline-brush' gives the style of line (it may be an eraser).
REPEAT is the length of the line to draw or erase,
or the number of quadrant-blocks to draw,
or the length to extend region.
REPEAT defaults to 1.
When FORCE is not nil, overwrite characters which are not lines."
  (interactive "P")
  (uniline--write-impl uniline-direction-ri→ force))

(defun uniline-write-dw↓ (repeat &optional force)
  "Move cursor down drawing or erasing glyphs, or extending region.
- If region is already active, just extend it without drawing.
- If the brush is set to blocks, draw one quadrant-block.
  Then move cursor half a character.
- Otherwise, draw or erase pairs of half-lines.
  `uniline-brush' gives the style of line (it may be an eraser).
REPEAT is the length of the line to draw or erase,
or the number of quadrant-blocks to draw,
or the length to extend region.
REPEAT defaults to 1.
When FORCE is not nil, overwrite characters which are not lines."
  (interactive "P")
  (uniline--write-impl uniline-direction-dw↓ force))

(defun uniline-write-lf← (repeat &optional force)
  "Move cursor left drawing or erasing glyphs, or extending region.
- If region is already active, just extend it without drawing.
- If the brush is set to blocks, draw one quadrant-block.
  Then move cursor half a character.
- Otherwise, draw or erase pairs of half-lines.
  `uniline-brush' gives the style of line (it may be an eraser).
REPEAT is the length of the line to draw or erase,
or the number of quadrant-blocks to draw,
or the length to extend region.
REPEAT defaults to 1.
When FORCE is not nil, overwrite characters which are not lines."
  (interactive "P")
  (uniline--write-impl uniline-direction-lf← force))

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
  `uniline-brush' gives the style of line (it may be an eraser).
When FORCE is not nil, overwrite whatever is in the buffer."
  (funcall
   (uniline--switch-with-table dir
     (uniline-direction-up↑ 'uniline-write-up↑)
     (uniline-direction-ri→ 'uniline-write-ri→)
     (uniline-direction-dw↓ 'uniline-write-dw↓)
     (uniline-direction-lf← 'uniline-write-lf←))
   1
   force))

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
            (if (setq p (uniline--neighbour-point uniline-direction-up↑))
                (push p stack))
            (if (setq p (uniline--neighbour-point uniline-direction-ri→))
                (push p stack))
            (if (setq p (uniline--neighbour-point uniline-direction-dw↓))
                (push p stack))
            (if (setq p (uniline--neighbour-point uniline-direction-lf←))
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
are made available to BODY for easy handling of the selection.
The selection may be reversed in any way, the variables
are sets as if the selection was made from
the upper-most, left-most to the lower-most, right-most points.
It works even when in `rectangle-mark-mode'.
Note that ENDX & ENDY point outside the selection in such a way that
ENDX-BEGX is the width of the rectangle, ENDY-BEGY is the height
After execution of the body, selection is activated
from BEGX,BEGY inclusive to ENDX,ENDY exclusive
in `rectangle-mark-mode'."
    (declare (debug (body)))
    `(when (region-active-p)
       (rectangle-mark-mode -1) ; otherwise sometimes end is wrong
       (let* ((deactivate-mark) ; kludge needed to avoid deactivating the mark
              (beg (region-beginning))
              (end (region-end))
              (begx (progn (goto-char beg) (current-column)))
              (begy (1- (line-number-at-pos)))
              (endx (progn (goto-char end) (current-column)))
              (endy (line-number-at-pos)))
         (when (< endx begx)
           (setq endx (prog1 begx (setq begx endx)))
           (setq beg (+ beg (- begx endx)))
           (setq end (+ end (- endx begx))))
         ,@body
         (uniline-move-to-lin-col (1- endy) endx)
         (set-mark (point))
         (uniline-move-to-lin-col begy begx)
         (rectangle-mark-mode 1)))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--compute-leakage-4halfs (dir)
    "Compute lines leakage from two directions for 4halfs characters.
When a rectangle moves, it leaves blank chars.
Those chars are filled with leakage from their two neighbours,
in DIR direction, and its opposite.
For instance consider a situation like this:
   ╶┬╴
    E
    ┗╸
where E is the char leaved empty after translation.
Then the leakage of the two glyphs fills in E:
   ╶┬╴
    ╽
    ┗╸"
    (setq dir (eval dir))
    (let ((odir (uniline--reverse-direction dir)))
      `(let ((here
              (or
               (gethash (uniline--char-after) uniline--char-to-4halfs)
               0))
             (prev    ; char preceding (point) as a 4halfs-bit-pattern
              (let ((p (uniline--neighbour-point ,odir)))
                (or
                 (and
                  p
                  (gethash (uniline--char-after p) uniline--char-to-4halfs))
                 0))))
         ;; mask pairs of bits in the desired direction
         (setq
          here (logand here ,(uniline--shift-4half 3 odir))
          prev (logand prev ,(uniline--shift-4half 3 dir)))

         ;; rotate pairs of bits 180°
         (setq
          here (uniline--shift-4half here ,(- dir odir))
          prev (uniline--shift-4half prev ,(- odir dir)))

         ;; return the blank char with leakage from neighbours.
         (aref uniline--4halfs-to-char (logior here prev))))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--compute-leakage-quadb (dir)
    "Compute lines leakage from two directions for 4quadb characters.
When a rectangle moves, it leaves blank chars.
Those chars are filled with leakage from their two neighbours,
in DIR direction, and its opposite.
For instance consider a situation like this:
    ▌
    E
    ▙
where E is the char leaved empty after translation.
Then the leakage of the two glyphs fills in E:
    ▌
    ▌
    ▙"
    (setq dir (eval dir))
    (let ((odir (uniline--reverse-direction dir)))
      `(let ((here (or (uniline--char-to-4quadb (uniline--char-after)) 0))
             (prev    ; char preceding (point) as a 4quadb-bit-pattern
              (let ((p (uniline--neighbour-point ,odir)))
                (or
                 (and p (uniline--char-to-4quadb (uniline--char-after p)))
                 0))))
         (setq
          here (uniline--4quadb-pushed , dir here)
          prev (uniline--4quadb-pushed ,odir prev))
         (aref uniline--4quadb-to-char (logior here prev))))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--translate-1xsize-slice (dir size)
    "Translate a rectangle 1 x SIZE rectangle one char.
This is a helper function called several times to move
a rectangle one slice at a time.
DIR is the direction to move.
The cursor (point) is one end of the slice.
When moved, the 1 x SIZE rectangle leaves a blank char.
This char is filled with leakage from its two neighbours."
    (setq dir (eval dir))
    ;; initialize `hand' with the computed leakage from neighbours
    ;; then iterate to move one char at a time, keeping it in `hand'
    `(let ((hand (uniline--compute-leakage-4halfs ,dir)))
       (if (eq hand ? )
           (setq hand (uniline--compute-leakage-quadb ,dir)))
       (cl-loop
        repeat ,size
        do
        (setq hand
              (prog1 (uniline--char-after)
                (uniline--insert-char hand)))
        (uniline--move-in-direction ,dir))
       (uniline--insert-char hand))))

(defun uniline-move-rect-up↑ (repeat)
  "Move the rectangle marked by selection one line upper.
Truncate the selection if it touches the upper side of the buffer.
REPEAT tells how many characters the rectangle should move,
defaulting to 1.
    ↑ ↑ ↑ ↑
    ░░░░░░░
    ░░░░░░░
    ░░░░░░░
"
  (interactive "P")
  (cl-loop
   repeat (or repeat 1)
   do
   (uniline--operate-on-rectangle
    (setq
     begy (max (1- begy) 0)
     endy (max (1- endy) 0))
    (cl-loop
     for x from begx below endx
     do
     (uniline-move-to-lin-col endy x)
     (uniline--translate-1xsize-slice
      uniline-direction-up↑
      (- endy begy))))))

(defun uniline-move-rect-ri→ (repeat)
  "Move the rectangle marked by selection one char to the left.
The buffer is infinite at its right side.
REPEAT tells how many characters the rectangle should move,
defaulting to 1.
    ░░░░░░░→
    ░░░░░░░→
    ░░░░░░░→
"
  (interactive "P")
  (cl-loop
   repeat (or repeat 1)
   do
   (uniline--operate-on-rectangle
    (cl-loop
     for y from begy below endy
     do
     (uniline-move-to-lin-col y begx)
     (uniline--translate-1xsize-slice
      uniline-direction-ri→
      (- endx begx)))
    (setq
     begx (1+ begx)
     endx (1+ endx)))))

(defun uniline-move-rect-dw↓ (repeat)
  "Move the rectangle marked by selection one line down.
The buffer is infinite at the bottom.
REPEAT tells how many characters the rectangle should move,
defaulting to 1.
    ░░░░░░░
    ░░░░░░░
    ░░░░░░░
    ↓ ↓ ↓ ↓
"
  (interactive "P")
  (cl-loop
   repeat (or repeat 1)
   do
   (uniline--operate-on-rectangle
    (cl-loop
     for x from begx below endx
     do
     (uniline-move-to-lin-col begy x)
     (uniline--translate-1xsize-slice
      uniline-direction-dw↓
      (- endy begy)))
    (setq
     begy (1+ begy)
     endy (1+ endy)))))

(defun uniline-move-rect-lf← (repeat)
  "Move the rectangle marked by selection one char to the left.
Truncate the selection if it touches the left side of the buffer.
REPEAT tells how many characters the rectangle should move,
defaulting to 1.
   ←░░░░░░░
   ←░░░░░░░
   ←░░░░░░░
"
  (interactive "P")
  (cl-loop
   repeat (or repeat 1)
   do
   (uniline--operate-on-rectangle
    (setq
     begx (max (1- begx) 0)
     endx (max (1- endx) 0))
    (cl-loop
     for y from begy below endy
     do
     (uniline-move-to-lin-col y endx)
     (uniline--translate-1xsize-slice
      uniline-direction-lf←
      (- endx begx))))))

(defun uniline-fill-rectangle ()
  "Fill the rectangle marked by selection.
Interactively choose the filling character.
See `uniline--choose-fill-char'.
    ░░░░░░░
    ░░░░░░░
    ░░░░░░░
"
  (interactive)
  (let ((char (uniline--choose-fill-char)))
    (uniline--operate-on-rectangle
     (cl-loop
      for y from begy below endy
      do
      (uniline-move-to-lin-col y begx)
      (cl-loop
       for x from begx below endx
       do
       (uniline--insert-char char)
       (uniline-move-to-delta-column 1))))))

(defun uniline-draw-inner-rectangle (&optional force)
  "Draws a rectangle inside a rectangular selection.
Use the current brush style, which may be thin, thick,
double line, block, or eraser.
When FORCE is not nil, overwrite whatever is there."
  (interactive)
  (uniline--operate-on-rectangle
   (let ((width  (- endx begx 1))
         (height (- endy begy 1)))
     (goto-char beg)
     (if (eq uniline-brush :block)
         (setq
          width  (+ width  width  1)
          height (+ height height 1)
          uniline--which-quadrant (uniline--char-to-4quadb ?▘)))
     (let ((mark-active nil)) ;; otherwise brush would be inactive
       (uniline-write-ri→ width  force)
       (uniline-write-dw↓ height force)
       (uniline-write-lf← width  force)
       (uniline-write-up↑ height force)))))

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
   (let ((width  (- endx begx -1))
         (height (- endy begy -1))
         (mark-active nil))             ; otherwise brush is inactive
     (goto-char beg)
     (if (<= begx 0)                    ; at leftmost side of buffer
         (setq width (1- width))
       (uniline-move-to-delta-column -1))
     (when (eq begy 0)                  ; at the top of buffer
       (goto-char (point-min))
       (insert ?\n))
     (forward-line -1)
     (if (eq uniline-brush :block)
         (setq
          width  (+ width  width  -1)
          height (+ height height -1)
          uniline--which-quadrant (uniline--char-to-4quadb ?▗)))
     (uniline-move-to-column (1- begx))
     (uniline-write-ri→ width  force)
     (uniline-write-dw↓ height force)
     (uniline-write-lf← width  force)
     (if (> begx 0)
         (uniline-write-up↑ height force))
     (when (eq begy 0)
       (goto-char (point-min))
       (delete-line)))))

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
   (goto-char beg)
   (cl-loop
    for line in killed-rectangle
    do
    (cl-loop
     for char across line
     do
     (uniline--insert-char char)
     (uniline-move-to-delta-column 1))
    (uniline-move-to-column begx)
    (uniline-move-to-delta-line 1))
   (setq endx (+ begx (length (car killed-rectangle))))
   (setq endy (+ begy (length killed-rectangle)))))

;;;╭──────────────╮
;;;│Text direction│
;;;╰──────────────╯

(defvar-local uniline-text-direction
    (uniline-direction-ri→)
  "Direction of text insertion.
It can be any of the 4 values of
`uniline-direction-up↑' `-ri→' `-dw↓' `-lf←'
which means that typing a key on the keyboard moves the cursor
in this direction.
It can also be nil, which means that uniline makes no tweaking of
the natural cursor movement upon insertion.")

(defun uniline--post-self-insert ()
  "Change the cursor movement after `self-insert-command'.
Usually the cursor moves to the right.
Sometimes to the left for some locales, but this is not currently handled.
This hook fixes the cursor movement according to `uniline-text-direction'"
  (let* ((pn
          (cond
           ((numberp current-prefix-arg)
            current-prefix-arg)
           ((and
             (consp current-prefix-arg)
             (numberp (car current-prefix-arg)))
            (car current-prefix-arg))
           ((null current-prefix-arg)
            1)
           (t (error "current-prefix-arg = %S" current-prefix-arg))))
         (mn (- pn)))
    (uniline--switch-with-cond uniline-text-direction
      (uniline-direction-ri→ ()) ;; nothing to fix
      (uniline-direction-lf←
       (forward-char mn)
       (uniline-move-to-delta-column mn))
      (uniline-direction-dw↓
       (forward-char mn)
       (uniline-move-to-delta-line pn))
      (uniline-direction-up↑
       (forward-char mn)
       (uniline-move-to-delta-line mn))
      (nil))))

(defun uniline-text-direction-up↑ ()
  "Set text insertion direction up↑."
  (interactive)
  (setq uniline-text-direction (uniline-direction-up↑)))
(defun uniline-text-direction-ri→ ()
  "Set text insertion direction right→."
  (interactive)
  (setq uniline-text-direction (uniline-direction-ri→)))
(defun uniline-text-direction-dw↓ ()
  "Set text insertion direction down↓."
  (interactive)
  (setq uniline-text-direction (uniline-direction-dw↓)))
(defun uniline-text-direction-lf← ()
  "Set text insertion direction left←."
  (interactive)
  (setq uniline-text-direction (uniline-direction-lf←)))

;;;╭───────────────────────────╮
;;;│Macro calls in 4 directions│
;;;╰───────────────────────────╯

(defvar-local uniline--directional-macros
    (make-vector 8 nil)
  "A cache handling 4 versions of the current macro in 4 directions.
It is needed to avoid repeatidly re-creating a new directional macro
from the recorded macro.
There are 4 entries indexed by
`uniline-direction-up↑' `-ri→' `-dw↓' `-lf←'
Each entry has 2 slots:
- the recorded macro (a vector of key strokes)
- the twisted macro (the same vector with <up> <right> and sisters twisted).")

(eval-when-compile ; not used at runtime
  (defconst uniline--directional-keystrokes-table
    `(
      ;;   ╭─keystroke as used in keyboard macros
      ;;   │      ╭─direction of the keystroke
      ;;   │      │      shift-control modifiers╮
      ;;   │      │                     ╭───────╯
      ;;   ▽      ▽                     ▽
      (  up    ,uniline-direction-up↑)
      (  right ,uniline-direction-ri→)
      (  down  ,uniline-direction-dw↓)
      (  left  ,uniline-direction-lf←)
      (S-up    ,uniline-direction-up↑ . S)
      (S-right ,uniline-direction-ri→ . S)
      (S-down  ,uniline-direction-dw↓ . S)
      (S-left  ,uniline-direction-lf← . S)
      (C-up    ,uniline-direction-up↑ . C)
      (C-right ,uniline-direction-ri→ . C)
      (C-down  ,uniline-direction-dw↓ . C)
      (C-left  ,uniline-direction-lf← . C))
    "Temporary table of conversion between keystrokes and uniline directions.
It will be converted into 2 hashtables for both conversions."))

(uniline--defconst-hash-table
 uniline--keystroke-to-dir-shift
 (eval-when-compile
   (cl-loop
    for entry in uniline--directional-keystrokes-table
    collect (cons (car entry) (cdr entry))))
 31 'eq
 "Hashtable to convert a directional keystroke into Uniline constants.")

;; is it collision-less?
;; (internal--hash-table-histogram  uniline--keystroke-to-dir-shift)
;; (internal--hash-table-index-size uniline--keystroke-to-dir-shift)

(uniline--defconst-hash-table
 uniline--dir-shift-to-keystroke
 (eval-when-compile
   (cl-loop
    for entry in uniline--directional-keystrokes-table
    collect (cons (cdr entry) (car entry))))
 31 'equal
 "Hashtable to convert Uniline directional constants into keystrokes.")

;; is it collision-less?
;; (internal--hash-table-histogram  uniline--dir-shift-to-keystroke)
;; (internal--hash-table-index-size uniline--dir-shift-to-keystroke)

(defun uniline-call-macro-in-direction (dir)
  "Call last keybord macro twisted in DIR direction.
A twisted version of the last keybord macro is created, unless
it is already present in the `uniline--directional-macros' cache"
  (interactive)
  (let*
      ((uniline-text-direction dir)
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
                                (% (+ (car y) dir 3) 4)
                                (cdr y))
                               uniline--dir-shift-to-keystroke)
                            x)))
                      last-kbd-macro)))))))
    (kmacro-end-and-call-macro 1)))

;; Run the following cl-loop to automatically write a bunch
;; of 4 interactive functions
;; They have little value, except to be an interface between
;; `easy-menu-define', `defhydra',
;; and the real function `uniline-call-macro-in-direction'

(when nil
  (insert "\n;; BEGIN -- Automatically generated\n")
  (cl-loop
   for dir in '("up↑" "ri→" "dw↓" "lf←")
   do
    (cl-prettyprint
     `(defun ,(intern (format "uniline-call-macro-in-direction-%s" dir)) ()
        ,(format "Call macro in direction %s." dir)
        (interactive)
        (uniline-call-macro-in-direction (,(intern (format "uniline-direction-%s" dir)))))))
  (insert "\n;; END -- Automatically generated\n"))

;; BEGIN -- Automatically generated

(defun uniline-call-macro-in-direction-up↑ nil
  "Call macro in direction up↑."
  (interactive)
  (uniline-call-macro-in-direction (uniline-direction-up↑)))
(defun uniline-call-macro-in-direction-ri→ nil
  "Call macro in direction ri→."
  (interactive)
  (uniline-call-macro-in-direction (uniline-direction-ri→)))
(defun uniline-call-macro-in-direction-dw↓ nil
  "Call macro in direction dw↓."
  (interactive)
  (uniline-call-macro-in-direction (uniline-direction-dw↓)))
(defun uniline-call-macro-in-direction-lf← nil
  "Call macro in direction lf←."
  (interactive)
  (uniline-call-macro-in-direction (uniline-direction-lf←)))
;; END -- Automatically generated

;;;╭───────────────────────────╮
;;;│High level brush management│
;;;╰───────────────────────────╯

(defun uniline-set-brush-nil ()
  "Change the current style of line to nothing.
It means that cursor movements do not trace anything."
  (interactive)
  (setq uniline-brush nil)
  (force-mode-line-update))

(defun uniline-set-brush-0 ()
  "Change the current style of line to the eraser.
It means that moving the cursor horizontally erase horizontal
lines, and moving vertically erase vertical lines.  Characters
other than lines or arrows are not touched."
  (interactive)
  (setq uniline-brush 0)
  (force-mode-line-update))

(defun uniline-set-brush-1 ()
  "Change the current style of line to a single thin line╶─╴."
  (interactive)
  (setq uniline-brush 1)
  (force-mode-line-update))

(defun uniline-set-brush-2 ()
  "Change the current style of line to a single thick line╺━╸."
  (interactive)
  (setq uniline-brush 2)
  (force-mode-line-update))

(defun uniline-set-brush-3 ()
  "Change the current style of line to a double line╺═╸."
  (interactive)
  (setq uniline-brush 3)
  (force-mode-line-update))

(defun uniline-set-brush-block ()
  "Change the current style of line to blocks ▙▄▟▀."
  (interactive)
  (setq uniline-brush :block)
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
         (gethash
          (uniline--char-after)
          (cond
           (back uniline--glyphs-reverse-hash-bw)
           (t    uniline--glyphs-reverse-hash-fw)))))
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

(eval-when-compile ; not needed at runtime
  (defmacro uniline--rotate-arrow (dir)
    "Rotate an arrow, or tweak a 4halfs or a 4quadb.
- If character under point is a known arrow, turn it in DIR direction.
- If character under point is a combination of 4halfs lines,
then change the 4half segment pointing in the DIR direction.
Repeateadly changing in the same direction cycles through at most
4 characters (with thin, thick, double, or no line at all in DIR
direction). Sometimes, the cycle is shorter than 4, because UNICODE
does not define all combinations of 4halfs when one of them is a
double line.
- If character under point is a combination of 4quadb blocks,
then flip the block in DIR direction on-and-off. As the blocks are
in the 4 corners of the character, DIR cannot point exactly to a block.
Instead DIR is twisted 45° from the actual direction of the block."
    (setq dir (eval dir)) ; turn symbol like --direction-dw↓ to number like 2
    (let* ((dir1 (1+ dir))
           (ash3dir2 (uniline--shift-4half 3 dir))
           (ash1dir2 (uniline--shift-4half 1 dir))
           (notash3dir2 (lognot ash3dir2))
           (dir4
            (uniline--4quadb-pushed
             (uniline--turn-left dir)
             (uniline--4quadb-pushed
              dir
              (uniline--char-to-4quadb ?█) ; this is constant 15 = 0b1111
              ))))
      `(let ((pat                       ; pattern
              (gethash (uniline--char-after)
                       uniline--glyphs-reverse-hash-fw)))
         (cond
          ;; If (point) is on a directional arrow
          ((car pat)
           (uniline--insert-char        ; then change its direction
            (nth ,dir1 (cadr pat)))
           (setq uniline--arrow-direction ,dir))

          ;; If point is on lines crossing
          ((setq pat (gethash (uniline--char-after) uniline--char-to-4halfs))
           (let ((patdir (logand pat ,   ash3dir2)) ; pattern in DIR
                 (patnot (logand pat ,notash3dir2)) ; pattern not in DIR
                 patnew)                            ; new pattern to insert
             (while               ; search for valid UNICODE
                 (progn           ; with one 4half segment changed
                   (setq patdir (logand (+ patdir ,ash1dir2) ,ash3dir2))
                   (setq patnew (logior patnot patdir))
                   (not
                    (eq
                     (gethash
                      (aref uniline--4halfs-to-char patnew)
                      uniline--char-to-4halfs)
                     patnew))))
             (uniline--insert-4halfs patnew)))

          ;; If point is on a quarter-char
          ((setq pat (uniline--char-to-4quadb (uniline--char-after)))
           (uniline--insert-4quadb (logxor pat ,dir4))))))))

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
        ,(format "Rotate an arrow or tweak 4halfs.
If character under point is an arrow, turn it %s.
If character under point is a combination of 4halfs lines,
then change the 4half segment pointing %s."
                 dir dir)
        (interactive)
        (uniline--rotate-arrow (,(intern (format "uniline-direction-%s" dir)))))))
  (insert "\n;; END -- Automatically generated\n"))
;; BEGIN -- Automatically generated

(defun uniline-rotate-up↑ nil
  "Rotate an arrow or tweak 4halfs.
If character under point is an arrow, turn it up↑.
If character under point is a combination of 4halfs lines,
then change the 4half segment pointing up↑."
  (interactive)
  (uniline--rotate-arrow (uniline-direction-up↑)))
(defun uniline-rotate-ri→ nil
  "Rotate an arrow or tweak 4halfs.
If character under point is an arrow, turn it ri→.
If character under point is a combination of 4halfs lines,
then change the 4half segment pointing ri→."
  (interactive)
  (uniline--rotate-arrow (uniline-direction-ri→)))
(defun uniline-rotate-dw↓ nil
  "Rotate an arrow or tweak 4halfs.
If character under point is an arrow, turn it dw↓.
If character under point is a combination of 4halfs lines,
then change the 4half segment pointing dw↓."
  (interactive)
  (uniline--rotate-arrow (uniline-direction-dw↓)))
(defun uniline-rotate-lf← nil
  "Rotate an arrow or tweak 4halfs.
If character under point is an arrow, turn it lf←.
If character under point is a combination of 4halfs lines,
then change the 4half segment pointing lf←."
  (interactive)
  (uniline--rotate-arrow (uniline-direction-lf←)))
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
When FORCE is not nil, overwrite whatever is in the buffer.
 ╭──────╮
 │AAAAAA╰─╮
 ╰─╮AAAAAA│
   │AA╭───╯
   ╰──╯
"
  (interactive)
  (while (not (uniline--blank-after (point)))
    (uniline-move-to-delta-column 1))

  (let ((dir (uniline-direction-dw↓))
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
    (if (eq uniline-brush :block)
        (setq
         q
         (setq
          uniline--which-quadrant
          (uniline--switch-with-table dir
            (lambda (dir)
              (uniline--4quadb-pushed
               (uniline--reverse-direction dir)
               (uniline--4quadb-pushed
                (uniline--turn-right dir)
                (uniline--char-to-4quadb ?█))))
            (uniline-direction-up↑)
            (uniline-direction-ri→)
            (uniline-direction-dw↓)
            (uniline-direction-lf←)))))
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
             (eq dir (uniline-direction-lf←))
             (uniline--at-border-p uniline-direction-lf←)
             (or (not (eq uniline-brush :block))
                 (eq
                  (logand uniline--which-quadrant
                          (uniline--char-to-4quadb ?▐))
                  0)))
            (while
                (and
                 (= (forward-line -1) 0)
                 (not (uniline--blank-after (point)))))
            (if (uniline--at-border-p uniline-direction-up↑)
                (setq dir (uniline-direction-up↑)
                      uniline--which-quadrant (uniline--char-to-4quadb ?▝))
              (setq dir (uniline-direction-ri→)
                    uniline--which-quadrant (uniline--char-to-4quadb ?▖))))
           ((and
             (eq dir (uniline-direction-up↑))
             (uniline--at-border-p uniline-direction-up↑)
             (or (not (eq uniline-brush :block))
                 (eq
                  (logand uniline--which-quadrant
                          (uniline--char-to-4quadb ?▄))
                  0)))
            (while
                (progn
                  (forward-char 1)
                  (not (uniline--blank-after (point)))))
            (setq dir (uniline-direction-dw↓))
            (setq uniline--which-quadrant (uniline--char-to-4quadb ?▘)))
           (t
            (uniline--write dir force)
            (setq n (1+ n))))
          (and
           (not
            (and (eq (point) (marker-position start))
                 (or (not (eq uniline-brush :block))
                     (eq uniline--which-quadrant q))))
           (< n 10000))))
    (set-marker start nil)
    (message "drew a %s steps contour" n)))

;;;╭─────────────────────────────╮
;;;│Dashed lines and other styles│
;;;╰─────────────────────────────╯

(defun uniline--change-style-hash (fromto)
  "Change some characters to similar characters in a rectangular selection.
FROMTO is a hash-table telling what are the characters replacements.
The changes are reversible in a single undo command."
    (interactive)
    (uniline--operate-on-rectangle
     (let ((handle (prepare-change-group)))
       (cl-loop
        for y from begy below endy
        do
        (uniline-move-to-line y)
        (cl-loop
         for x from begx below endx
         do
         (uniline-move-to-column x)
         (let ((rep (gethash (uniline--char-after) fromto)))
           (if rep
               (uniline--insert-char rep)))))
       (undo-amalgamate-change-group handle)
       (accept-change-group handle))))

(defconst uniline--char-to-dot-3-2-char
  (eval-when-compile
    (let ((table (make-hash-table)))
      (puthash ?─ ?╌ table)
      (puthash ?┄ ?╌ table)
      (puthash ?┈ ?╌ table)
      (puthash ?━ ?╍ table)
      (puthash ?═ ?╍ table)
      (puthash ?┅ ?╍ table)
      (puthash ?┉ ?╍ table)
      (puthash ?│ ?┆ table)
      (puthash ?┊ ?┆ table)
      (puthash ?┃ ?┇ table)
      (puthash ?║ ?┇ table)
      (puthash ?┋ ?┇ table)
      table)))

(defun uniline-change-style-dot-3-2 ()
  "Change plain lines to dashed lines in a rectangular selection.
It retains thickness of the lines.
Vertical lines will be 3-dots, while horizontal will be 2-dots."
  (interactive)
  (uniline--change-style-hash uniline--char-to-dot-3-2-char))

(defconst uniline--char-to-dot-4-4-char
  (eval-when-compile
    (let ((table (make-hash-table)))
      (puthash ?─ ?┈ table)
      (puthash ?┄ ?┈ table)
      (puthash ?╌ ?┈ table)
      (puthash ?━ ?┉ table)
      (puthash ?═ ?┉ table)
      (puthash ?┅ ?┉ table)
      (puthash ?╍ ?┉ table)
      (puthash ?│ ?┊ table)
      (puthash ?┆ ?┊ table)
      (puthash ?┃ ?┋ table)
      (puthash ?║ ?┋ table)
      (puthash ?┇ ?┋ table)
      table)))

(defun uniline-change-style-dot-4-4 ()
  "Change plain lines to dashed lines in a rectangular selection.
It retains thickness of the lines.
Vertical and horizontzl lines will be 4-dots."
  (interactive)
  (uniline--change-style-hash uniline--char-to-dot-4-4-char))

(defconst uniline--char-to-standard-char
  (eval-when-compile
    (let ((table (make-hash-table)))
      (puthash ?┄ ?─ table)
      (puthash ?┈ ?─ table)
      (puthash ?╌ ?─ table)
      (puthash ?┉ ?━ table)
      (puthash ?┅ ?━ table)
      (puthash ?╍ ?━ table)
      (puthash ?┆ ?│ table)
      (puthash ?┊ ?│ table)
      (puthash ?┇ ?┃ table)
      (puthash ?┋ ?┃ table)
      (puthash ?┐ ?╮ table)
      (puthash ?└ ?╰ table)
      (puthash ?┌ ?╭ table)
      (puthash ?┘ ?╯ table)
      (puthash ?^ ?△ table)
      (puthash ?v ?▽ table)
      (puthash ?> ?▷ table)
      (puthash ?< ?◁ table)
      (puthash ?- ?─ table)
      (puthash ?_ ?─ table)
      (puthash ?| ?│ table)
      (puthash ?\" ?║ table)
      (puthash ?= ?═ table)
      table)))

(defun uniline-change-style-standard ()
  "Change fancy lines styles to standard ones in a rectangular selection.
This includes dashed lines, which become plain while preserving thickness,
and hard corners which become rounded."
    (interactive)
  (uniline--change-style-hash uniline--char-to-standard-char)
  (uniline--operate-on-rectangle
   (let ((handle (prepare-change-group)))
     (cl-loop
      for y from begy below endy
      do
      (uniline-move-to-line y)
      (cl-loop
       for x from begx below endx
       do
       (uniline-move-to-column x)
       (let ((c (uniline--char-after))
             (bits 0))
         (cond
          ((or (eq c ?+) (eq c ?/) (eq c ?\\))
           (cl-loop
            for dir in '(0 1 2 3)
            do
            (let ((neighbour
                   (uniline--switch-with-cond dir
                     (uniline-direction-up↑ (uniline--neighbour-point uniline-direction-up↑))
                     (uniline-direction-ri→ (uniline--neighbour-point uniline-direction-ri→))
                     (uniline-direction-dw↓ (uniline--neighbour-point uniline-direction-dw↓))
                     (uniline-direction-lf← (uniline--neighbour-point uniline-direction-lf←)))))
              (when neighbour
                (let ((b
                       (gethash
                        (uniline--char-after neighbour)
                        uniline--char-to-4halfs)))
                  (when b
                    (setq b
                          (logand
                           b
                           (ash 3 (* 2 (% (+ dir 2) 4)))))
                    (setq bits
                          (logior
                           bits
                           (ash b  4)
                           (ash b -4)))))
                (if (memq (uniline--char-after neighbour) '(?+ ?/ ?\\))
                    (setq bits
                          (logior
                           bits
                           (ash 1 (* 2 dir))
                           ))))))))
         (unless (eq bits 0)
           (uniline--insert-4halfs (logand bits 255))))
       (undo-amalgamate-change-group handle)
       (accept-change-group handle))))
   ))

(defconst uniline--char-to-hard-corner-char
  (eval-when-compile
    (let ((table (make-hash-table)))
      (puthash ?╮ ?┐ table)
      (puthash ?╰ ?└ table)
      (puthash ?╭ ?┌ table)
      (puthash ?╯ ?┘ table)
      table)))

(defun uniline-change-style-hard-corners ()
  "Change rounded corners to hard corners in a rectangular selection.
This happens only for thin-lines corners, as UNICODE does not define
thick-line or double-line rounded corners."
    (interactive)
  (uniline--change-style-hash uniline--char-to-hard-corner-char))

(defconst uniline--char-to-thin-char
  (eval-when-compile
    (let ((table (make-hash-table)))
      (cl-loop
       for c being hash-keys of uniline--char-to-4halfs
       using (hash-values v)
       for 4halfs
       = (uniline--pack-4halfs
          (cl-loop
           for u in (uniline--unpack-4halfs v)
           collect (if (or (eq u 2) (eq u 3)) 1 u)))
       unless (eq 4halfs v)
       do (puthash
           c
           (aref uniline--4halfs-to-char 4halfs)
           table))
      (puthash ?┅ ?┄ table)
      (puthash ?┉ ?┈ table)
      (puthash ?╍ ?╌ table)
      (puthash ?┇ ?┆ table)
      (puthash ?┋ ?┊ table)
      (puthash ?▲ ?△ table)
      (puthash ?▶ ?▷ table)
      (puthash ?▼ ?▽ table)
      (puthash ?◀ ?◁ table)
      (puthash ?▴ ?▵ table)
      (puthash ?▸ ?▹ table)
      (puthash ?▾ ?▿ table)
      (puthash ?◂ ?◃ table)
      (puthash ?■ ?□ table)
      (puthash ?▪ ?▫ table)
      (puthash ?◆ ?◇ table)
      (puthash ?• ?◦ table)
      table)))

(defun uniline-change-style-thin ()
  ""
  (interactive)
  (uniline--change-style-hash uniline--char-to-thin-char))

(defconst uniline--char-to-thick-char
  (eval-when-compile
    (let ((table (make-hash-table)))
      (cl-loop
       for c being hash-keys of uniline--char-to-4halfs
       using (hash-values v)
       for 4halfs
       = (uniline--pack-4halfs
          (cl-loop
           for u in (uniline--unpack-4halfs v)
           collect (if (or (eq u 1) (eq u 3)) 2 u)))
       unless (eq 4halfs v)
       do (puthash
           c
           (aref uniline--4halfs-to-char 4halfs)
           table))
      (puthash ?┄ ?┅ table)
      (puthash ?┈ ?┉ table)
      (puthash ?╌ ?╍ table)
      (puthash ?┆ ?┇ table)
      (puthash ?┊ ?┋ table)
      (puthash ?△ ?▲ table)
      (puthash ?▷ ?▶ table)
      (puthash ?▽ ?▼ table)
      (puthash ?◁ ?◀ table)
      (puthash ?▵ ?▴ table)
      (puthash ?▹ ?▸ table)
      (puthash ?▿ ?▾ table)
      (puthash ?◃ ?◂ table)
      (puthash ?□ ?■ table)
      (puthash ?▫ ?▪ table)
      (puthash ?◇ ?◆ table)
      (puthash ?◦ ?• table)
      table)))

(defun uniline-change-style-thick ()
  ""
  (interactive)
  (uniline--change-style-hash uniline--char-to-thick-char))

(defconst uniline--char-to-double-line
  (eval-when-compile
    (let ((table (make-hash-table)))
      (cl-loop
       for c being hash-keys of uniline--char-to-4halfs
       using (hash-values v)
       for 4halfs
       = (uniline--pack-4halfs
          (cl-loop
           for u in (uniline--unpack-4halfs v)
           collect (if (or (eq u 1) (eq u 2)) 3 u)))
       unless (eq 4halfs v)
       do (puthash
           c
           (aref uniline--4halfs-to-char 4halfs)
           table))
      (puthash ?┄ ?═ table)
      (puthash ?┅ ?═ table)
      (puthash ?┈ ?═ table)
      (puthash ?┉ ?═ table)
      (puthash ?╌ ?═ table)
      (puthash ?╍ ?═ table)
      (puthash ?┆ ?║ table)
      (puthash ?┇ ?║ table)
      (puthash ?┊ ?║ table)
      (puthash ?┋ ?║ table)
      table)))

(defun uniline-change-style-double ()
  ""
  (interactive)
  (uniline--change-style-hash uniline--char-to-double-line))

;;;╭────────────────╮
;;;│Hydra interfaces│
;;;╰────────────────╯

(defun uniline-customize-face ()
  "Customize a temporary font to may-be set it for future sessions."
  (interactive)
  (customize-face-other-window 'default))

(defun uniline--is-font (letter)
  "Check if current font is the one presented by LETTER."
  (let ((name
         (uniline--switch-with-table letter
          (?d "DejaVu"     )
          (?u "Unifont"    )
          (?h "Hack"       )
          (?b "JetBrain"   )
          (?c "Cascadia"   )
          (?a "Agave"      )
          (?j "Julia"      )
          (?f "FreeMono"   )
          (?i "Iosevka"    )
          (?s "Source Code"))))
    (and name (string-match name (frame-parameter nil 'font)))))

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

(defun uniline-text-direction-str ()
  "Return a textual representation of current text direction."
  (interactive)
  (uniline--switch-with-table uniline-text-direction
    (uniline-direction-up↑ "↑")
    (uniline-direction-ri→ "→")
    (uniline-direction-dw↓ "↓")
    (uniline-direction-lf← "←")))

(defhydra uniline-hydra-arrows
  (:hint nil
   :exit nil)
  ;; Docstring MUST begin with an empty line to benefit from substitutions
  (concat
   (string-replace
    "Text dir────"
    "Text dir─╴%s(uniline-text-direction-str)╶"
  "\
╭^─^─^Insert glyph^─────╮╭^Rotate arrow^╮╭^Text dir────^╮╭^─Contour─^╮╭^─^─^─^─^─^─^─^────────────╮
│_a_,_A_rrow ▷ ▶ → ▹ ▸ ↔││_S-<left>_  ← ││_C-<left>_  ← ││_c_ contour││_-_ _+_ _=_ _#_ self-insert│
│_s_,_S_quare  □ ■ ◇ ◆ ◊││_S-<right>_ → ││_C-<right>_ → ││_C_ ovwrt  ││_f_ ^^^^^^      choose font│
│_o_,_O_-shape · ● ◦ Ø ø││_S-<up>_    ↑ ││_C-<up>_    ↑ │╭^─────────^╮│_TAB_   ^^^^^^  short hint │
│_x_,_X_-cross ╳ ÷ × ± ¤││_S-<down>_  ↓ ││_C-<down>_  ↓ ││_i_ fill   ││_q_ _RET_ ^^^^  exit       │
╰^─^─^─^────────────────╯╰^Tweak glyph─^╯╰^─^───────────╯╰^─Fill────^╯╰^─^─^─^─^─^─^─^────────────╯"))
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
  ("C-<left>"  uniline-text-direction-lf← :exit t)
  ("C-<up>"    uniline-text-direction-up↑ :exit t)
  ("C-<down>"  uniline-text-direction-dw↓ :exit t)
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

(defun uniline--aa2u-rectangle ()
  "Wrapper arround `aa2u-rectangle'."
  (interactive)
  (if (functionp 'aa2u-rectangle)
      (eval '(aa2u-rectangle (region-beginning) (region-end)))
    (message "Install the ascii-art-to-unicode package prior to using aa2u.
It is available on ELPA.
Or use the '0 standard' style transformer instead.")))

(defhydra uniline-hydra-alt-styles
  (:pre (rectangle-mark-mode 1)
   :hint nil
   :exit nil)
  ;; Docstring MUST begin with an empty line to benefit from substitutions
  "
╭^─^───────╮╭^─Alt styles^──╮╭─^─^────────╮
│_-_ thin  ││_3_ 3x2 dots   ││_0_ standard│
│_+_ thick ││_4_ 4x4 dots   ││_a_ aa2u    │
│_=_ double││_h_ hard corner││_RET_ quit  │
╰^─^───────╯╰^─^────────────╯╰─^─^────────╯"
  ("3"             uniline-change-style-dot-3-2)
  ("<kp-3>"        uniline-change-style-dot-3-2)
  ("4"             uniline-change-style-dot-4-4)
  ("<kp-4>"        uniline-change-style-dot-4-4)
  ("h"             uniline-change-style-hard-corners)
  ("0"             uniline-change-style-standard)
  ("<kp-0>"        uniline-change-style-standard)
  ("-"             uniline-change-style-thin)
  ("<kp-subtract>" uniline-change-style-thin)
  ("c"             uniline-change-style-thin)
  ("+"             uniline-change-style-thick)
  ("<kp-add>"      uniline-change-style-thick)
  ("b"             uniline-change-style-thick)
  ("="             uniline-change-style-double)
  ("a"             uniline--aa2u-rectangle)
  ("C-x C-x" rectangle-exchange-point-and-mark)
  ("C-/"     uniline--hydra-rect-undo)
  ("C-_"     uniline--hydra-rect-undo)
  ("C-x u"   uniline--hydra-rect-undo)
  ("TAB"     uniline-toggle-hydra-hints)
  ("RET"     uniline--hydra-rect-quit :exit t))

(defhydra uniline-hydra-moverect
  (:pre (rectangle-mark-mode 1)
   :hint nil
   :exit nil)
  ;; Docstring MUST begin with an empty line to benefit from substitutions
  "
╭^Move ^rect╮╭────^Draw^ rect────╮╭^─Rect^─╮╭^Brush^╮╭──^Misc^─────────╮
│_<right>_ →││_r_     trace inner││_c_ copy││_-_ ╭─╯││_s_   alt styles │
│_<left>_  ←││_R_     trace outer││_k_ kill││_+_ ┏━┛││_f_   choose font│
│_<up>_    ↑││_C-r_   ovewr inner││_y_ yank││_=_ ╔═╝││_C-/_ undo       │
│_<down>_  ↓││_C-S-R_ ovewr outer│╰^^┬─────╯╯_#_ ▄▄▟││_TAB_ sort hint  │
╰^─────^────╯│_i_     fill       │ ^^│_<delete>_ DEL││_RET_ exit       │
 ^     ^     ╰^────^─────────────╯ ^^╰^────────^────╯╰^───^────────────╯
"
  ("<right>" uniline-move-rect-ri→)
  ("<left>"  uniline-move-rect-lf←)
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
  ("<delete>"       uniline-set-brush-0)
  ("<deletechar>"   uniline-set-brush-0)
  ("C-<delete>"     uniline-set-brush-0)
  ("C-<deletechar>" uniline-set-brush-0)
  ("-"              uniline-set-brush-1)
  ("<kp-subtract>"  uniline-set-brush-1)
  ("+"              uniline-set-brush-2)
  ("<kp-add>"       uniline-set-brush-2)
  ("="              uniline-set-brush-3)
  ("#"              uniline-set-brush-block)
  ("TAB" uniline-toggle-hydra-hints)
  ("f"     uniline-hydra-fonts/body      :exit t)
  ("s"     uniline-hydra-alt-styles/body :exit t)
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
│_<left>_  call ← │^ ^ ^   ^         │
│_<up>_    call ↑ │_TAB_^^ short hint│
│_<down>_  call ↓ │_q_ _RET_ exit    │
╰^^───────────────┴^─^─^───^─────────╯"
  ("e"       (kmacro-end-and-call-macro 1))
  ("<right>" uniline-call-macro-in-direction-ri→)
  ("<left>"  uniline-call-macro-in-direction-lf←)
  ("<up>"    uniline-call-macro-in-direction-up↑)
  ("<down>"  uniline-call-macro-in-direction-dw↓)
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
        "glyph:^aAsSoOxX-+=#^ arr&tweak:^S-→←↑↓^ text-dir:^C-→←↑↓^ ^c^-ontour f-^i^-ll ^f^-onts ^TAB^")))
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
        "move: ^→←↑↓^ trace: ^rR C-rR^ copy-paste: ^cky^ f-^i^-ll brush: ^-+=# DEL^ ^s^tyle ^f^-onts ^TAB^")))
 uniline-hydra-macro-exec/hint
 `(if (eq uniline-hint-style t)
      ,uniline-hydra-macro-exec/hint
    ,(eval-when-compile
       (uniline--color-hint
        "macro exec usual: ^e^  directional: ^→←↑↓^  hint: ^TAB^")))
 uniline-hydra-alt-styles/hint
 `(if (eq uniline-hint-style t)
      ,uniline-hydra-alt-styles/hint
    ,(eval-when-compile
       (uniline--color-hint
        "alt styles, brush: ^-+=^, dashed: ^34^ corners: ^h^ standard: ^0^ ^a^a2u"))))

(defun uniline-toggle-hydra-hints (&optional notoggle)
  "Toggle between styles of hydra hints.
When NOTOGGLE is t, do not toggle `uniline-hint-style',
just put everything in sync."
  (interactive)
  (unless notoggle
    (setq uniline-hint-style
          (if (eq uniline-hint-style t) 1 t)))
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
 ╭─^^────────────╴Uniline╶╴mode╶─────────────────────────────╮
 │^(Ctrl) → ↓ ← ↑^  (overwrite)/draw lines with current brush│
 │^Shift  → ↓ ← ↑^         extend selection                  │
 │^- + = # DEL RET^        change brush style                │
 │^INS^ without selection  insert glyphs, change font        │
 │^INS^ with    selection  handle rectangles                 │
 │^C-h TAB^                switch small/large hints          │
 │^M-: (info \"uniline\")^   info documentation page           │
 │^C-c C-c^                quit uniline                      │
 ╰─^^────────────────────────────────────────────────────────╯")))
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
   (uniline--switch-with-table uniline-text-direction
     (nil                    ? )
     (uniline-direction-up↑ ?↑)
     (uniline-direction-ri→ ?→)
     (uniline-direction-dw↓ ?↓)
     (uniline-direction-lf← ?←))
   (uniline--switch-with-table uniline-brush
     (nil    ? )
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
│ \\[uniline-set-brush-1]	for thin   lines	╭─┬─╮
│ \\[uniline-set-brush-2]	for thick  lines	┏━┳━┓
│ \\[uniline-set-brush-3]	for double lines	╔═╦═╗
│ \\[uniline-set-brush-block]	for blocks		▙▄▟▀
│ \\[uniline-set-brush-0]	to erase lines
│ \\[uniline-set-brush-nil]	to move cursor without drawing
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
├─Tweak 1/4 line─────────────╴
│ \\[uniline-hydra-arrows/uniline-rotate-ri→]	change ¼ line → right
│ \\[uniline-hydra-arrows/uniline-rotate-lf←]	change ¼ line ← left
│ \\[uniline-hydra-arrows/uniline-rotate-up↑]	change ¼ line ↑ up
│ \\[uniline-hydra-arrows/uniline-rotate-dw↓]	change ¼ line ↓ down
├─Text direction─────────────╴
│ Usually when typing text, cursor moves to the right.
│ \\[uniline-hydra-arrows/uniline-text-direction-ri→-and-exit]	text goes right→
│ \\[uniline-hydra-arrows/uniline-text-direction-lf←-and-exit]	text goes left ←
│ \\[uniline-hydra-arrows/uniline-text-direction-up↑-and-exit]	text goes up   ↑
│ \\[uniline-hydra-arrows/uniline-text-direction-dw↓-and-exit]	text goes down ↓
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
│ \\[uniline-hydra-macro-exec/body] then → ← ↑ ↓ : directional call of last keyboard macro
╰────────────────────────────╴
╭╴Alternate styles───────────╴
│ highlight a region (a rectangle) then \\<uniline-mode-map>\\[uniline-hydra-choose-body] \\<uniline-hydra-moverect/keymap>\\[uniline-hydra-moverect/uniline-hydra-alt-styles/body-and-exit]\\<uniline-hydra-alt-styles/keymap>
│ This enters a menu where alternative styles are applied
│ to the rectangular selection
│ \\[uniline-hydra-alt-styles/uniline-change-style-dot-3-2] make 3 dots vertical, 2 dots horizontal lines
│ \\[uniline-hydra-alt-styles/uniline-change-style-dot-4-4] make 4 dots vertical and horizontal lines
│ \\[uniline-hydra-alt-styles/uniline-change-style-hard-corners] convert round corners to hard ones
│ \\[uniline-hydra-alt-styles/uniline-change-style-thin] make thin lines
│ \\[uniline-hydra-alt-styles/uniline-change-style-thick] make thick lines
│ \\[uniline-hydra-alt-styles/uniline-change-style-double] make double lines
│ \\[uniline-hydra-alt-styles/uniline-change-style-standard] come back to standard base line style, including from ASCII art
│ \\[uniline-hydra-alt-styles/uniline--aa2u-rectangle] apply external package aa2u conversion from ASCII art to UNICODE
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
    ([left ]   . uniline-write-lf←)
    ([up   ]   . uniline-write-up↑)
    ([down ]   . uniline-write-dw↓)
    ([C-right] . uniline-overwrite-ri→)
    ([C-left ] . uniline-overwrite-lf←)
    ([C-up   ] . uniline-overwrite-up↑)
    ([C-down ] . uniline-overwrite-dw↓)
    ([insert]      . uniline-hydra-choose-body)
    ([insertchar]  . uniline-hydra-choose-body)
    ([?\r]           . uniline-set-brush-nil)
    ([delete]        . uniline-set-brush-0)
    ([deletechar]    . uniline-set-brush-0)
    ("-"             . uniline-set-brush-1)
    ([kp-subtract]   . uniline-set-brush-1)
    ("+"             . uniline-set-brush-2)
    ([kp-add]        . uniline-set-brush-2)
    ("="             . uniline-set-brush-3)
    ("#"             . uniline-set-brush-block)
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
    ("Overwrite"
     ["overwrite right" uniline-overwrite-ri→ t]
     ["overwrite left"  uniline-overwrite-lf← t]
     ["overwrite up"    uniline-overwrite-up↑ t]
     ["overwrite down"  uniline-overwrite-dw↓ t])
    "----"
    ["─ light brush"   uniline-set-brush-1     :style radio :selected (eq uniline-brush 1     )]
    ["━ heavy brush"   uniline-set-brush-2     :style radio :selected (eq uniline-brush 2     )]
    ["═ double brush"  uniline-set-brush-3     :style radio :selected (eq uniline-brush 3     )]
    ["▞ blocks brush"  uniline-set-brush-block :style radio :selected (eq uniline-brush :block)]
    ["eraser brush"    uniline-set-brush-0     :style radio :selected (eq uniline-brush 0     )]
    ["inactive brush"  uniline-set-brush-nil   :style radio :selected (eq uniline-brush nil   )]
    "----"
    ("Insert glyph"
     ["insert arrow ▷ ▶ → ▹ ▸ ↔" uniline-insert-fw-arrow  :keys "INS a"]
     ["insert square □ ■ ◇ ◆ ◊"  uniline-insert-fw-square :keys "INS s"]
     ["insert oshape · ● ◦ Ø ø"  uniline-insert-fw-oshape :keys "INS o"]
     ["insert cross ╳ ÷ × ± ¤"   uniline-insert-fw-cross  :keys "INS x"])
    ("Rotate arrow, tweak ¼ line"
     ["rotate arrow, tweak ¼ line → right" uniline-rotate-ri→ :keys "INS S-<right>"]
     ["rotate arrow, tweak ¼ line ← left"  uniline-rotate-lf← :keys "INS S-<left>" ]
     ["rotate arrow, tweak ¼ line ↑ up"    uniline-rotate-up↑ :keys "INS S-<up>"   ]
     ["rotate arrow, tweak ¼ line ↓ down"  uniline-rotate-dw↓ :keys "INS S-<down>" ])
    ("Rectangular region" :active (region-active-p)
     ["move selection right" uniline-move-rect-ri→ :keys "INS <right>"]
     ["move selection left"  uniline-move-rect-lf← :keys "INS <left>" ]
     ["move selection up"    uniline-move-rect-up↑ :keys "INS <up>"   ]
     ["move selection down"  uniline-move-rect-dw↓ :keys "INS <down>" ]
     "----"
     ["copy"         uniline-copy-rectangle :keys "INS c"]
     ["kill"         uniline-kill-rectangle :keys "INS k"]
     ["yank, paste"  uniline-yank-rectangle :keys "INS y"]
     "----"
     ["trace rectangle inside selection" uniline-draw-inner-rectangle :keys "INS r"]
     ["trace rectangle around selection" uniline-draw-outer-rectangle :keys "INS R"]
     ["overwrite rectangle inside selection" uniline-overwrite-inner-rectangle :keys "INS C-r"]
     ["overwrite rectangle around selection" uniline-overwrite-outer-rectangle :keys "INS C-R"]
     ["fill" uniline-fill-rectangle :keys "INS i"])
    ("Alternate styles" :active (region-active-p)
     ["thin lines"          uniline-change-style-thin     :keys "INS s -"]
     ["thick lines"         uniline-change-style-thick    :keys "INS s +"]
     ["double lines"        uniline-change-style-double   :keys "INS s ="]
     ["3 dots vert, 2 dots horiz" uniline-change-style-dot-3-2 :keys "INS s 3"]
     ["4 dots vert & horiz" uniline-change-style-dot-4-4  :keys "INS s 4"]
     ["hard corners"    uniline-change-style-hard-corners :keys "INS s h"]
     ["back to standard"    uniline-change-style-standard :keys "INS s 0"]
     ["aa2u (ext. package)" uniline--aa2u-rectangle       :keys "INS s a"])
    ("Fill & contour"
     ["contour"         uniline-contour                 :keys "INS c"]
     ["contour overw"  (uniline-contour t)              :keys "INS C"]
     ["fill" (uniline-fill (uniline--choose-fill-char)) :keys "INS i"])
    ("Text insertion direction"
     ["→ right" uniline-text-direction-ri→ :keys "INS C-<right>" :style radio :selected (eq uniline-text-direction (uniline-direction-ri→))]
     ["← left"  uniline-text-direction-lf← :keys "INS C-<left> " :style radio :selected (eq uniline-text-direction (uniline-direction-lf←))]
     ["↑ up"    uniline-text-direction-up↑ :keys "INS C-<up>   " :style radio :selected (eq uniline-text-direction (uniline-direction-up↑))]
     ["↓ down"  uniline-text-direction-dw↓ :keys "INS C-<down> " :style radio :selected (eq uniline-text-direction (uniline-direction-dw↓))])
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
    ["info" (info "uniline") :keys "M-: (info \"uniline\")"]
    ["quit Uniline Mode" uniline-mode t] ))

(provide 'uniline)
;;; uniline.el ends here
