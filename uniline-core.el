;;; uniline-core.el --- Add▶ ■─UNICODE based diagrams─■ to▶ ■─text files─■ -*- coding:utf-8; lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Thierry Banel

;; Author: Thierry Banel tbanelwebmin at free dot fr
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
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
;;╭─Pure text────────────────□
;;│ UNICODE characters are available to draw nice boxes and lines.
;;│ They come in 4 flavours: thin, thick, double, and quadrant-blocks.
;;│ Uniline makes it easy to draw and combine all 4 flavours.
;;│ Use the arrows on the keyboard to move around leaving a line behind.
;;╰──────────────────────────╮
;;╭─Minor mode───────────────╯
;;│ Uniline is a minor mode.  Enter it with:
;;│   M-x uniline-mode
;;│ Leave it with:
;;│   C-c C-c
;;╰──────────────────────────╮
;;╭─Fonts────────────────────╯
;;│ A font able to displays the needed UNICODE characters have to
;;│ be used.  It works well with the following families:
;;│ - DejaVu Sans Mono
;;│ - Unifont
;;│ - Hack
;;│ - JetBrains Mono
;;│ - Cascadia Mono
;;│ - Agave
;;│ - JuliaMono
;;│ - FreeMono
;;│ - Iosevka Comfy Fixed, Iosevka Comfy Wide Fixed
;;│ - Aporetic Sans Mono, Aporetic Serif Mono
;;│ - Source Code Pro
;;╰──────────────────────────╮
;;╭─UTF-8────────────────────╯
;;│ Also, the encoding of the file must support UNICODE.
;;│ One way to do that, is to add a line like this one
;;│ at the top of your file:
;;│   -*- coding:utf-8; -*-
;;╰──────────────────────────╮
;;╭─Hydra or Transient───────╯
;;│ Uniline comes with two flavours of user interfaces:
;;│ Hydra and Transient.
;;│ Both versions are compiled when installing the package.
;;│
;;│ Then one or the other packages must be loaded (not both)
;;│ for example with:
;;│   (require 'uniline-hydra)
;;│ or
;;│   (use-package uniline-hydra
;;│     :bind ("C-<insert>" . uniline-mode))
;;│
;;│ This file, uniline-core.el, is the largest one, the one
;;│ implementing all the core functions independent from
;;│ Hydra or Transient
;;╰──────────────────────────□

;;; Requires:
(require 'cl-lib)
(require 'rect)
(cl-proclaim '(optimize (speed 3) (safety 1)))

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

;;;╭──────────────────────╮
;;;│Small helper functions│
;;;╰──────────────────────╯

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
            (if (fixnump dir)
                (aref vec dir)
              `(aref ,vec ,dir)))
        ;; create a plist for versatile lookup
        (let ((plist
               (cl-loop
                for c in body
                collect (eval (car c))
                collect (if lambda
                            (funcall lambda (eval (car c)))
                          (eval (cadr c))))))
          (if (fixnump dir)
              (plist-get plist dir)
            `(plist-get ',plist ,dir))))))
  )

(eval-when-compile ; not needed at runtime
  (defmacro uniline--reverse-direction (dir)
    "Reverse DIR.
DIR is any of the 4 `uniline-direction-*'.
Exchange left with right, up with down."
    `(% (+ 2 ,dir) 4))

  (defmacro uniline--turn-right (dir)
    "Return DIR turned 90° clockwise.
DIR & returned values are in [0,1,2,3]."
    `(% (1+ ,dir) 4))

  (defmacro uniline--turn-left (dir)
    "Return DIR turned 90° anti-clockwise.
DIR & returned values are in [0,1,2,3]."
    `(% (+ 3 ,dir) 4))
  )

(defmacro uniline-move-to-column (x)
  "Move to column X staying on the same line.
Add blanks if line is too short.
Move to 0 if X negative."
  ;; x is never numeric in the Uniline calls
  ;; so it is pointless to optimize this case
  `(move-to-column (max ,x 0) t))

(defmacro uniline-move-to-delta-column (x)
  "Move X characters, staying on the same line.
Add blanks if line is too short.
X may be negative to move backward.
Move to 0 if target is beyond the left border of buffer."
  (cond
   ((eq x  0) ()) ;; this case never happens in Uniline calls
   ((eq x  1) `(move-to-column      (1+ (current-column)   )    t))
   ((eq x -1) `(move-to-column (max (1- (current-column)   ) 0) t))
   ((and
     (numberp x)
     (>= x  0))
    (progn    `(move-to-column      (+  (current-column) ,x)    t)))
   (t         `(move-to-column (max (+  (current-column) ,x) 0) t))))

(defcustom uniline-infinite-up↑ nil
  "Is the buffer infinitely extensible in the upper direction?
If not, the begining of buffer is a hard limit, as usual in most
Emacs modes.
Note that if the buffer is narrowed to a region, for example through
the use of C-x n n, then the narrowed region may grow
in both the upper and lower direction by automatic insertion of
blank lines."
  :type 'boolean
  :group 'uniline)

(defun uniline--forward-line-force (y p c)
  "Helper function to move cursor Y lines.
Create lines at the end of the buffer if there
are not enough lines to honor Y.
Y may be negative.
Also create lines at the begining of the buffer
if Y is too negative,
and if uniline-infinite-up↑ is not nil.
Does not preserve current column,
instead leave cursor at column C.
P may be (point) for a relative Y move,
or (point-min) for an absolute Y move."
  ;; here we fix a bug in the return of (forward-line):
  ;; when (forward-line) cannot move enough lines
  ;; because it is stuck at the end of buffer,
  ;; it erroneously returns one less missing forwards
  ;; but the bug does not appears if the end-of-buffer
  ;; is at the beginning-of-line
  ;; so here we get out of the corner-case of the bug,
  ;; by ensuring that there is an empty
  ;; line at the end of buffer.
  (goto-char (point-max))
  (or (bolp) (insert ?\n))
  (goto-char p)
  (let ((n (forward-line y)))
    (if (>= n 0)
        (insert-byte ?\n n)
      (when uniline-infinite-up↑
        (insert-byte ?\n (- n))
        (forward-line n))))
  (move-to-column c t))

(defmacro uniline-move-to-line (y)
  "Move to line Y, while staying on the same column.
Create blank lines at the end of the buffer if needed,
or blank characters at the end of target line.
Y=0 means first line in buffer.
Y negative means to create that many lines at the
beginning of the buff, if permitted by uniline-infinite-up↑"
  `(uniline--forward-line-force ,y (point-min) (current-column)))

(defmacro uniline-move-to-delta-line (y)
  "Move Y lines while staying on the same column.
Create blank lines at the end of the buffer if needed,
or blank characters at the end of target line.
Y may be negative to move backward.
In this case, blank lines may be added at the beginning
of buffer, if permitted by uniline-infinite-up↑"
  `(uniline--forward-line-force ,y (point)     (current-column)))

(defmacro uniline-move-to-lin-col (y x)
  "Move to line Y and column X.
Create blank lines at the end or the beginning of
the buffer if needed, or blank characters at the
end of target line if needed.
Y=0 means first line of buffer.
X=0 means first column of buffer."
  `(uniline--forward-line-force ,y (point-min) ,x))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--move-in-direction (dir &optional nb)
    "Move NB characters in direction DIR.
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
;; Uniline has 13 constant hash-tables. We want to make them
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
;;  - Unfortunately, the specified size is not retained in the
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
    (declare (indent 1) (doc-string 5))
    `(defconst ,name
       (let ((table (make-hash-table :size ,size :test ,test)))
         (dolist (pair ,pairs)
           (puthash (car pair) (cdr pair) table))
         table)
       ,doc)))

;; Display useful metrics to tune constant hash-tables
;; those created by `uniline--defconst-hash-table'
;; Useful only in development cycle.
(if nil
    (defun uniline--hash-tables-metrics ()
      (interactive)
      (switch-to-buffer "*hash-tables metrics*")
      (erase-buffer)
      (local-set-key "g" 'uniline--hash-tables-metrics)
      (insert "\
try to make a single element list in the last column (collision-less)
by adjusting table size                       ╰───┬╯
(3th parameter of `uniline--defconst-hash-table') ╰───────────╮
type g to refresh                                             │
                                                          ╭───┴───╮
               table                buckets  resize       histogram
               ╰─┬─╯                ╰──┬──╯  ╰─┬──╯       ╰───┬───╯
                 │             entries │ index │ threshold    │
                 │             ╰───┬─╯ │ ╰─┬─╯ │ ╰───┬───╯    │
╭────────────────┴───────────────╮╭┴─╮╭┴─╮╭┴─╮╭┴─╮╭──┴──╮╭────┴──────────▷
")
      (cl-loop
       for table in
       '(uniline--char-to-4halfs
         uniline--glyphs-reverse-hash-fw
         uniline--glyphs-reverse-hash-bw
         uniline--char-to-4quadb1
         uniline--keystroke-to-dir-shift
         uniline--char-to-dot-3-2-char
         uniline--char-to-dot-4-4-char
         uniline--char-to-standard-char
         uniline--char-to-hard-corner-char
         uniline--char-to-thin-char
         uniline--char-to-thick-char
         uniline--char-to-double-line
         uniline--unicode-to-ascii)
       do
       (let ((hash (eval table)))
         (insert
          (format
           "│%-33s│%3d│%3d│%3d│%3g│%5g│%s\n"
           table
           (hash-table-count hash)
           (hash-table-size hash)
           (internal--hash-table-index-size hash)
           (hash-table-rehash-size hash)
           (hash-table-rehash-threshold hash)
           (internal--hash-table-histogram hash)))))))

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
;;
;; Example, the character ┖ is made of
;; - half a thick line in the north direction:
;;   value 2 shifted by 0 positions╶──────────╮
;; - half a thin line in the east direction:  │
;;   value 1 shifted by 2 positions╶╮         │
;;                             ╭┬───╯         │
;;                             ││╭┬───────────╯
;; therefore its bit-code is 0b0110 = 6
;;
;; So a `4halfs' number goes from 0 to 4x4x4x4 = 256.
;; This representation is great because it is easily handled by
;; the bits manipulation primitives: `logior', `logand', `ash'.

(eval-when-compile ; not needed at runtime
  (defmacro uniline--shift-4half (4half to-dir &optional from-dir)
    "Shift 4HALF bits in TO-DIR direction.
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
If FROM-DIR is given, the 4HALF bits pattern is supposed to be
shifted in the FROM-DIR direction. Otherwise, it is not shifted.
The result is the bit pattern 4HALF << (2*(TO-DIR - FROM-DIR)).
This macros handles the cases where any of its parameter is numeric,
to fold the result as much as possible, often yielding to just a number."
    (condition-case nil
        (setq to-dir (eval to-dir))     ;; fold if to-dir is a numerical sexpr
      (error nil))                      ;; otherwise leave dir alone
    (condition-case nil
        (setq from-dir (eval from-dir)) ;; fold if to-dir is a numerical sexpr
      (error nil))                      ;; otherwise leave dir alone
    (let ((dir
           (if from-dir
               (if (and (fixnump to-dir) (fixnump from-dir))
                   (- to-dir from-dir)
                 `(- ,to-dir ,from-dir))
             to-dir)))
      (cond
       ((eq dir 0) 4half)
       ((fixnump dir)
        (if (fixnump 4half)
            (ash  4half  (* 2  dir))
          `( ash ,4half ,(* 2  dir))))
       (t `( ash ,4half  (* 2 ,dir))))))

  (defmacro uniline--extract-reverse-4half (4half dir)
    "Extract a 2 bits pattern from the 8 bits pattern 4HALF.
Extract the 2 bits in reverse direction of DIR,
and shift them in direction DIR."
    (let ((odir (uniline--reverse-direction dir)))
      `(uniline--shift-4half
        (logand ,4half ,(uniline--shift-4half 3 odir))
        ,dir
        ,odir)))

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
     (uniline--shift-4half (car    urld) uniline-direction-up↑)
     (uniline--shift-4half (cadr   urld) uniline-direction-ri→)
     (uniline--shift-4half (caddr  urld) uniline-direction-dw↓)
     (uniline--shift-4half (cadddr urld) uniline-direction-lf←)))

  (defun uniline--unpack-4halfs (4halfs)
    "Inverse of `uniline--pack-4halfs'."
    (list
     (logand (uniline--shift-4half 4halfs 0 uniline-direction-up↑) 3)
     (logand (uniline--shift-4half 4halfs 0 uniline-direction-ri→) 3)
     (logand (uniline--shift-4half 4halfs 0 uniline-direction-dw↓) 3)
     (logand (uniline--shift-4half 4halfs 0 uniline-direction-lf←) 3))))

(eval-when-compile ; not used at runtime
  (defconst uniline--list-of-available-halflines
    '(;;╭───────────────unicode char
      ;;│  ╭─┬─┬─┬──────4half description
      ;;▽  ▽ ▽ ▽ ▽
      (?\t 0 0 0 0 ) ;; TAB character     considered as a space
      ( ?  0 0 0 0 ) ;; NO-BREAK SPACE    considered as a space
      (8200 0 0 0 0) ;; PUNCTUATION SPACE considered as a space
      ( ?  0 0 0 0 ) ;; real space comes AFTER the exotic spaces
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
  ;; the UNICODE standard does not define all combinations of
  ;; double lines with thin or thick lines,
  ;; therefore Uniline falls back to the nearest available UNICODE
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
        (cl-loop
         for x in
         '(
           ( ?┆ 1 0 1 0 )
           ( ?┇ 2 0 2 0 )
           ( ?┄ 0 1 0 1 )
           ( ?┅ 0 2 0 2 ))
         for i = (uniline--pack-4halfs (cdr x))
         for c = (aref table i)
         do
         (if (numberp c) (aset table i (setq c (vector c nil nil))))
         (aset c 1 (car x)))
        (cl-loop
         for x in
         '(
           ( ?┊ 1 0 1 0 )
           ( ?┋ 2 0 2 0 )
           ( ?┈ 0 1 0 1 )
           ( ?┉ 0 2 0 2 ))
         for i = (uniline--pack-4halfs (cdr x))
         for c = (aref table i)
         do
         (if (numberp c) (aset table i (setq c (vector c nil nil))))
         (aset c 2 (car x)))
        table))
    "Convert a 4halfs bits description to a UNICODE character.
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
which in turn is converted to ┕.
All entries in this vector are integers, each repesenting a UNICODE,
except 4 of them:
they are vectors of length 3 containing the 3 variants of vertical
and horizontal lines, thin or thick, those variants being plain,
three dots, four dots."))

(eval-and-compile
  (uniline--defconst-hash-table uniline--char-to-4halfs
    (eval-when-compile
      (cl-loop
       for x in uniline--list-of-available-halflines
       collect
       (cons (car x)
             (uniline--pack-4halfs (cdr x)))))
    256 'eq
    ;; the setting 256 'eq
    ;; creates 1 collision between almost never used characters
    ;; this is the best that can be done
    "Convert a UNICODE character to a 4halfs description.
The UNICODE character is supposed to represent
a combination of half lines in 4 directions
and in 4 brush styles.
The retrieved value is the 4halfs description (UP RI DW LF)
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
without the fall-back characters.
There are entries for the 8 dotted lines: ┆ ┇ ┄ ┅ ┊ ┋ ┈ ┉ "))

(eval-and-compile
  ;; the brushes are local variableq, meaning that several Uniline
  ;; session in the same Emacs have different brushes.
  (defvar-local uniline-brush 1
    "Controls the style of line.
Possible values are as follows:
nil: no action except cursor movements
  0: erase,
  1: simple line     like ╰─┤
  2: thick  line     like ┗━┫
  3: double line     like ╚═╣
:block block drawing like ▙▄▟▀")

  (defvar-local uniline-brush-dots 0
    "Sub brush to control whether lines are plain or dotted.
0: plain lines
1: 3 dots vertical, 2 dots horizontal
2: 4 dots both vertical & horizontal")

  (defun uniline--4halfs-to-char-aref (4halfs)
    "Access the array `uniline--4halfs-to-char' through this function.
Because sometimes an indirection is needed to desambiguate between
several versions of a glyph. For instance there are at least 3
versions of a vertical thin line:
- │ plain line
- ┆ 3 dotted line
- ┊ 4 dotted line"
    (let ((c (aref uniline--4halfs-to-char 4halfs)))
      (if (not (numberp c)) ;; `not' is on purpose to make a shorter bytecode
          (aref c uniline-brush-dots)
        c)))

)

;;;╭───────────────────────────────────────╮
;;;│Generate fallbacks for missing UNICODEs│
;;;╰───────────────────────────────────────╯

(when nil

  ;; This inactive code was used to generate the
  ;;   `uniline--list-of-double-halflines' list above.

  ;; As the ╬ double line glyphs combinations were not all defined
  ;; in the UNICODE standard, a penalty system was applied to look
  ;; for the closest possible alternate glyph.

  ;; For example, ├ and ╠ exist, but a mixture of both do no exist,
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
          (unless (uniline--4halfs-to-char-aref
                   (uniline--pack-4halfs (list u1 r1 d1 l1)))
            (let ((m 9999)
                  m0
                  u3 r3 d3 l3)
              (dotimes (u2 4)
                (dotimes (r2 4)
                  (dotimes (d2 4)
                    (dotimes (l2 4)
                      (when (uniline--4halfs-to-char-aref
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
                       (uniline--4halfs-to-char-aref
                        (uniline--pack-4halfs (list u3 r3 d3 l3))))))))))))

;;;╭────────────────────────────────────────────────────────╮
;;;│Reference tables of △▶↓□◆● arrows & other UNICODE glyphs│
;;;╰────────────────────────────────────────────────────────╯

(eval-when-compile ; helper constant not needed at runtime
  (defconst uniline--glyphs-tmp
    '(
      ;; arrows
      ;; they are directional,
      ;; so each entry define the four directions characters╶╮
      ;;    ╭──┬──┬──┬───────────────────────────────────────╯
      ;;    ╵  ╵  ╵  ╵
      (a   ?△ ?▷ ?▽ ?◁)      ;; white *-pointing triangle
      (a   ?▲ ?▶ ?▼ ?◀)      ;; black *-pointing triangle
      (a   ?↑ ?→ ?↓ ?←)      ;; *wards arrow
      (a   ?▵ ?▹ ?▿ ?◃)      ;; white *-pointing small triangle
      (a   ?▴ ?▸ ?▾ ?◂)      ;; black *-pointing small triangle
      (a   ?↕ ?↔ ?↕ ?↔)      ;; up down arrow, left right arrow

      ;; Those commented-out arrows are monospaces and supported
      ;; by the 6 fonts.  But they do not have 4 directions.
      ;;(a   ?‹ ?› ?› ?‹)      ;; single *-pointing angle quotation mark

      ;; squares
      ;; those glyphs are not directional,
      ;; hence each entry has a single character╶╮
      ;;    ╭────────────────────────────────────╯
      ;;    ╵
      (s   ?□)      ;; white square
      (s   ?■)      ;; black square
      (s   ?▫)      ;; white small square
      (s   ?▪)      ;; black small square
      ;;(s   ?◇)      ;; white diamond ;; glitch with Source Code Pro
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
      (x   ?╱)      ;; box drawings light diagonal upper right to lower left
      (x   ?╲)      ;; box drawings light diagonal upper left to lower right
      (x   ?÷)      ;; division sign
      (x   ?×)      ;; multiplication sign
      (x   ?±)      ;; plus-minus sign
      (x   ?¤)      ;; currency sign

      ;; 5 shades of grey
      (g   ? )
      (g   ?░)
      (g   ?▒)
      (g   ?▓)
      (g   ?█)

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
      (let ((r (cl-copy-list uniline--glyphs-tmp)))
      ;; nconc is used to create a circular list on purpose
        (nconc r r)))
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

(uniline--defconst-hash-table uniline--glyphs-reverse-hash-fw
  (eval-when-compile
    (uniline--make-glyph-hash uniline--glyphs-fw))
  128 'equal ; `equal' instead of `eq' to lower table size without collisions
  "Same as `uniline--glyphs-fw' reversing keys & values.")

(uniline--defconst-hash-table uniline--glyphs-reverse-hash-bw
  (eval-when-compile
    (uniline--make-glyph-hash uniline--glyphs-bw))
  128 'equal ; `equal' instead of `eq' to lower table size without collisions
  "Same as `uniline--glyphs-bw' reversing keys & values.")

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
;;  2^0╶╮       ╭╴2^1
;;      ╰┲╾╮ ╭╼┱╯
;;       ╿▘│ │▝╿
;;       ╰─╯ ╰─╯
;;       ╭─╮ ╭─╮
;;       ╽▖│ │▗╽
;;      ╭┺╾╯ ╰╼┹╮
;;  2^2╶╯       ╰─2^3
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
  (uniline--defconst-hash-table uniline--char-to-4quadb1
    (eval-when-compile
      (append
       '((?\t .  0)  ;; TAB considered as space character
         (?    . 0)  ;; NO-BREAK SPACE
         (8200 . 0)) ;; PUNCTUATION SPACE
       (cl-loop
        for c across uniline--4quadb-to-char
        for i from 0
        collect (cons c i))))
    64 'eq
    "Convert a UNICODE character to a quadrant bitmap.
Reverse of `uniline--4quadb-to-char'"))

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
        (cl-loop for i from 0 to 3       ;        ▽  entries will be zero anyway
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
                (aset subtable      ;  │ and if bit=1, get entry╶─╮
                      i             ;  ╰─╮                      ╭─╯
                      (logior       ;    ▽                      ▽
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
    (condition-case nil
        (setq dir (eval dir)) ;; fold if dir is a numerical sexpr
      (error nil))            ;; otherwise leave dir alone
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
  ;; automatic untabification with (move-to-column … t)
  ;; is not enough when cursor goes right onto the beginning of a TAB
  (if (eq (char-after) ?\t)
      (untabify (point) (1+ (point))))
  ;; `insert' before `delete-char' to preserve `point-marker'
  (insert char)
  (or (eolp) (delete-char 1))
  (backward-char))

;;;╭────────────────────────────────────────────────────────╮
;;;│Low level management of ┏╾╯half-lines UNICODE characters│
;;;╰────────────────────────────────────────────────────────╯

(eval-when-compile ; not needed at runtime
  (defmacro uniline--insert-4halfs (4halfs)
    "Insert at (point) a UNICODE like ┬.
The UNICODE character is described by the 4HALFS bits pattern.
The (point) does not move."
    `(uniline--insert-char
      (uniline--4halfs-to-char-aref ,4halfs))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--insert-4quadb (4quadb)
    "Insert at (point) a UNICODE like ▙.
The UNICODE character is described by the 4QUADB bits pattern.
The (point) does not move."
    `(uniline--insert-char
      (aref uniline--4quadb-to-char ,4quadb))))

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
  (let ((bits (uniline--char-to-4quadb (uniline--char-after))))
    (cond
     (bits
      (uniline--insert-4quadb (logior bits uniline--which-quadrant)))
     (force
      (uniline--insert-4quadb              uniline--which-quadrant)))))

;;;╭────────────────────────────────╮
;;;│Test blanks in the neighbourhood│
;;;╰────────────────────────────────╯

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
   (let ((c (char-after p)))
     (or
      (memq c '(?\n nil))
      ;; the case c==nil never happens in Uniline calls
      (gethash c uniline--char-to-4halfs)
      (uniline--char-to-4quadb c)))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--neighbour-point (dir)
    "Return the (point) one char away from current (point) in DIR direction.
Return nil if no such point exists because it would fall outside the buffer.
The buffer is not modified.
This macro seems large, but actually it is a bag of 4 small algorithms.
Just one out of those 4 algorithms is retrieved on a call."
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

(eval-when-compile ;; not used at runtime
  ;; a (defmacro) vs. a (defsubst) saves 2 bytecodes
  (defmacro uniline--blank-neighbour4 (dir)
  "Return non-nil if the quarter point cursor can move in DIR
while staying on the same (point)."
  ;; Try evaluating
  ;;   (macroexpand-all '(uniline--blank-neighbour4 dir))
  ;; or
  ;;   (disassemble     '(uniline--blank-neighbour4 dir))
  ;; You will see a short 9 byte-codes function which references
  ;; a constant vector. This is the fastest it can be.
  (condition-case nil
      (setq dir (eval dir)) ;; fold if dir is a numerical sexpr
    (error nil))            ;; otherwise leave dir alone
  `(eq
    (logand
     uniline--which-quadrant
     (uniline--switch-with-table ,dir
       ;; This lambda computes the values in the resulting lookup table
       ;; for each entry. It is run at compile-time, never at run-time.
       (lambda (dir)
         (uniline--4quadb-pushed
          dir
          (uniline--char-to-4quadb ?█)         ; this is constant 15 = 0b1111
          ))
       (uniline-direction-up↑)
       (uniline-direction-ri→)
       (uniline-direction-dw↓)
       (uniline-direction-lf←)))
    0)))

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

;; `uniline--write-one-4halfs-impl' and `uniline--write-one-4halfs'
;; work togheter. Why not a single defun or defmacro?
;; Because the macro `uniline--write-one-4halfs' turns
;; its parameter DIR into bits-masks, which makes sense:
;; - on each call, DIR is a known constant like 2
;; - creating the bits-masks out of DIR is done at compile-time
;;   yielding to integers.
;; `uniline--write-one-4halfs-impl' groups the fraction of the algorithm
;; which cannot benefit from compile-time computations, thus
;; it is a defun. This avoids duplicating 4 times (in the 4 directions)
;; the same algorithm.

(defun uniline--write-one-4halfs-impl (brush force 4halfmask 4quadmask)
  "Draw half a line.
If there are too few characters on the row where the line
will be drawn, fill it with blank characters.
Cursor does not move.
BRUSH is `uniline-brush' turned in the direction of drawing.
When FORCE is not nil, overwrite a possible non-4halfs character.
4HALFMASK is a bit-mask to erase 4halfs lines found at (point).
4QUADMASK is a bit-mask to erase 4quads blocks found at (point)."
  (let ((bits
         (gethash (uniline--char-after) uniline--char-to-4halfs)))
    (cond
     ;; 1st case: (char-after) is a line-character like ├,
     ;; or any character if FORCE
     ;; then change a half-line of this character
     ;; for example changing it from ├ to ┽
     (bits
      (uniline--insert-4halfs
       (logior
        (logand bits 4halfmask)
        brush)))
     ;; 2nd case: (char-after) is a block character like ▜,
     ;; and the brush is the eraser
     ;; then clear only half of this character
     ((eq uniline-brush 0)
      (if (setq bits (uniline--char-to-4quadb (uniline--char-after)))
          (uniline--insert-4quadb (logand 4quadmask bits))))
     ;; 3th case: force
     (force
      (uniline--insert-4halfs brush)))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--write-one-4halfs (dir force)
    "Draw half a line in the direction DIR.
If there are too few characters on the row where the line
will be drawn, fill it with blank characters.
Cursor does not move.
When FORCE is not nil, overwrite a possible non-4halfs character."
    `(uniline--write-one-4halfs-impl
      (uniline--shift-4half uniline-brush ,dir)
      ,force
      ,(lognot (uniline--shift-4half 3 dir))
      ,(uniline--4quadb-pushed
        (uniline--reverse-direction dir)
        (uniline--char-to-4quadb ?█))))) ; this is constant 15 = 0b1111

;; The `uniline--write-impl' function is a macro (instead of a defun)
;; because there is room to make a lot of computations at compile-time.
;; The macro will be called 4 times (with DIR being each of the
;; 4 directions).

(eval-when-compile ; not needed at runtime
  (defmacro uniline--write-impl (dir repeat force)
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
    (let* ((dir (eval dir)) ;; to convert 'uniline-direction-dw↓ into 2
           (odir (uniline--reverse-direction dir)))
      `(progn
         (unless ,repeat (setq ,repeat 1))
         (setq uniline--arrow-direction ,dir)
         (handle-shift-selection)
         (cond
          ((region-active-p)
           ;; region is marked, continue extending it
           (uniline--move-in-direction ,dir ,repeat)
           (setq deactivate-mark nil))

          ((eq uniline-brush :block)
           ;; draw quadrant-blocks ▝▙▄▌
           (uniline--store-undo-quadrant-cursor)
           (cl-loop
            repeat ,repeat
            do
            (if (uniline--blank-neighbour4 ,odir)
                (uniline--move-in-direction ,dir))
            (setq
             uniline--which-quadrant
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

          (uniline-brush
           ;; draw lines ╰──╮
           (cl-loop
            repeat ,repeat
            do
            (uniline--write-one-4halfs ,dir ,force)
            (uniline--move-in-direction ,dir)
            (uniline--write-one-4halfs ,odir ,force)))
          (t
           ;; brush is nil, just move point
           (uniline--move-in-direction ,dir ,repeat)))))))

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
  (uniline--write-impl uniline-direction-up↑ repeat force))

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
  (uniline--write-impl uniline-direction-ri→ repeat force))

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
  (uniline--write-impl uniline-direction-dw↓ repeat force))

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
  (uniline--write-impl uniline-direction-lf← repeat force))

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

;; A naive version of `uniline-fill' would be recursive.
;; Instead, this version is flattened. It manages a stack
;; of points (locations in the buffer), which wait to be
;; drawn upon.
;; The flattened version does not risk overloading the List stack,
;; and is very fast because it avoid function-calls.

(defun uniline-fill (&optional char)
  "Fill a hollow shape with character CHAR.
CHAR is optional, if not given, user is queried.
The hole is the set of contiguous identical characters.
The character at point is used as reference for other
identical characters."
  (interactive)
  (unless char
    (setq char (uniline--choose-fill-char)))

  ;; why is stack initialized with twice the current point?
  ;; the first is for starting the filling process
  ;; the second is for returning to the starting point after filling
  (let ((currentchar (uniline--char-after))
        (stack (list (point) (point))))
    (unless (eq char currentchar)
      (while stack
        (goto-char (pop stack))
        (when (eq (char-after) currentchar) ; not (uniline--char-after) !
          (uniline--insert-char char)
          (let ((p (uniline--neighbour-point uniline-direction-up↑)))
            (if  p (push p stack)))
          (let ((p (uniline--neighbour-point uniline-direction-ri→)))
            (if  p (push p stack)))
          (let ((p (uniline--neighbour-point uniline-direction-dw↓)))
            (if  p (push p stack)))
          (let ((p (uniline--neighbour-point uniline-direction-lf←)))
            (if  p (push p stack))))))))

;;;╭────────────────────────╮
;;;│Undo rectangle selection│
;;;╰────────────────────────╯

;; Rectangle functions operate on a visually highlighted rectangular selection
;; Keeping the selection highlighted
;;  - after changing the rectangle (move, fill, contour, kill, yank…)
;;  - when undoing a rectangle change
;; gives a sense of confidence.
;; To achieve that, we add the point and mark (the selection) into the
;; regular Emacs undo machinery, so as to restore it.

(defun uniline--undo-restore-selection (i p)
  "Function called by the Emacs undo system to restore selection.
I is the mark,
P is the point."
  (set-mark i)
  (goto-char p)
  (activate-mark)
  (rectangle-mark-mode 1)
  (setq deactivate-mark nil))

(defun uniline--record-undo-rectangle-selection ()
  "Add the selection (point and mark) into the Emacs undo stack."
  (setq buffer-undo-list
        `((apply
           uniline--undo-restore-selection
           ,(mark)
           ,(point))
          ,@buffer-undo-list)))

;; Here is the list of all interactive functions which leave
;; the region visually active on the rectangle they just operated on.
;;
;; To correclty undo such rectangle operations, the selection should
;; first be deactivated. This is because some changes in the buffer
;; are on the fringe of the region, and are not prpoperly undone.
;;
;; However deactivating the selection proved to be quite tricky to achieve.
;;
;; Therefore we leave the region highlighted, and we instructs `undo'
;; to disregard the region. The `undo' mechanism provides for that,
;; by looking at the `undo-inhibit-region' property of the `last-command'
;;
;; The following list was hand-compiled by looking for interactive
;; functions which call `uniline--operate-on-rectangle', either directly
;; or through a utility function.

(dolist
    (fun
     '(uniline-move-rect-up↑
       uniline-move-rect-dw↓
       uniline-move-rect-ri→
       uniline-move-rect-lf←
       uniline-fill-rectangle
       uniline-draw-inner-rectangle
       uniline-draw-outer-rectangle
       uniline-yank-rectangle
       uniline-change-style-standard
       uniline-aa2u-rectangle
       uniline-change-style-dot-3-2
       uniline-change-style-dot-4-4
       uniline-change-style-hard-corners
       uniline-change-style-thin
       uniline-change-style-thick
       uniline-change-style-double))
  (put fun 'undo-inhibit-region t))

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
     ← empty space
    ┗╸
A space is leaved empty after translation.
Then the leakage of the two glyphs fills in the gap:
   ╶┬╴
    ╽← filled with leakage
    ┗╸"
    (setq dir (eval dir))
    (let ((odir (uniline--reverse-direction dir)))
      `(let* ((cc (uniline--char-after))
              (4c (gethash cc uniline--char-to-4halfs 0))
              (leak (uniline--extract-reverse-4half 4c ,dir))
              (p (uniline--neighbour-point ,odir)))
         (if p
             (setq
              leak
              (logior
               leak
               (uniline--extract-reverse-4half
                (gethash (uniline--char-after p) uniline--char-to-4halfs 0)
                ,odir))))
         ;; here the case of dotted line is handled:
         ;; `leak' and `4c' may be the same code, for example 02-00-02-00
         ;; denoting 3 possible actual characters: ┇ ┋ ┃
         ;; in this case, we recover and return the actual character in the buffer
         ;; not the syntetic `leak'
         (if (and (eq leak 4c) (not (eq 4c 0)))
             cc
           (uniline--4halfs-to-char-aref leak))))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--compute-leakage-quadb (dir)
    "Compute lines leakage from two directions for 4quadb characters.
When a rectangle moves, it leaves blank chars.
Those chars are filled with leakage from their two neighbours,
in DIR direction, and its opposite.
For instance consider a situation like this:
    ▌
     ← empty space
    ▙
A space is leaved empty after translation.
Then the leakage of the two glyphs fills in E:
    ▌
    ▌← filled with leakage
    ▙"
    (setq dir (eval dir))
    (let ((odir (uniline--reverse-direction dir)))
      `(let ((leak ;; leak from (point)
              (uniline--4quadb-pushed
               ,dir
               (or (uniline--char-to-4quadb (uniline--char-after)) 0)))
             (lean ;; leak from neighbour of (point)
              (let ((p (uniline--neighbour-point ,odir)))
                (and
                 p
                 (uniline--char-to-4quadb (uniline--char-after p))))))
         (aref uniline--4quadb-to-char
               (if (not lean)
                   leak
                 (logior
                  leak
                  (uniline--4quadb-pushed ,odir lean)
                  )))))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--compute-leakage (dir)
    "Compute lines leakage from two directions: DIR and its opposite."
    `(let ((c (uniline--compute-leakage-4halfs ,dir)))
       (if (eq c ? )
           (uniline--compute-leakage-quadb ,dir)
         c))))

(eval-when-compile ; not needed at runtime
  (defmacro uniline--compute-leakage-on-region (dir y begx endx)
    "Compute the leakage of all characters in a region.
Region is BEGX..ENDX x Y..Y, it is one char high.
The region just below is considered when DIR is ↑,
the region just above is considered when DIR is ↓.
Return a string which will be inserted back in the buffer."
    `(cl-loop
      with s = (make-vector (- ,endx ,begx) ? )
      for x from ,begx below ,endx
      for i from 0
      do
      (uniline-move-to-lin-col ,y x)
      (aset s i (uniline--compute-leakage ,dir))
      finally return (concat s))))

(defun uniline--exchange-region-string (y begx endx hand)
  "Replace the region BEGX..ENDX x Y..Y with HAND.
Region is 1 character high.
HAND is a string of the same length as the region.
Return the replaced region as a string."
  (uniline-move-to-line y)
  (let* ((end (progn (uniline-move-to-column endx) (point)))
         (beg (progn (uniline-move-to-column begx) (point)))
         (line (buffer-substring beg end)))
    (delete-region beg end)
    (goto-char beg)
    (insert hand)
    line))

(defun uniline--untabify-rectangle (begy endy)
  "Untabify all lines over which the rectangle operates.
BEGY and ENDY are the line-numbers of the beggining and ending
of the rectangle.
The costly `untabify' function is called only if there are
TAB characters in the region."
  (let ((end
         (progn
           (uniline-move-to-line (1- endy))
           (end-of-line)
           (point)))
        (beg
         (progn
           (uniline-move-to-line begy)
           (beginning-of-line)
           (point))))
    ;; (point) is at beg
    (when (search-forward "\t" end t)
      (untabify beg end))))


(defun uniline-move-rect-up↑ (repeat)
  "Move the rectangle marked by selection one line upper.
Truncate the selection if it touches the upper side of the buffer,
and if `uniline-infinite-up↑' is nil.
REPEAT tells how many characters the rectangle should move,
defaulting to 1.
    ↑ ↑ ↑ ↑
    ░░░░░░░
    ░░░░░░░
    ░░░░░░░
"
  (interactive "P")
  (uniline--record-undo-rectangle-selection)
  (cl-loop
   repeat (or repeat 1)
   do
   (uniline--operate-on-rectangle
    (uniline--untabify-rectangle begy endy)
    (if (and (eq begy 0) uniline-infinite-up↑)
        (save-excursion (uniline-move-to-line -1))
      (setq
       begy (max (1- begy) 0)
       endy      (1- endy)))
    (cl-loop
     with hand = (uniline--compute-leakage-on-region
                  uniline-direction-up↑ endy begx endx)
     for y from endy downto begy
     do (setq hand (uniline--exchange-region-string y begx endx hand))))))

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
  (uniline--record-undo-rectangle-selection)
  (cl-loop
   repeat (or repeat 1)
   do
   (uniline--operate-on-rectangle
    (uniline--untabify-rectangle begy endy)
    (cl-loop
     with hand = (uniline--compute-leakage-on-region
                  uniline-direction-dw↓ begy begx endx)
     for y from begy to endy
     do (setq hand (uniline--exchange-region-string y begx endx hand)))
    (setq
     begy (1+ begy)
     endy (1+ endy)))))

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
  (uniline--record-undo-rectangle-selection)
  (cl-loop
   repeat (or repeat 1)
   do
   (uniline--operate-on-rectangle
    (uniline--untabify-rectangle begy endy)
    (cl-loop
     for y from begy below endy
     do
     (uniline-move-to-lin-col y begx)
     (insert
      (uniline--compute-leakage uniline-direction-ri→))
     (uniline-move-to-column (1+ endx))
     (or (eolp) (delete-char 1)))
    (setq
     begx (1+ begx)
     endx (1+ endx)))))

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
  (uniline--record-undo-rectangle-selection)
  (cl-loop
   repeat (or repeat 1)
   do
   (uniline--operate-on-rectangle
    (uniline--untabify-rectangle begy endy)
    (setq
     begx (max (1- begx) 0)
     endx (max (1- endx) 0))
    (cl-loop
     for y from (1- endy) downto begy
     do
     (uniline-move-to-lin-col y endx)
     (insert
      (prog1
          (uniline--compute-leakage uniline-direction-lf←)
        (uniline-move-to-delta-column 1)))
     (uniline-move-to-column begx)
     (delete-char 1)))))

(defun uniline-fill-rectangle ()
  "Fill the rectangle marked by selection.
Interactively choose the filling character.
See `uniline--choose-fill-char'.
    ░░░░░░░
    ░░░░░░░
    ░░░░░░░
"
  (interactive)
  (uniline--record-undo-rectangle-selection)
  (uniline--operate-on-rectangle
   (set-mark end)  ;; ensure we are inside the region before
   (goto-char beg) ;; asking for the filling character
   (rectangle-mark-mode 1)
   (let ((char (uniline--choose-fill-char)))
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
  (uniline--record-undo-rectangle-selection)
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
  (uniline--record-undo-rectangle-selection)
  (uniline--operate-on-rectangle
   (let ((width  (- endx begx -1))
         (height (- endy begy -1))
         (mark-active nil))             ; otherwise brush is inactive
     (goto-char beg)
     (if (<= begx 0)
         (setq width (1- width))         ; at leftmost side of buffer
       (uniline-move-to-delta-column -1))
     (let ((uniline-infinite-up↑ t))         ; forcefully add a line
       (uniline-move-to-delta-line -1))        ; above the rectangular region
     (if (eq uniline-brush :block)
         (setq
          width  (+ width  width  -1)
          height (+ height height -1)
          uniline--which-quadrant (uniline--char-to-4quadb ?▗)))
     (uniline-move-to-column (1- begx))
     (uniline-write-ri→ width  force)
     (uniline-write-dw↓ height force)
     (uniline-write-lf← width  force)
     (when (> begx 0)
       (uniline-write-up↑ height force)
       (setq begx (1- begx)))
     (setq endx (1+ endx))
     (if (eq begy 0)                    ; an additional line was needed
         (if uniline-infinite-up↑
             (setq endy (+ 2 endy))
           (setq endy (1+ endy))
           (goto-char (point-min))
           (delete-line))
       (setq begy (1- begy))            ; no additionl line
       (setq endy (1+ endy))))))

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
  (rectangle-mark-mode -1)
  (copy-rectangle-as-kill (region-beginning) (region-end))
  (deactivate-mark))

(defun uniline-kill-rectangle ()
  "Kill the selected rectangle.
It differs from the standard Emacs `kill-rectangle'
in that it leaves a rectangle of space characters."
  (interactive)
  (uniline--record-undo-rectangle-selection)
  (copy-rectangle-as-kill (region-beginning) (region-end))
  (clear-rectangle        (region-beginning) (region-end))
  (deactivate-mark))

(defun uniline-yank-rectangle ()
  "Insert a previously cut rectangle.
It differs from the standard Emacs `yank-rectangle'
in that it overwrites the rectangle.
Only non blank characters are copied."
  (interactive)
  (uniline--record-undo-rectangle-selection)
  (uniline--operate-on-rectangle
   (goto-char beg)
   (cl-loop
    for line in killed-rectangle
    do
    (cl-loop
     for char across line
     do
     (unless (eq char ? )
       (uniline--insert-char char))
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

(defvar-local uniline--mode-line-brush nil
  "The current brush suitable for display in the `:lighter'.")

(defvar-local uniline--mode-line-dir nil
  "The current text direction suitable for ddisplay in the `:lighter'.")

(defun uniline--update-mode-line ()
  "Computes the string which appears in the mode-line."
  (setq uniline--mode-line-dir
	(uniline--switch-with-table uniline-text-direction
	  (nil                   " ")
	  (uniline-direction-up↑ "↑")
	  (uniline-direction-ri→ "→")
	  (uniline-direction-dw↓ "↓")
	  (uniline-direction-lf← "←"))
        uniline--mode-line-brush
	(uniline--switch-with-cond uniline-brush
	  (nil    " ")
	  (0      "/")
	  (1
           (uniline--switch-with-table uniline-brush-dots
             (0   "┼")
             (1   "┆")
             (2   "┊")))
	  (2
           (uniline--switch-with-table uniline-brush-dots
             (0   "╋")
             (1   "┇")
             (2   "┋")))
	  (3      "╬")
	  (:block "▞")))
  (force-mode-line-update))

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
  (setq uniline-text-direction (uniline-direction-up↑))
  (uniline--update-mode-line))
(defun uniline-text-direction-ri→ ()
  "Set text insertion direction right→."
  (interactive)
  (setq uniline-text-direction (uniline-direction-ri→))
  (uniline--update-mode-line))
(defun uniline-text-direction-dw↓ ()
  "Set text insertion direction down↓."
  (interactive)
  (setq uniline-text-direction (uniline-direction-dw↓))
  (uniline--update-mode-line))
(defun uniline-text-direction-lf← ()
  "Set text insertion direction left←."
  (interactive)
  (setq uniline-text-direction (uniline-direction-lf←))
  (uniline--update-mode-line))

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
    `(;; ╭───keystroke as used in keyboard macros
      ;; │           ╭─direction of the keystroke
      ;; │           │   ╭─shift-control modifiers
      ;; │           │   ╰─────────────────╮
      ;; ▽           ▽                     ▽
      (  up    . ,(+ uniline-direction-up↑ 0))
      (  right . ,(+ uniline-direction-ri→ 0))
      (  down  . ,(+ uniline-direction-dw↓ 0))
      (  left  . ,(+ uniline-direction-lf← 0))
      (S-up    . ,(+ uniline-direction-up↑ 4))
      (S-right . ,(+ uniline-direction-ri→ 4))
      (S-down  . ,(+ uniline-direction-dw↓ 4))
      (S-left  . ,(+ uniline-direction-lf← 4))
      (C-up    . ,(+ uniline-direction-up↑ 8))
      (C-right . ,(+ uniline-direction-ri→ 8))
      (C-down  . ,(+ uniline-direction-dw↓ 8))
      (C-left  . ,(+ uniline-direction-lf← 8)))))

(uniline--defconst-hash-table uniline--keystroke-to-dir-shift
  (eval-when-compile uniline--directional-keystrokes-table)
  16 'eq
  "Hashtable to convert a directional keystroke into Uniline constants.")
;; it is impossible to guaranty that the table will stay collision-less
;; because keys are symbols, whose hash-values may change from
;; one Emacs session to another

(defconst uniline--dir-shift-to-keystroke
  (eval-when-compile ;; not needed at runtime
    (cl-loop
     with vec = (make-vector
                 (1+
                  (cl-loop
                   for x in uniline--directional-keystrokes-table
                   maximize (cdr x)))
                 nil)
     for r in uniline--directional-keystrokes-table
     do (aset vec (cdr r) (car r))
     finally return vec))
  "Convert Uniline directional constants back into keystrokes.")

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
                    (cl-loop
                     with result = (vconcat last-kbd-macro)
                     for r across-ref result
                     for y = (gethash r uniline--keystroke-to-dir-shift)
                     if y
                     do (setf
                         r
                         (aref
                          uniline--dir-shift-to-keystroke
                          (logior
                           (logand (+ y dir 3) 3)
                           (logand y (eval-when-compile (lognot 3))))))
                     finally return result))))))
    (execute-kbd-macro last-kbd-macro 1)))

;; Run the following cl-loop to automatically write a bunch
;; of 4 interactive functions
;; They have little value, except to be an interface between
;; `easy-menu-define', `defhydra', `transient-define-prefix',
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
  (uniline--update-mode-line))

(defun uniline-set-brush-0 ()
  "Change the current style of line to the eraser.
It means that moving the cursor horizontally erase horizontal
lines, and moving vertically erase vertical lines.  Characters
other than lines or blocks are not touched. As there is no clear
horizontal or vertical lines for block characters, they are
erased unconditionally."
  (interactive)
  (setq uniline-brush 0)
  (uniline--update-mode-line))

(defun uniline-set-brush-0dots ()
  "Change the current style of line to plain."
  (interactive)
  (setq uniline-brush-dots 0)
  (uniline--update-mode-line))

(defun uniline-set-brush-3dots ()
  "Change the current style of line to 3 dots vertical, 2 dots horizontal."
  (interactive)
  (setq uniline-brush-dots 1)
  (uniline--update-mode-line))

(defun uniline-set-brush-4dots ()
  "Change the current style of line to 4 dots vertical & horizontal."
  (interactive)
  (setq uniline-brush-dots 2)
  (uniline--update-mode-line))

(defun uniline-set-brush-dot-toggle ()
  "Toggle dotted lines: plain → 3-2-dots → 4-4-dots → plain."
  (interactive)
  (setq uniline-brush-dots (% (1+ uniline-brush-dots) 3))
  (uniline--update-mode-line))

(defun uniline-set-brush-1 ()
  "Change the current style of line to a single thin line ──."
  (interactive)
  (setq uniline-brush 1)
  (uniline--update-mode-line))

(defun uniline-set-brush-2 ()
  "Change the current style of line to a single thick line ━━."
  (interactive)
  (setq uniline-brush 2)
  (uniline--update-mode-line))

(defun uniline-set-brush-3 ()
  "Change the current style of line to a double line ══."
  (interactive)
  (setq uniline-brush 3)
  (uniline--update-mode-line))

(defun uniline-set-brush-block ()
  "Change the current style of line to blocks ▙▄▟▀."
  (interactive)
  (setq uniline-brush :block)
  (uniline--update-mode-line))

;;;╭─────────────────────────────────────╮
;;;│High level arrows & glyphs management│
;;;╰─────────────────────────────────────╯

(defun uniline--insert-glyph (letter back repeat)
  "Insert or modify a glyph.
LETTER is any of
 `a' for arrows
 `s' for squares
 `x' for crosses
 `o' for o-shapes
 `g' for shades of grey
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
;; `easy-menu-define', `defhydra', `transient-define-prefix',
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
       (x . "cross" )
       (g . "grey"  ))
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
(defun uniline-insert-fw-grey (repeat)
  "Insert or modify a glyph of type grey in forward mode.
REPEAT cycles through glyphs list that many times, default to 1.
See `uniline--insert-glyph'."
  (interactive "P")
  (uniline--insert-glyph 'g nil repeat))
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
(defun uniline-insert-bw-grey (repeat)
  "Insert or modify a glyph of type grey in backward mode.
REPEAT cycles through glyphs list that many times, default to 1.
See `uniline--insert-glyph'."
  (interactive "P")
  (uniline--insert-glyph 'g t repeat))
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
           (ash3dir2 (uniline--shift-4half 3 dir)) ;; 3 is a bit-mask
           (ash1dir2 (uniline--shift-4half 1 dir)) ;; 1 is an increment
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
                      (uniline--4halfs-to-char-aref patnew)
                      uniline--char-to-4halfs)
                     patnew))))
             (uniline--insert-4halfs patnew)))

          ;; If point is on a quarter-char
          ((setq pat (uniline--char-to-4quadb (uniline--char-after)))
           (uniline--insert-4quadb (logxor pat ,dir4))))))))

;; Run the following cl-loop to automatically write a bunch
;; of 4 interactive functions
;; They have little value, except to be an interface between
;; `easy-menu-define', `defhydra', `transient-define-prefix',
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
then change the 4half segment pointing %s.
If character under point is a block, flip one corner of
the block on & off."
                 dir dir)
        (interactive)
        (uniline--rotate-arrow (,(intern (format "uniline-direction-%s" dir)))))))
  (insert "\n;; END -- Automatically generated\n"))

;; BEGIN -- Automatically generated

(defun uniline-rotate-up↑ nil
  "Rotate an arrow or tweak 4halfs.
If character under point is an arrow, turn it up↑.
If character under point is a combination of 4halfs lines,
then change the 4half segment pointing up↑.
If character under point is a block, flip one corner of
the block on & off."
  (interactive)
  (uniline--rotate-arrow (uniline-direction-up↑)))
(defun uniline-rotate-ri→ nil
  "Rotate an arrow or tweak 4halfs.
If character under point is an arrow, turn it ri→.
If character under point is a combination of 4halfs lines,
then change the 4half segment pointing ri→.
If character under point is a block, flip one corner of
the block on & off."
  (interactive)
  (uniline--rotate-arrow (uniline-direction-ri→)))
(defun uniline-rotate-dw↓ nil
  "Rotate an arrow or tweak 4halfs.
If character under point is an arrow, turn it dw↓.
If character under point is a combination of 4halfs lines,
then change the 4half segment pointing dw↓.
If character under point is a block, flip one corner of
the block on & off."
  (interactive)
  (uniline--rotate-arrow (uniline-direction-dw↓)))
(defun uniline-rotate-lf← nil
  "Rotate an arrow or tweak 4halfs.
If character under point is an arrow, turn it lf←.
If character under point is a combination of 4halfs lines,
then change the 4half segment pointing lf←.
If character under point is a block, flip one corner of
the block on & off."
  (interactive)
  (uniline--rotate-arrow (uniline-direction-lf←)))
;; END -- Automatically generated

;;;╭───────╮
;;;│Contour│
;;;╰───────╯

(defcustom uniline-contour-max-steps 10000
  "Maximum number of steps before stopping the contour algorithm.
This is the number of characters drawn with lines╶─┬─══╦══━━┳━╸,
or twice the number of characters drawn with block-characters ▝▙.
This limit may be reached if the text is huge. Of course, the
algorithm may be started again for an additional 10000 steps."
  :type 'natnum
  :group 'uniline)

(defun uniline-contour (&optional force)
  "Draw a contour arround a block of characters.
A block of characters is a contiguous set of non-blank characters.
For the sake of the contour, a non-blank character is any character
not in the 4halfs set or in the 4quadb set.
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

  (let ((dir)
        (start (point-marker))
        (q uniline--which-quadrant)
        (n 0))
    ;; look for a surrounding wall successively in directions lf← up↑ ri→ dw↓.
    ;; stop as soon as hitting a wall.
    ;; then initialize dir-ection turning left from the wall.
    ;; so for instance, if there is empty space toward west lf←,
    ;; and a wall toward north up↑, then initial dir will be set to west lf←.
    ;;      \\\\\\\\\\\\\\\\\
    ;;      ╺━━━━━━╳━━━━━━┓ \
    ;;             ↑      ┃ \
    ;;          ╭──╯──╮   ┃ \
    ;;          │     │   ┃ \
    ;;        ←─╮  ●  ╰──→╳ \
    ;;          ╰──╯  │   ┃ \
    ;;           ╭─╭──╯   ┃ \
    ;;   error╶──╯ ↓      ┃ \
    ;;                    ╹ \
    (and
     (progn
       (setq dir (uniline--turn-left (uniline-direction-lf←)))
       (uniline--blank-neighbour1    (uniline-direction-lf←)))
     (progn
       (setq dir (uniline--turn-left (uniline-direction-up↑)))
       (uniline--blank-neighbour1    (uniline-direction-up↑)))
     (progn
       (setq dir (uniline--turn-left (uniline-direction-ri→)))
       (uniline--blank-neighbour1    (uniline-direction-ri→)))
     (progn
       (setq dir (uniline--turn-left (uniline-direction-dw↓)))
       (uniline--blank-neighbour1    (uniline-direction-dw↓)))
     (error "No border of any shape found around point"))
    (if (eq uniline-brush :block)
        (setq
         q
         (setq
          uniline--which-quadrant
          ;; this huge expression folds to just:
          ;; (aref [8 4 1 2] dir)
          ;; which is as fast as can be at runtime
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
          ;; change current dir-ection in whichever direction there is no wall.
          ;; in this example dir pointed north up↑, and was changed to west lf←
          ;;       \\\\\\\\\\\\\\\\\\\\\\
          ;;       ╺━━━━━━━━╳━━━━━━┓\\\\\
          ;;                ↑      ┃\\\\\
          ;;            ╭───╰───╮  ┃\\\\\
          ;;            │   ╭─╮ │  ┃\\\\\
          ;;  no wall ←─╯   ● │ ╭─→╳\\\\\
          ;;            │     ╰─╯  ╹\\\\\
          ;;            ╰───╮───╮     \\\
          ;;                ↓   ╰error  \
          (let ((d dir))
            (or
             (uniline--blank-neighbour (setq dir (uniline--turn-right dir)))
             (uniline--blank-neighbour (setq dir d))
             (uniline--blank-neighbour (setq dir (uniline--turn-left dir)))
             (uniline--blank-neighbour (setq dir (uniline--turn-left dir)))
             (error "Cursor is surrounded by walls")))
          ;; bumping into the left or upper borders?
          ;; move the (point) silently as if drawing outside the buffer
          ;; until finding a blank (or eolp)) which allows the (point)
          ;; to re-enter the actual buffer.
          (cond
           ;; bump into the left border
           ((and
             (eq dir (uniline-direction-lf←))
             (uniline--at-border-p uniline-direction-lf←)
             (or (not (eq uniline-brush :block))
                 (uniline--blank-neighbour4 uniline-direction-ri→)))
            (while
                (and
                 (eq (forward-line -1) 0)
                 (not (uniline--blank-after (point)))))
            (if (uniline--at-border-p uniline-direction-up↑)
                (setq dir (uniline-direction-up↑)
                      uniline--which-quadrant (uniline--char-to-4quadb ?▝))
              (setq dir (uniline-direction-ri→)
                    uniline--which-quadrant (uniline--char-to-4quadb ?▖))))
           ;; bump into the upper border
           ((and
             (not uniline-infinite-up↑)
             (eq dir (uniline-direction-up↑))
             (uniline--at-border-p uniline-direction-up↑)
             (or (not (eq uniline-brush :block))
                 (uniline--blank-neighbour4 uniline-direction-dw↓)))
            (while
                (progn
                  (forward-char 1)
                  (not (uniline--blank-after (point)))))
            (setq dir (uniline-direction-dw↓))
            (setq uniline--which-quadrant (uniline--char-to-4quadb ?▘)))
           ;; not bumping into a border, so just draw
           (t
            (uniline--write dir force)
            (setq n (1+ n))))
          (and
           (not
            (and (eq (point) (marker-position start))
                 (or (not (eq uniline-brush :block))
                     (eq uniline--which-quadrant q))))
           (< n uniline-contour-max-steps))))
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
             (uniline--insert-char rep)))))))

(uniline--defconst-hash-table uniline--char-to-dot-3-2-char
  '((?─ . ?╌)
    (?┄ . ?╌)
    (?┈ . ?╌)
    (?━ . ?╍)
    (?┅ . ?╍)
    (?┉ . ?╍)
    (?═ . ?╍)
    (?│ . ?┆)
    (?╎ . ?┆)
    (?┊ . ?┆)
    (?┃ . ?┇)
    (?╏ . ?┇)
    (?┋ . ?┇)
    (?║ . ?┇))
  16 'eq
  "Convert to 3 vertical & 2 horizontal dashes")

(defun uniline-change-style-dot-3-2 ()
  "Change plain lines to dashed lines in a rectangular selection.
It retains thickness of the lines.
Vertical lines will be 3-dots, while horizontal will be 2-dots."
  (interactive)
  (uniline--record-undo-rectangle-selection)
  (uniline--change-style-hash uniline--char-to-dot-3-2-char))

(uniline--defconst-hash-table uniline--char-to-dot-4-4-char
  '((?─ . ?┈)
    (?╌ . ?┈)
    (?┄ . ?┈)
    (?━ . ?┉)
    (?╍ . ?┉)
    (?┅ . ?┉)
    (?═ . ?┉)
    (?│ . ?┊)
    (?╎ . ?┊)
    (?┆ . ?┊)
    (?┃ . ?┋)
    (?╏ . ?┋)
    (?┇ . ?┋)
    (?║ . ?┋))
  32 'eq
  "Convert to 4 vertical & 4 horizontal dashes")

(defun uniline-change-style-dot-4-4 ()
  "Change plain lines to dashed lines in a rectangular selection.
It retains thickness of the lines.
Vertical and horizontzl lines will be 4-dots."
  (interactive)
  (uniline--record-undo-rectangle-selection)
  (uniline--change-style-hash uniline--char-to-dot-4-4-char))

(uniline--defconst-hash-table uniline--char-to-standard-char
  '((?╌ . ?─)
    (?┄ . ?─)
    (?┈ . ?─)
    (?╍ . ?━)
    (?┅ . ?━)
    (?┉ . ?━)
    (?╎ . ?│)
    (?┆ . ?│)
    (?┊ . ?│)
    (?╏ . ?┃)
    (?┇ . ?┃)
    (?┋ . ?┃)
    (?┐ . ?╮)
    (?└ . ?╰)
    (?┌ . ?╭)
    (?┘ . ?╯))
  64 'equal ; `equal' instead of `eq' to achieve collision-less table
  "Convert back to base-line style")

(eval-when-compile ;; not used at runtime
  (defmacro uniline--infer-neighbour-4half (dir)
    "Return a 4half to seamlessly connect with neighbour in DIR direction.
Return 0 if there is no neighbour, or if neighbour does not look like
a connecting line or glyph."
    (setq dir (eval dir))
    (let
        ((isvert (memq dir `(,uniline-direction-up↑ ,uniline-direction-dw↓)))
         (ishorz (memq dir `(,uniline-direction-lf← ,uniline-direction-ri→)))
         ;; 1 is thin   half-line in direction DIR ──▷
         (shift1 (uniline--shift-4half 1 dir))
         ;; 1 is double half-line in direction DIR ══▷
         (shift3 (uniline--shift-4half 3 dir)))

      `(let ((neighbour (uniline--neighbour-point ,dir)))
         (if (not neighbour)
             0
           (setq neighbour (uniline--char-after neighbour))
           (let ((b (gethash neighbour uniline--char-to-4halfs)))
             (cond
              (b
               (uniline--extract-reverse-4half b ,dir))
              ((memq neighbour '(?+ ?\\ ?/ ?' ?`))
               ,shift1)
              ((eq neighbour ?#)
               ,shift3)
              ,@(when isvert
                  `(((memq neighbour
                           '(?^ ?v ?V ?| ?△ ?▽ ?▲ ?▼ ?↑ ?↓ ?▵ ?▿ ?▴ ?▾ ?↕))
                     ,shift1)
                    ((eq neighbour ?\")
                     ,shift3)))
              ,@(when ishorz
                  `(((memq neighbour
                           '(?> ?< ?- ?_ ?▷ ?◁ ?▶ ?◀ ?→ ?← ?▹ ?◃ ?▸ ?◂ ?↔))
                     ,shift1)
                    ((eq neighbour ?=)
                     ,shift3)))
              (t 0))))))))

(defun uniline-change-style-standard ()
  "Change fancy lines styles to standard ones in a rectangular selection.
This includes dashed lines, which become plain while preserving thickness,
and hard corners which become rounded.
Also ASCII-art is converted to UNICODE-art."
  (interactive)
  (uniline--record-undo-rectangle-selection)
  (uniline--change-style-hash uniline--char-to-standard-char)
  ;; Hereafter, we handle ASCII characters used to draw sketches
  ;; We distinguish two cases for those characters
  ;; - when they should be left alone as ASCII chars
  ;; - when they are part of a future UNICODE line
  ;; ASCII chars are converted to UNICODE glyphs only when they
  ;; are surrounded by UNICODE or ASCII characters that are
  ;; themselves part of a sketch or will be.
  ;; Surrounding is considered depending on the orientation of the
  ;; char: Vertical, horizontal, or both.
  ;; So for instance:
  ;;   -  surrounding is left and right
  ;;   |  surrounding is up and down
  ;;   +  surrounding is in the 4 directions
  ;; For UNICODE box drawing characters, an attempt is made to
  ;; complete them with half lines so that they will connect
  ;; seamlessly with their surrounding.
  (uniline--operate-on-rectangle
   (cl-loop
    for y from begy below endy
    do
    (uniline-move-to-line y)
    (cl-loop
     for x from begx below endx
     do
     (uniline-move-to-column x)
     (let ((char (uniline--char-after)))
       (if (memq char
                 '(?^ ?v ?V ?| ?\" ?- ?_ ?> ?< ?=
                      ?+ ?/ ?\\ ?' ?` ?# ?o ?O ?* ?.))
           (let*
               ((4halfvert
                 (logior
                  (uniline--infer-neighbour-4half uniline-direction-up↑)
                  (uniline--infer-neighbour-4half uniline-direction-dw↓)))
                (4halfhorz
                 (logior
                  (uniline--infer-neighbour-4half uniline-direction-lf←)
                  (uniline--infer-neighbour-4half uniline-direction-ri→)))
                (4half (logior 4halfvert 4halfhorz))
                (newchar
                 (or
                  (unless (eq 4halfvert 0)
                    (uniline--switch-with-table char
                      (?^ ?△)
                      (?v ?▽)
                      (?V ?▽)
                      (?| ?│)
                      (?\" ?║)))
                  (unless (eq 4halfhorz 0)
                    (uniline--switch-with-table char
                      (?- ?─)
                      (?_ ?─)
                      (?> ?▷)
                      (?< ?◁)
                      (?= ?═)))
                  (unless (eq 4half 0)
                    (uniline--switch-with-table char
                      (?O ?●)
                      (?o ?◦)
                      (?* ?●)
                      (?. ?∙))))))
             (if newchar
                 (uniline--insert-char newchar)
               (unless (eq 4half 0)
                 (if (memq char '(?+ ?# ?/ ?\\ ?' ?`))
                     (uniline--insert-4halfs 4half)))))))))))

(uniline--defconst-hash-table uniline--char-to-hard-corner-char
  '((?╮ . ?┐)
    (?╰ . ?└)
    (?╭ . ?┌)
    (?╯ . ?┘))
  4 'eq
  "Convert rounded corners to hard ones")

(defun uniline-change-style-hard-corners ()
  "Change rounded corners to hard corners in a rectangular selection.
This happens only for thin-lines corners, as UNICODE does not define
thick-line or double-line rounded corners."
  (interactive)
  (uniline--record-undo-rectangle-selection)
  (uniline--change-style-hash uniline--char-to-hard-corner-char))

(uniline--defconst-hash-table uniline--char-to-thin-char
  (eval-when-compile
    (append
     (cl-loop
      for c being hash-keys of uniline--char-to-4halfs
      using (hash-values v)
      for 4halfs
      = (uniline--pack-4halfs
         (cl-loop
          for u in (uniline--unpack-4halfs v)
          collect (if (or (eq u 2) (eq u 3)) 1 u)))
      unless (eq 4halfs v)
      collect (cons c (uniline--4halfs-to-char-aref 4halfs)))
     '((?┅ . ?┄)
       (?┉ . ?┈)
       (?╍ . ?╌)
       (?┇ . ?┆)
       (?┋ . ?┊)
       (?▲ . ?△)
       (?▶ . ?▷)
       (?▼ . ?▽)
       (?◀ . ?◁)
       (?▴ . ?▵)
       (?▸ . ?▹)
       (?▾ . ?▿)
       (?◂ . ?◃)
       (?■ . ?□)
       (?▪ . ?▫)
       (?• . ?◦))))
  256 'eq
  "Convert black or heavy characters to white or light ones")

(defun uniline-change-style-thin ()
  "Change bold lines and glyphs to thin ones.
This includes plain and dashed lines (e.g. ┴ to ┻, or ┅ to ┄)
as well as glyphs (e.g. ■ to □ or ▼ to ▽)."
  (interactive)
  (uniline--record-undo-rectangle-selection)
  (uniline--change-style-hash uniline--char-to-thin-char))

(uniline--defconst-hash-table uniline--char-to-thick-char
  (eval-when-compile
    (append
     (cl-loop
      for c being hash-keys of uniline--char-to-4halfs
      using (hash-values v)
      for 4halfs
      = (uniline--pack-4halfs
         (cl-loop
          for u in (uniline--unpack-4halfs v)
          collect (if (or (eq u 1) (eq u 3)) 2 u)))
      unless (eq 4halfs v)
      collect (cons c (uniline--4halfs-to-char-aref 4halfs)))
     '((?┄ . ?┅)
       (?┈ . ?┉)
       (?╌ . ?╍)
       (?┆ . ?┇)
       (?┊ . ?┋)
       (?△ . ?▲)
       (?▷ . ?▶)
       (?▽ . ?▼)
       (?◁ . ?◀)
       (?▵ . ?▴)
       (?▹ . ?▸)
       (?▿ . ?▾)
       (?◃ . ?◂)
       (?□ . ?■)
       (?▫ . ?▪)
       (?◦ . ?•))))
  128 'equal ; `equal' instead of `eq' to achieve a small collision-less table
  "Convert white or light characters to black or heavy ones")

(defun uniline-change-style-thick ()
  "Change thin lines and glyphs to bold ones.
This includes plain and dashed lines (e.g. ┴ to ┻, or ┄ to ┅)
as well as glyphs (e.g. □ to ■ or ▽ to ▼)."
  (interactive)
  (uniline--record-undo-rectangle-selection)
  (uniline--change-style-hash uniline--char-to-thick-char))

(uniline--defconst-hash-table uniline--char-to-double-line
  (eval-when-compile
    (append
     (cl-loop
      for c being hash-keys of uniline--char-to-4halfs
      using (hash-values v)
      for 4halfs
      = (uniline--pack-4halfs
         (cl-loop
          for u in (uniline--unpack-4halfs v)
          collect (if (or (eq u 1) (eq u 2)) 3 u)))
      unless (eq 4halfs v)
      collect (cons c (uniline--4halfs-to-char-aref 4halfs)))
     '((?┄ . ?═)
       (?┅ . ?═)
       (?┈ . ?═)
       (?┉ . ?═)
       (?╌ . ?═)
       (?╍ . ?═)
       (?┆ . ?║)
       (?┇ . ?║)
       (?┊ . ?║)
       (?┋ . ?║))))
  128 'eq
  "Convert any line to double line.")

(defun uniline-change-style-double ()
  "Change thin lines and bold lines to double ones.
This includes plain and dashed lines (e.g. ┻ to ╩, or ┄ to ═)."
  (interactive)
  (uniline--record-undo-rectangle-selection)
  (uniline--change-style-hash uniline--char-to-double-line))

(uniline--defconst-hash-table uniline--unicode-to-ascii
  '(
    ( ?  . ? )  ;; NO-BREAK SPACE    considered as a space
    (8200 . ? ) ;; PUNCTUATION SPACE considered as a space
    ( ?╵ . ?')
    ( ?╹ . ?')
    ( ?╶ . ?-)
    ( ?└ . ?\\)
    ( ?╰ . ?\\)
    ( ?┖ . ?\\)
    ( ?╺ . ?-)
    ( ?┕ . ?\\)
    ( ?┗ . ?\\)
    ( ?╷ . ?|)
    ( ?┆ . ?:)
    ( ?┊ . ?:)
    ( ?│ . ?|)
    ( ?┇ . ?:)
    ( ?┋ . ?:)
    ( ?┃ . ?|)
    ( ?╿ . ?|)
    ( ?┌ . ?/)
    ( ?╭ . ?/)
    ( ?├ . ?+)
    ( ?┞ . ?+)
    ( ?┍ . ?/)
    ( ?┝ . ?+)
    ( ?┡ . ?+)
    ( ?╻ . ?|)
    ( ?╽ . ?|)
    ( ?┎ . ?/)
    ( ?┟ . ?+)
    ( ?┠ . ?+)
    ( ?┏ . ?/)
    ( ?┢ . ?+)
    ( ?┣ . ?+)
    ( ?╴ . ?-)
    ( ?┘ . ?/)
    ( ?╯ . ?/)
    ( ?┚ . ?/)
    ( ?┄ . ?-)
    ( ?┈ . ?-)
    ( ?╌ . ?-)
    ( ?─ . ?-)
    ( ?┴ . ?+)
    ( ?┸ . ?+)
    ( ?╼ . ?-)
    ( ?┶ . ?+)
    ( ?┺ . ?+)
    ( ?┐ . ?\\)
    ( ?╮ . ?\\)
    ( ?┤ . ?+)
    ( ?┦ . ?+)
    ( ?┬ . ?+)
    ( ?┼ . ?+)
    ( ?╀ . ?+)
    ( ?┮ . ?+)
    ( ?┾ . ?+)
    ( ?╄ . ?+)
    ( ?┒ . ?\\)
    ( ?┧ . ?+)
    ( ?┨ . ?+)
    ( ?┰ . ?+)
    ( ?╁ . ?+)
    ( ?╂ . ?+)
    ( ?┲ . ?+)
    ( ?╆ . ?+)
    ( ?╊ . ?+)
    ( ?╸ . ?-)
    ( ?┙ . ?/)
    ( ?┛ . ?/)
    ( ?╾ . ?-)
    ( ?┵ . ?+)
    ( ?┹ . ?+)
    ( ?┉ . ?-)
    ( ?┅ . ?-)
    ( ?╍ . ?-)
    ( ?━ . ?-)
    ( ?┷ . ?+)
    ( ?┻ . ?+)
    ( ?┑ . ?\\)
    ( ?┥ . ?+)
    ( ?┩ . ?+)
    ( ?┭ . ?+)
    ( ?┽ . ?+)
    ( ?╃ . ?+)
    ( ?┯ . ?+)
    ( ?┿ . ?+)
    ( ?╇ . ?+)
    ( ?┓ . ?\\)
    ( ?┪ . ?+)
    ( ?┫ . ?+)
    ( ?┱ . ?+)
    ( ?╅ . ?+)
    ( ?╉ . ?+)
    ( ?┳ . ?+)
    ( ?╈ . ?+)
    ( ?╋ . ?+)

    ( ?╒ . ?/)
    ( ?╞ . ?+)
    ( ?║ . ?|)
    ( ?╓ . ?/)
    ( ?╟ . ?+)
    ( ?╔ . ?/)
    ( ?╠ . ?+)
    ( ?╜ . ?/)
    ( ?╨ . ?+)
    ( ?╖ . ?\\)
    ( ?╢ . ?+)
    ( ?╥ . ?+)
    ( ?╫ . ?+)
    ( ?╛ . ?/)
    ( ?╝ . ?/)
    ( ?═ . ?=)
    ( ?╧ . ?+)
    ( ?╩ . ?+)
    ( ?╕ . ?\\)
    ( ?╡ . ?+)
    ( ?╤ . ?+)
    ( ?╪ . ?+)
    ( ?╗ . ?\\)
    ( ?╣ . ?+)
    ( ?╦ . ?+)
    ( ?╬ . ?+)
    ( ?╙ . ?\\)
    ( ?╘ . ?\\)
    ( ?╚ . ?\\)

    ( ?▘ . ?')
    ( ?▝ . ?')
    ( ?▀ . ?=)
    ( ?▖ . ?,)
    ( ?▌ . ?|)
    ( ?▞ . ?:)
    ( ?▛ . ?/)
    ( ?▗ . ?-)
    ( ?▚ . ?\\)
    ( ?▐ . ?|)
    ( ?▜ . ?\\)
    ( ?▄ . ?=)
    ( ?▙ . ?\\)
    ( ?▟ . ?/)
    ( ?█ . ?+)

    ( ?△ . ?^)
    ( ?▲ . ?^)
    ( ?↑ . ?^)
    ( ?▵ . ?^)
    ( ?▴ . ?^)
    ( ?↕ . ?|)

    ( ?▷ . ?>)
    ( ?▶ . ?>)
    ( ?→ . ?>)
    ( ?▹ . ?>)
    ( ?▸ . ?>)
    ( ?↔ . ?=)

    ( ?▽ . ?v)
    ( ?▼ . ?v)
    ( ?↓ . ?v)
    ( ?▿ . ?v)
    ( ?▾ . ?v)
    ( ?↕ . ?|)

    ( ?◁ . ?<)
    ( ?◀ . ?<)
    ( ?← . ?<)
    ( ?◃ . ?<)
    ( ?◂ . ?<)
    ( ?↔ . ?=)

    ( ?□ . ?o)
    ( ?■ . ?O)
    ( ?▫ . ?o)
    ( ?▪ . ?O)
    ( ?◆ . ?O)
    ( ?◊ . ?o)

    ( ?· . ?.)
    ( ?∙ . ?.)
    ( ?• . ?.)
    ( ?● . ?O)
    ( ?◦ . ?o)
    ( ?Ø . ?O)
    ( ?ø . ?o)

    ( ?╳ . ?X)
    ( ?╱ . ?/)
    ( ?╲ . ?\\)
    ( ?÷ . ?+)
    ( ?× . ?x)
    ( ?± . ?+)
    ( ?¤ . ?o)

    ( ?░ . ?+)
    ( ?▒ . ?+)
    ( ?▓ . ?+)
    )
  256 'eq
  "Converts UNICODE to closest ASCII equivalent.")

(defun uniline-change-style-ascii ()
  "Change UNICODE to the closest ASCII equivalent."
  (interactive)
  (uniline--record-undo-rectangle-selection)
  (uniline--change-style-hash uniline--unicode-to-ascii))

(defun uniline-aa2u-rectangle ()
  "Wrapper arround `aa2u-rectangle'."
  (interactive)
  (uniline--record-undo-rectangle-selection)
  (if (functionp 'aa2u-rectangle)
      (uniline--operate-on-rectangle
       ;; here we use `eval' on purpose, to get a loose coupling
       ;; with the `ascii-art-to-unicode' package; if not installed
       ;; the native compiler may complain that `aa2u-rectangle'
       ;; is not defined; no longer with `eval'.
       ;; but long after compiling `uniline', if the
       ;; `ascii-art-to-unicode' package is eventually installed,
       ;; the `aa2u-rectangle' will be called without the need to
       ;; recompile or native-recompile `uniline'.
       (eval `(aa2u-rectangle ,beg ,end)))
    (message "Install the ascii-art-to-unicode package prior to using aa2u.
It is available on ELPA.
Or use the '0 standard' style transformer instead.")))

;;;╭───────────────────────────╮
;;;│Common to Hydra & Transient│
;;;╰───────────────────────────╯

(defun uniline-customize-face ()
  "Customize a temporary font to may-be set it for future sessions."
  (interactive)
  (customize-face-other-window 'default))

(defun uniline--is-font (letter)
  "Check if current font is the one presented by LETTER."
  (let ((name
         (uniline--switch-with-table letter
          (?d "DejaVu"                  )
          (?u "Unifont"                 )
          (?h "Hack"                    )
          (?b "JetBrain"                )
          (?c "Cascadia"                )
          (?a "Agave"                   )
          (?j "Julia"                   )
          (?f "FreeMono"                )
          (?i "Iosevka Comfy Fixed"     )
          (?I "Iosevka Comfy Wide Fixed")
          (?p "Aporetic Sans Mono"      )
          (?P "Aporetic Serif Mono"     )
          (?s "Source Code"             ))))
    (and name (string-match name (frame-parameter nil 'font)))))

(defun uniline--font-name-ticked (letter)
  "Return the name of the font presented by LETTER,
with a tick-glyph ▶ if current."
  (funcall (if (uniline--is-font letter) #'cdr #'car)
           (uniline--switch-with-table letter
             (?d '(" DejaVu"          . "▶DejaVu"         ))
             (?u '(" Unifont"         . "▶Unifont"        ))
             (?h '(" Hack"            . "▶Hack"           ))
             (?b '(" JetBrains"       . "▶JetBrains"      ))
             (?c '(" Cascadia"        . "▶Cascadia"       ))
             (?a '(" Agave"           . "▶Agave"          ))
             (?j '(" Julia"           . "▶Julia"          ))
             (?f '(" FreeMono"        . "▶FreeMono"       ))
             (?i '(" Iosevka"         . "▶Iosevka"        ))
             (?I '(" Iosevka Wide"    . "▶Iosevka Wide"   ))
             (?p '(" Aporetic Sans"   . "▶Aporetic Sans"  ))
             (?P '(" Aporetic Serif"  . "▶Aporetic Serif" ))
             (?s '(" Source Code Pro" . "▶Source Code Pro")))))

(when nil
  ;; Those low-added-value functions are automatically generated.
  ;; Their purpose is to avoid lambdas in the definition
  ;; of Hydras and Tansients.
  (insert "\n;; BEGIN -- Automatically generated\n")
  (cl-loop
   for f in
   '((?d . "DejaVu Sans Mono"        )
     (?u . "Unifont"                 )
     (?h . "Hack"                    )
     (?b . "JetBrains Mono"          )
     (?c . "Cascadia Mono"           )
     (?a . "Agave"                   )
     (?j . "JuliaMono"               )
     (?f . "FreeMono"                )
     (?i . "Iosevka Comfy Fixed"     )
     (?I . "Iosevka Comfy Wide Fixed")
     (?p . "Aporetic Sans Mono"      )
     (?P . "Aporetic Serif Mono"     )
     (?s . "Source Code Pro"         ))
   do
   (insert
    (format "(defun uniline--set-font-%c ()\n" (car f)))
   (insert "  (interactive)\n")
   (insert
    (format "  (set-frame-font \"%s\"))\n" (cdr f))))
  (insert "\n;; END -- Automatically generated\n"))

;; BEGIN -- Automatically generated
(defun uniline--set-font-d ()
  (interactive)
  (set-frame-font "DejaVu Sans Mono"))
(defun uniline--set-font-u ()
  (interactive)
  (set-frame-font "Unifont"))
(defun uniline--set-font-h ()
  (interactive)
  (set-frame-font "Hack"))
(defun uniline--set-font-b ()
  (interactive)
  (set-frame-font "JetBrains Mono"))
(defun uniline--set-font-c ()
  (interactive)
  (set-frame-font "Cascadia Mono"))
(defun uniline--set-font-a ()
  (interactive)
  (set-frame-font "Agave"))
(defun uniline--set-font-j ()
  (interactive)
  (set-frame-font "JuliaMono"))
(defun uniline--set-font-f ()
  (interactive)
  (set-frame-font "FreeMono"))
(defun uniline--set-font-i ()
  (interactive)
  (set-frame-font "Iosevka Comfy Fixed"))
(defun uniline--set-font-I ()
  (interactive)
  (set-frame-font "Iosevka Comfy Wide Fixed"))
(defun uniline--set-font-p ()
  (interactive)
  (set-frame-font "Aporetic Sans Mono"))
(defun uniline--set-font-P ()
  (interactive)
  (set-frame-font "Aporetic Serif Mono"))
(defun uniline--set-font-s ()
  (interactive)
  (set-frame-font "Source Code Pro"))

;; END -- Automatically generated

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

(defun uniline--rect-quit ()
  "Quit this hydra or transient."
  (interactive)
  (deactivate-mark))

;;;╭────────────────────╮
;;;│Language environment│
;;;╰────────────────────╯

;; Some language environments give a double width to some characters
;; used by Uniline. For instance
;;   M-x set-language-environment Chinese-BIG5
;; gives a width of 2 to ─
;; It does so through char-width-table, which is a global Emacs variable.
;; So we fix that by patching (setq char-width-table …)
;; to a new table which inherit from the original char-width-table,
;; and set the witdh of all needed characters to 1.
;; No attempt is done to revert the change on exiting uniline-mode,
;; because this would create more problems than it solves.
;; To revert char-width-table, just re-set the language environments:
;;   C-x RET l Chinese-BIG5

(defun uniline--fix-char-width-table ()
  "Create a descendent of char-width-table with characters widths set to 1.
It does so for all characters Uniline creates.
The process is lazy in the sense that if a character already has a width of 1,
its entry in the table is left as is."
  (let ((allchars))
    (maphash ;; list characters like ╶─┴╮╶╼━┻━┳━═══╩╦╸
     (lambda (key _val) (push key allchars))
     uniline--char-to-4halfs)
    (cl-loop ;; list characters like ▝▙▄█ ▗▄▖ ▖ ▘
     for e across uniline--4quadb-to-char
     do (push e allchars))
    (let ((x uniline--glyphs-fw)) ;; warning: circular list
      (while ;; list characters like ◁▲→□■·●╳
          (progn
            (setq allchars (append (cdr (car x)) allchars))
            (not (eq (setq x (cdr x)) uniline--glyphs-fw)))))
    (setq allchars (sort allchars #'<=))
    (cl-loop ;; filter out characters already 1 in width, and duplicates
     for  iter on allchars
     for  curr  = (car iter)
     with prev  = nil
     do
     (if (or
          (eq prev curr)                       ;; duplicate
          (eq (aref char-width-table curr) 1)) ;; width already 1
         (setcar iter nil))
     (setq prev curr))
    (setq allchars (delq nil allchars))

    (if allchars ;; patch only if there are characters to patch
      (let ((ct (make-char-table nil)))
        (nconc allchars (list nil))     ;; a last entry to close the algorithm
        (cl-loop
         for  curr in (cdr allchars)
         with prev  = (car allchars)
         with start = (car allchars)
         do
         (if (eq curr prev) (message "duplicate cannot happen"))
         (unless (eq (1+ prev) curr)
           (set-char-table-range ct (cons start prev) 1)
           (setq start curr))
         (setq prev curr))
        (set-char-table-parent ct char-width-table)
        (setq char-width-table ct)))))

;;;╭─────────────╮
;;;│Mouse support│
;;;╰─────────────╯

;; How it works?
;; The 3 mouse-button-1 handling functions of Emacs are
;; fairly complex. They are named:
;;   mouse-set-point, mouse-drag-region, mouse-set-region.
;;
;; Here we just let them do whatever they want to. But first
;; we intercept them, so that Uniline can add blank
;; characters or lines if the mouse event falls outside the
;; buffer.
;;
;; The mouse events provide a (point) position, which is
;; wrong when outside the buffer. But they also provide a
;; position in pixels along with the width and height in
;; pixels of a typical character. This allows to reconstruct
;; a hopefully accurate (point) position, which we re-inject
;; in the mouse event in place of the wrong (point) position.
;;
;; Of course, things are tricky because the pixel-position is
;; relative to the upper-left corner of the displayed window,
;; while we need the buffer-position. We use the (window-start)
;; function to get the line number under the upper-left corner.
;; And the (scroll-left 0) function to get the column number
;; of this corner.
;;
;; But things get even trickier when the window is zoomed with
;; C-x C-+ C-- and it is scrolled with C-x <. In this case,
;; the result of (scroll-left 0) is wrong. It does not
;; account for the zoom. So we have to reconstruct the actual
;; value using the text-scale-mode-amount variable, which
;; contains the number of x1.2 zooms (1.2 is stored in the
;; text-scale-mode-step variable).
;;
;; But reconstruction trying to reverse a computation which
;; mixes floating point numbers along with rounding to
;; integers is impossible to do accurately. Therefore, the
;; mouse placement when the window is zoomed AND scrolled
;; horizontally is not perfect.
;;
;; Moreover, each click generates 1, 2, or 3 events. The
;; first one is used by Uniline to add blank characters if
;; needed, and adjust the point. The following events also
;; need their point being adjusted as well. But the following
;; events do not have the information that blanks have been
;; added. We do not want to adjust the point if the buffer
;; have not received additional blanks, because in this case
;; the point stored in the event is right and accurate. We do
;; not want to ruin it with an unreliable approximation.
;; Therefore we put in place a variable used by the 1st event
;; to communicate information to the 2nd and 3th events.
;;
;; The Uniline intercepting functions are attached to the
;; uniline-mode keymap. Therefore they are active only in
;; uniline-mode.
;;
;; Nothing happens in a non-graphical environment, although
;; the interceptions and keymap-bindings are still present.
;;
;; Picture-mode & Artist-mode also handle the mouse. The
;; picture-mode way, unfortunately, breaks down when the
;; buffer is zoomed (C-x C-+).
;;
;; The artist-mode way smoothly handles zooming. But it is
;; completely off when window is zoomed and horizontally
;; scrolled. Artist-mode re-implements parts of the standard
;; Emacs event handling. This is because it needs to draw
;; while moving the mouse in many different styles. The
;; uniline-mode does not need this complexity.

(require 'face-remap)

(defun uniline--scroll-horiz ()
  "Compute the actual horizontal scroll.
There is a bug in Emacs that this function tries to fix.
The scroll is returned by Emacs as a number of characters,
which is fine. But Emacs assumes that the window is not
zoomed. When it is, the result is wrong. Here the scroll
is zoomed back. Unfortunately, this is not reliable no matter
what is attempted. Zoom is a floating point as powers of 1.2.
Scroll is an integer number of characters."
  (let ((scroll (scroll-left 0)))
    (cond
     ((> text-scale-mode-amount 0)
      (cl-loop
       repeat text-scale-mode-amount
       do
       (setq scroll (ceiling (/ scroll text-scale-mode-step)))))
     ((< text-scale-mode-amount 0)
      (cl-loop
       repeat (- text-scale-mode-amount)
       do
       (setq scroll (ceiling (* scroll text-scale-mode-step))))))
    scroll))

(defun uniline--intercept-mouse-1 (position)
  "Add blanks characters if mouse click falls outside buffer.
Also adjust the buffer position coded in POSITION, so that
it will be located right under the mouse event."
  ;; make the clicked window the selected one
  (set-frame-selected-window nil (posn-window position) t)

  (let* ((firstcol (uniline--scroll-horiz))
         (firstlin (1- (line-number-at-pos (window-start))))
         (last (point-max))
         (pxy (posn-x-y position))
         (wxy (posn-object-width-height position))
         (x (+ (/ (car pxy) (car wxy)) firstcol))
         (y (+ (/ (cdr pxy) (cdr wxy)) firstlin)))

    ;; possibly add blank characters
    (uniline-move-to-lin-col y x)
    (if (<= (point-max) last)
        ;; case where no blank characters where added
        ;; the point in the event can be trusted
        () ;; nothing to change

      ;; blanks were added
      (when (eobp) (insert "\n") (forward-char -1))
      (setf (nth 1 position) (point))
      (setf (nth 5 position) (point)))))

(defun uniline-mouse-set-point (event &optional promote-to-region)
  "Drop-in replacement for the base mouse-set-point.
It adds blank characters if necessary."
  (interactive "e\np")
  (uniline--intercept-mouse-1 (event-end event))
  (mouse-set-point event promote-to-region))

;;;╭──────────────╮
;;;│Customizations│
;;;╰──────────────╯

(defun uniline-customize-hydra-or-transient (type)
  "Attempt to tweak .emacs to setup the type of interface.
TYPE is \"hydra\" or \"transient\"."
  (interactive)
  (let ((message))
    (find-file user-init-file)
    (goto-char (point-min))
    (if (re-search-forward
         (rx bol (* (not ";")) "uniline-" (group (+ word)))
         nil
         t)
        (let ((current (match-string 1)))
          (beginning-of-line)
          (cond
           ((string= current type)
            (setq message (format "Already configured as uniline-%s" type)))
           ((or (string= current "hydra") (string= current "transient"))
            (setq message "It seems your current configuration is here."))
           (t
            (setq message
                  (format "It seems your current configuration is the unknown uniline-%s"
                          current)))))
      (setq message "It seems that nothing is currently configured in .emacs.")
      (goto-char (point-max)))
    (kill-new
     (format
      "(use-package uniline-%s\n  :bind (\"C-<insert>\" . uniline-mode))\n"
      type))
    (message "%s\nType C-y to insert the suggested new configuration."
             message)))

;;;╭──────────────────╮
;;;│Uniline minor mode│
;;;╰──────────────────╯

(defgroup uniline nil
  "Draw Unicode lines"
  :group 'text
  :link '(url-link :tag "GitHub" "https://github.com/tbanel/uniline"))

(defun uniline--propagate-cursor-type (_symbol value)
  "When customizing the cursor type, this function propagate
the new value to all buffers where uniline minor mode is active."
  (setq uniline-cursor-type value)
  (cl-loop
   for buff being buffers
   if (memq 'uniline-mode (assq 'local-minor-modes (buffer-local-variables buff)))
   do
   (with-current-buffer buff
     (setq cursor-type value))))

(defcustom uniline-cursor-type 'hollow
  "The suggested cursor in Uniline is a the hollow one,
because it has no prefered direction (up, down, right, left),
and the character under the cursor remains visible.
Yet, the cursor style is a matter of preference,
so any possible choice is available."
  :type '(choice
          (const :tag "Frame default" t)
          (const :tag "Filled box" box)
          (cons  :tag "Box with specified size"
                 (const box) integer)
          (const :tag "Hollow cursor" hollow)
          (const :tag "Vertical bar" bar)
          (cons  :tag "Vertical bar with specified height"
                 (const bar) integer)
          (const :tag "Horizontal bar" hbar)
          (cons  :tag "Horizontal bar with specified width"
                 (const hbar) integer)
          (const :tag "None "nil))
  :set 'uniline--propagate-cursor-type
  :local t
  :group 'uniline)

;; toggle between normal hydra hints, and one-liners

(defun uniline--propagate-hint-style (symbol value)
  "When customizing the cursor type, this function propagate
the new value to all buffers where uniline minor mode is active."
  (setq uniline-hint-style value)
  (cl-loop
   for buff being buffers
   if (memq 'uniline-mode (assq 'local-minor-modes (buffer-local-variables buff)))
   do
   (with-current-buffer buff
     (set symbol value))))

(defcustom uniline-hint-style t
  "Which kind of hint message should the Hydras menus display?
t: large and detailed menus
1: one-line non-disturbing menus in the echo area
0: no menus
Those values are loosely in sync with those defined by the
`:verbosity' Hydra property."
  :type '(choice
          (const :tag "full fledged hints" t)
          (const :tag "one liner hints"    1)
          (const :tag "no hint"            0))
  :set 'uniline--propagate-hint-style
  :local t
  :group 'uniline)

(eval-and-compile
  (defun uniline--color-hint (face hint)
    "Return a colored message mimicking the Hydra way.
HINT is the message string. It  contains pairs of ^xxx^
carets which are to be removed from the message, while the
text within will be colored.
FACE is the face used to color text."
    (interactive)
    (replace-regexp-in-string
     "\\^.*?\\^"
     (lambda (x)
       (setq x (substring x 1 (1- (length x))))
       (add-face-text-property 0 (length x) face nil x)
       x)
     hint
     t)))

(defcustom uniline-show-welcome-message t
  "Whether to show the welcome message upon activating uniline-mode."
  :type 'boolean
  :group 'uniline)

(defun uniline-dismiss-welcome-message ()
  (interactive)
  (customize-variable 'uniline-show-welcome-message))

(defun uniline--welcome-message ()
  "Display a message giving the main key-bindings of the minor mode."
  (interactive)
  (let ((message-log-max))
    (message
     (cond
      ((eq uniline-hint-style t)
       (eval-when-compile
         (uniline--color-hint
          'error
          "\
 ╭─^^────────────╴Uniline╶╴mode╶─────────────────────────────╮
 │^(Ctrl) → ↓ ← ↑^  (overwrite)/draw lines with current brush│
 │^Shift  → ↓ ← ↑^         extend selection                  │
 │^- + = # DEL RET^        change brush style                │
 │^INS^ without selection  insert glyphs, change font        │
 │^INS^ with    selection  handle rectangles                 │
 │^C-h TAB^                switch small/large hints          │
 │^C-h DEL^                dismiss this message in the future│
 │^C-c C-c^                quit uniline                      │
 ╰─^^────────────────────────────────────────────────────────╯")))
      ((eq uniline-hint-style 1)
       (eval-when-compile
         (uniline--color-hint
          'error
          "trace: ^←→↑↓^  ovwr: ^C-←→↑↓^  selec: ^C-←→↑↓^  brush: ^-+=# DEL RET^  menu: (sel)^INS^  hint: ^C-h TAB^")))
      (t nil)))))

;; The uniline-toggle-hints is defined in uniline-hydra.el & uniline-transient.el
;; with different implementation in each file.
;; It is declared here so that the compiler does not frown upon.
(declare-function uniline-toggle-hints "" (&optional notoggle))

(defun uniline-toggle-hints-welcome ()
  "Toggle between styles of hydra hints, and display welcome message."
  (interactive)
  (uniline-toggle-hints)
  (uniline--welcome-message))

(defvar-local uniline--remember-settings
    nil
  "Remember settings before entering uniline minor-mode.
It is a list containing:
  - `overwrite-mode'
  - `indent-tabs-mode'
  - `truncate-lines'
  - `cursor-type'
  - `post-self-insert-hook'")

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
  (setq cursor-type uniline-cursor-type)
  (add-hook
   'post-self-insert-hook
   #'uniline--post-self-insert
   nil 'local)
  (uniline--fix-char-width-table)
  (uniline-toggle-hints t)
  (uniline--update-mode-line)
  (if uniline-show-welcome-message
      (uniline--welcome-message)))

(defun uniline--mode-post ()
  "Restore settings when exiting uniline mode."
  (overwrite-mode    (if (nth 0 uniline--remember-settings) 1 0))
  (indent-tabs-mode  (if (nth 1 uniline--remember-settings) 1 0))
  (setq
   truncate-lines        (nth 2 uniline--remember-settings)
   cursor-type           (nth 3 uniline--remember-settings)
   post-self-insert-hook (nth 4 uniline--remember-settings)))

;; This `unintern' instruction is useful during development
;; to ensure that M-x eval-buffer reloads 100% of the Lisp code
;; (unintern 'uniline-mode-map nil)

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
╭─Glyphs (region inactive)───╴
│ \\[uniline-launch-interface] when there is NO region highlighted,
│ enter a sub-mode to draw a single character glyph,
│ and change its orientation.
├─Intersection glyphs────────╴
│ \\`a' or \\`A' arrows ▷ ▶ → ▹ ▸ ↔
│ \\`s' or \\`S' squares  □ ■ ◆ ◊
│ \\`o' or \\`O' circles  · ● ◦ Ø ø
│ \\`x' or \\`X' crosses  ╳ ╱ ╲ ÷ × ± ¤
│ Shifting the key cycles backward
├─Arrow direction────────────╴
│ \\`S-<right>' point arrow → right
│ \\`S-<left>'  point arrow ← left
│ \\`S-<up>'    point arrow ↑ up
│ \\`S-<down>'  point arrow ↓ down
├─Tweak 1/4 line─────────────╴
│ \\`S-<right>' change ¼ line → right
│ \\`S-<left>'  change ¼ line ← left
│ \\`S-<up>'    change ¼ line ↑ up
│ \\`S-<down>'  change ¼ line ↓ down
├─Text direction─────────────╴
│ Usually when typing text, cursor moves to the right.
│ \\`C-<right>' text goes right→
│ \\`C-<left>'  text goes left ←
│ \\`C-<up>'    text goes up   ↑
│ \\`C-<down>'  text goes down ↓
├─Insert characters──────────╴
│ In this sub-mode, the keys \\`-' \\`+' \\`=' \\`#' recover their
│ basic meaning, which is to insert this character.
├─Other──────────────────────╴
│ \\`f' enter the fonts sub-menu
│ \\`RET' or \\`q' exits the sub-mode
│ Any other key exits the sub-mode and do whatever they
│ are intended for.
╰────────────────────────────╴
╭─Rectangles (region active)─╴
│ \\[uniline-launch-interface] when region IS highlighted,
│ enter a sub-mode to handle rectangles,
│ marked by the highlighted region.
├─Move rectangle─────────────╴
│ \\`S-<right>' move rectangle → right
│ \\`S-<left>'  move rectangle ← left
│ \\`S-<up>'    move rectangle ↑ up
│ \\`S-<down>'  move rectangle ↓ down
├─Draw rectangle─────────────╴
│ \\`r'     draw      an inner rectangle
│ \\`R'     draw      an outer rectangle
│ \\`C-r'   overwrite an inner rectangle
│ \\`C-S-R' overwrite an outer rectangle
├─Fill───────────────────────╴
│ \\`i'	fill region with a character
├─Other──────────────────────╴
│ \\`C-_', \\`C-/', \\`C-x u' undo works outside selection
│ \\`RET', \\`q' exit the rectangle sub-mode
│ Any other key exits the sub-mode and do whatever they
│ are intended for.
╰────────────────────────────╴
╭╴Macros─────────────────────╴
│ Usual Emacs macros recording works as usual
│ Last keybord macro can be twisted in any of the 4 directions
│ \\[uniline-macro-exec] then \\`→' \\`←' \\`↑' \\`↓': directional call of last keyboard macro
╰────────────────────────────╴
╭╴Alternate styles───────────╴
│ Highlight a region (a rectangle) then \\[uniline-launch-interface] \\`s'
│ This enters a menu where alternative styles are applied
│ to the rectangular selection
│ \\`3' make 3 dots vertical, 2 dots horizontal lines
│ \\`4' make 4 dots vertical and horizontal lines
│ \\`h' convert round corners to hard ones
│ \\`-' make thin lines
│ \\`+' make thick lines
│ \\`=' make double lines
│ \\`0' come back to standard base line style, including from ASCII art
│ \\`a' apply external package aa2u conversion from ASCII art to UNICODE
│ \\`A' convert to closest ASCII
╰────────────────────────────╴
╭─Fonts──────────────────────╴
│ Try out some mono-spaced fonts with support for the
│ required UNICODE characters.
│ \\[uniline-launch-interface] \\`f' enters a sub-menu to change the font
│ type the first letter of the font name.
│ This setting is just for the current Emacs session.
│ \\`*' customize default font for future sessions.
╰────────────────────────────╴
╭─Toggle hint sizes──────────╴
│ This is for changing the height of Hydra menus,
│ between multiline to single-line and back,
│ \\[uniline-toggle-hints-welcome] in base Uniline mode
│ \\`TAB' in a \\[uniline-launch-interface]-activated menu
╰────────────────────────────╴
╭─Quit───────────────────────╴\\<uniline-mode-map>
│ \\[uniline-mode] quit the Uniline minor mode.
│ The state of the buffer (ex: `overwrite-mode' and cursor shape)
│ will return to what it was prior to entering `uniline-mode'
╰────────────────────────────╴

 Documentation here: (info \"uniline\")"
  :init-value nil
  ;;         ╭───╴without that, mouse-1 on mode-line does not display the menu
  ;;         ▽
  :lighter (:eval (format " %sUniline%s" uniline--mode-line-dir uniline--mode-line-brush))
  :keymap  ;; defines uniline-mode-map
  '(([right]   . uniline-write-ri→)
    ([left ]   . uniline-write-lf←)
    ([up   ]   . uniline-write-up↑)
    ([down ]   . uniline-write-dw↓)
    ([C-right] . uniline-overwrite-ri→)
    ([C-left ] . uniline-overwrite-lf←)
    ([C-up   ] . uniline-overwrite-up↑)
    ([C-down ] . uniline-overwrite-dw↓)
    ([insert]      . uniline-launch-interface)
    ([insertchar]  . uniline-launch-interface)
    ([?\r]           . uniline-set-brush-nil)
    ([delete]        . uniline-set-brush-0)
    ([deletechar]    . uniline-set-brush-0)
    ("-"             . uniline-set-brush-1)
    ([kp-subtract]   . uniline-set-brush-1)
    ("+"             . uniline-set-brush-2)
    ([kp-add]        . uniline-set-brush-2)
    ("="             . uniline-set-brush-3)
    ("#"             . uniline-set-brush-block)
    ("~"             . uniline-set-brush-dot-toggle)
    ([?\C-x ?e]      . uniline-macro-exec)
    ([?\C-h ?\t]        . uniline-toggle-hints-welcome)
    ([?\C-h delete]     . uniline-dismiss-welcome-message)
    ([?\C-h deletechar] . uniline-dismiss-welcome-message)
    ([mouse-1] . uniline-mouse-set-point  )
    ([?\C-c ?\C-c] . uniline-mode))
  :after-hook (if uniline-mode (uniline--mode-pre) (uniline--mode-post)))

(defun uniline--keymap-remove-launch-interface (keymap)
  "Remove key-bindings in KEYMAP whose action is `uniline-launch-interface'.
Do that recursively in child keymaps too."
  (cl-loop
   for on on keymap
   for bind = (car on)
   if (consp bind)
   do
   (cond ((eq (cdr bind) 'uniline-launch-interface)
          (setcar on nil))
         ((and (consp (cdr bind))
               (eq (cadr bind) 'keymap))
          (uniline--keymap-remove-launch-interface (cdr bind)))))
  (delq nil keymap))

(defun uniline--set-insert-key (symbol keys)
  "Replace all key-bindings pointing to `uniline-launch-interface'
with custom bindings to each key in KEYS.
_SYMBOL is not used."
  (uniline--keymap-remove-launch-interface uniline-mode-map)
  (cl-loop
   for key in keys
   do (keymap-set uniline-mode-map key 'uniline-launch-interface))
  (set-default-toplevel-value symbol keys))

(defcustom uniline-key-insert
  '("<insert>" "<insertchar>")
  "Prefix key (or keys) to invoke all Uniline minor mode functions.
<insert> and <insertchar> by default.
Use C-h k, then type a key (or key combination) to see the exact syntax.
Do not confuse this key (which is active inside Uniline-mode)
with the one used to invoke Uniline-mode."
  :type '(repeat (key))
  :set 'uniline--set-insert-key
  :group 'uniline)

(defun uniline-about ()
  "Print a message containing the MELPA version and the repository URL."
  (interactive)
  (let ((path
         (cl-loop
          for p in load-path
          if (string-match (rx "/uniline-" (group (+ (any "0-9."))) eos) p)
          collect (match-string 1 p))))
    (setq path
          (if (listp path)
              (car path)
            ""))
    (message "Uniline %s, https://github.com/tbanel/uniline" path)))

(defvar uniline--current-interface nil
  "Remember whether Hydra or Transient is loaded.
Its value is ?h or ?t")

(easy-menu-define
  uniline-menu
  uniline-mode-map
  ;; ╭─that makes this menu appear upon clicking on the mode-line
  ;; ╰────────────────────────╮
  ;;                          ▽
  "Uniline mode menu. \\{uniline-mode-map}"
  '("Uniline"
    :visible t
    :active t
    ["Write right" uniline-write-ri→ t]
    ["Write left"  uniline-write-lf← t]
    ["Write up"    uniline-write-up↑ t]
    ["Write down"  uniline-write-dw↓ t]
    ("Overwrite"
     ["Overwrite right" uniline-overwrite-ri→ t]
     ["Overwrite left"  uniline-overwrite-lf← t]
     ["Overwrite up"    uniline-overwrite-up↑ t]
     ["Overwrite down"  uniline-overwrite-dw↓ t])
    "----"
    ["─ light brush"    uniline-set-brush-1     :style radio :selected (eq uniline-brush 1     )]
    ["━ heavy brush"    uniline-set-brush-2     :style radio :selected (eq uniline-brush 2     )]
    ["═ double brush"   uniline-set-brush-3     :style radio :selected (eq uniline-brush 3     )]
    ["▞ blocks brush"   uniline-set-brush-block :style radio :selected (eq uniline-brush :block)]
    ["eraser brush"     uniline-set-brush-0     :style radio :selected (eq uniline-brush 0     )]
    ["inactive brush"   uniline-set-brush-nil   :style radio :selected (eq uniline-brush nil   )]
    "----"
    ["┄ 3-2 dots brush" uniline-set-brush-3dots :style radio :selected (eq uniline-brush-dots 1) :keys "~"  ]
    ["┄ 4-4 dots brush" uniline-set-brush-4dots :style radio :selected (eq uniline-brush-dots 2) :keys "~~" ]
    ["┄ no dots brush"  uniline-set-brush-0dots :style radio :selected (eq uniline-brush-dots 0) :keys "~~~"]
    "----"
    ("Insert glyph"
     ["Insert arrow ▷ ▶ → ▹ ▸ ↔"   uniline-insert-fw-arrow  :keys "INS a"]
     ["Insert square □ ■ ◆ ◊"      uniline-insert-fw-square :keys "INS s"]
     ["Insert oshape · ● ◦ Ø ø"    uniline-insert-fw-oshape :keys "INS o"]
     ["Insert cross ╳ ╱ ╲ ÷ × ± ¤" uniline-insert-fw-cross  :keys "INS x"])
    ("Rotate arrow, tweak ¼ line"
     ["Rotate arrow, tweak ¼ line → right" uniline-rotate-ri→ :keys "INS S-<right>"]
     ["Rotate arrow, tweak ¼ line ← left"  uniline-rotate-lf← :keys "INS S-<left>" ]
     ["Rotate arrow, tweak ¼ line ↑ up"    uniline-rotate-up↑ :keys "INS S-<up>"   ]
     ["Rotate arrow, tweak ¼ line ↓ down"  uniline-rotate-dw↓ :keys "INS S-<down>" ])
    ("Rectangular region" :active (region-active-p)
     ["Move selection right" uniline-move-rect-ri→ :keys "INS <right>"]
     ["Move selection left"  uniline-move-rect-lf← :keys "INS <left>" ]
     ["Move selection up"    uniline-move-rect-up↑ :keys "INS <up>"   ]
     ["Move selection down"  uniline-move-rect-dw↓ :keys "INS <down>" ]
     "----"
     ["Copy"        uniline-copy-rectangle :keys "INS c"]
     ["Kill"        uniline-kill-rectangle :keys "INS k"]
     ["Yank, paste" uniline-yank-rectangle :keys "INS y"]
     "----"
     ["Trace rectangle inside selection"     uniline-draw-inner-rectangle      :keys "INS r"  ]
     ["Trace rectangle around selection"     uniline-draw-outer-rectangle      :keys "INS R"  ]
     ["Overwrite rectangle inside selection" uniline-overwrite-inner-rectangle :keys "INS C-r"]
     ["Overwrite rectangle around selection" uniline-overwrite-outer-rectangle :keys "INS C-R"]
     ["Fill"                                 uniline-fill-rectangle            :keys "INS i"  ])
    ("Alternate styles" :active (region-active-p)
     ["─ thin lines"          uniline-change-style-thin     :keys "INS s -"]
     ["━ thick lines"         uniline-change-style-thick    :keys "INS s +"]
     ["═ double lines"        uniline-change-style-double   :keys "INS s ="]
     ["╌ 3 dots vert, 2 dots horiz" uniline-change-style-dot-3-2 :keys "INS s 3"]
     ["┈ 4 dots vert & horiz" uniline-change-style-dot-4-4  :keys "INS s 4"]
     ["┌ hard corners"    uniline-change-style-hard-corners :keys "INS s h"]
     ["╭ back to standard"    uniline-change-style-standard :keys "INS s 0"]
     ["aa2u (ext. package)" uniline-aa2u-rectangle        :keys "INS s a"])
    ("Fill & contour"
     ["Contour"        uniline-contour    :keys "INS c"]
     ["Contour overw" (uniline-contour t) :keys "INS C"]
     ["Fill"           uniline-fill       :keys "INS i"])
    ("Text insertion direction"
     ["→ right" uniline-text-direction-ri→ :keys "INS C-<right>" :style radio :selected (eq uniline-text-direction (uniline-direction-ri→))]
     ["← left"  uniline-text-direction-lf← :keys "INS C-<left> " :style radio :selected (eq uniline-text-direction (uniline-direction-lf←))]
     ["↑ up"    uniline-text-direction-up↑ :keys "INS C-<up>   " :style radio :selected (eq uniline-text-direction (uniline-direction-up↑))]
     ["↓ down"  uniline-text-direction-dw↓ :keys "INS C-<down> " :style radio :selected (eq uniline-text-direction (uniline-direction-dw↓))])
    "----"
    ("Font"
     ["DejaVu Sans Mono"         (set-frame-font "DejaVu Sans Mono"        ) :keys "INS f d" :style radio :selected (uniline--is-font ?d)]
     ["Hack"                     (set-frame-font "Hack"                    ) :keys "INS f h" :style radio :selected (uniline--is-font ?h)]
     ["Cascadia Mono"            (set-frame-font "Cascadia Mono"           ) :keys "INS f c" :style radio :selected (uniline--is-font ?c)]
     ["JuliaMono"                (set-frame-font "JuliaMono"               ) :keys "INS f j" :style radio :selected (uniline--is-font ?j)]
     ["JetBrains Mono"           (set-frame-font "JetBrains Mono"          ) :keys "INS f b" :style radio :selected (uniline--is-font ?b)]
     ["FreeMono"                 (set-frame-font "FreeMono"                ) :keys "INS f f" :style radio :selected (uniline--is-font ?f)]
     ["Source Code Pro"          (set-frame-font "Source Code Pro"         ) :keys "INS f s" :style radio :selected (uniline--is-font ?s)]
     ["Iosevka Comfy Fixed"      (set-frame-font "Iosevka Comfy Fixed"     ) :keys "INS f i" :style radio :selected (uniline--is-font ?i)]
     ["Iosevka Comfy Wide Fixed" (set-frame-font "Iosevka Comfy Wide Fixed") :keys "INS f I" :style radio :selected (uniline--is-font ?I)]
     ["Aporetic Sans Mono"       (set-frame-font "Aporetic Sans Mono"      ) :keys "INS f p" :style radio :selected (uniline--is-font ?p)]
     ["Aporetic Serif Mono"      (set-frame-font "Aporetic Serif Mono"     ) :keys "INS f P" :style radio :selected (uniline--is-font ?P)]
     ["Unifont"                  (set-frame-font "Unifont"                 ) :keys "INS f u" :style radio :selected (uniline--is-font ?u)]
     ["Agave"                    (set-frame-font "Agave"                   ) :keys "INS f a" :style radio :selected (uniline--is-font ?a)]
     ["Configure permanently"    uniline-customize-face                      :keys "INS f *"])
    ("Customize"
     ["Current session only:" :selected nil]
     ["Large hints sizes" uniline-toggle-hints :keys "C-t or C-h C-t" :style toggle :selected (eq uniline-hint-style t)]
     ["Hydra"     (load-library "uniline-hydra"    ) :style radio :selected (eq uniline--current-interface ?h)]
     ["Transient" (load-library "uniline-transient") :style radio :selected (eq uniline--current-interface ?t)]
     "----"
     ["Future sessions:" :selected nil]
     ["Uniline Group" (customize-group 'uniline)]
     ["Hydra     (change .emacs)" (uniline-customize-hydra-or-transient 'hydra    ) ]
     ["Transient (change .emacs)" (uniline-customize-hydra-or-transient 'transient) ]
     ["Line spacing" (customize-variable 'line-spacing)]
     )
    ["Info" (info "uniline") :keys "M-: (info \"uniline\")"]
    ["Quit Uniline Mode" uniline-mode t]
    ["About" uniline-about t]))

(provide 'uniline-core)
;;; uniline-core.el ends here
