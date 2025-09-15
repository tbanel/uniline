;;; uniline-hydra.el --- Add▶ ╭╴UNICODE based diagrams╶╮ to→ ╭╴text files╶╮ -*- coding:utf-8; lexical-binding: t -*-

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
;;│ However, the Hydra version will be compiled only if
;;│ the Hydra package was already installed, maybe by an
;;│ unrelated package (soft dependency).
;;│
;;│ Then one or the other packages must be loaded (not both)
;;│ for example with:
;;│   (require 'uniline-hydra)
;;│ or
;;│   (use-package uniline-hydra
;;│     ...)
;;│
;;│ This file, uniline-hydra.el, implements the Hydra interface
;;│ and calls the functions defined by uniline-core.el
;;╰──────────────────────────□

;;; Requires:
(require 'uniline-core)
;; (require 'hydra) ;; no hard dependency

;;; Code:

(eval-when-compile
  ;; temporarily fix a bug about Hydra generating too long docstrings
  (setq byte-compile-docstring-max-column 2000))

;;;╭────────────────╮
;;;│Hydra interfaces│
;;;╰────────────────╯

(require 'hydra nil t)

(unless (featurep 'hydra)
  (eval-and-compile
    (defun uniline-launch-interface ()
      "Fake function only when Hydra requested but not installed"
      (interactive)
      (warn "Uniline-Hydra requested, but Hydra is not installed."))))

(when (featurep 'hydra)
  (eval-and-compile

    (defun uniline--is-font-str (letter)
      "Return a tick-glyph ▶ if current font is the one presented by LETTER."
      (if (uniline--is-font letter) "▶" " "))

    (defhydra uniline-hydra-fonts
              (:hint nil :exit nil)
              ;; Docstring MUST begin with an empty line to benefit from substitutions
              (concat
               (replace-regexp-in-string
                "_\\([dhcjbfsiIuapP]\\)_ "
                "_\\1_%s(uniline--is-font-str ?\\1)"
                "\
╭^─Try a font^──^─^───────────^─^───────────────────╮╭^─^───^─^──────╮
│_d_ DejaVu     _b_ JetBrains _i_ Iosevka Comfy     ││_*_ ^^configure│
│_h_ Hack       _f_ FreeMono  _I_ Iosevka Comfy Wide││_C-t_^^ tg hint│
│_c_ Cascadia   _a_ Agave     _p_ Aporetic Sans     ││_RET_ _q_  exit│
│_j_ JuliaMono  _u_ Unifont   _P_ Aporetic Serif    │╰^─^───^─^──────╯
│_s_ Source Code Pro^^╭───────^─^───────────────────╯
╰^─^────────────^─^───╯"))
              ("d" uniline--set-font-d)
              ("u" uniline--set-font-u)
              ("h" uniline--set-font-h)
              ("b" uniline--set-font-b)
              ("c" uniline--set-font-c)
              ("a" uniline--set-font-a)
              ("j" uniline--set-font-j)
              ("f" uniline--set-font-f)
              ("i" uniline--set-font-i)
              ("I" uniline--set-font-I)
              ("p" uniline--set-font-p)
              ("P" uniline--set-font-P)
              ("s" uniline--set-font-s)
              ("*" uniline-customize-face :exit t)
              ("C-t" uniline-toggle-hints)
              ("TAB" uniline-toggle-hints)
              ("q"   () :exit t)
              ("RET" () :exit t))

    (defhydra uniline-hydra-arrows
              (:hint nil :exit nil)
              ;; Docstring MUST begin with an empty line to benefit from substitutions
              (concat
               (string-replace
                "Text dir────"
                "Text dir─╴%s(uniline-text-direction-str)╶"
                "\
╭^─^─^Insert glyph^─────╮╭^Rotate arrow^╮╭^Text dir────^╮╭^─Contour─^╮╭^─^─^─^─^─^─^─^────────────╮
│_a_,_A_rrow ▷ ▶ → ▹ ▸ ↔││_S-<left>_  ← ││_C-<left>_  ← ││_c_ contour││_-_ _+_ _=_ _#_ self-insert│
│_s_,_S_quare  □ ■ ◆ ◊  ││_S-<right>_ → ││_C-<right>_ → ││_C_ ovwrt  ││_f_ ^^^^^^      choose font│
│_o_,_O_-shape · ● ◦ Ø ø││_S-<up>_    ↑ ││_C-<up>_    ↑ │╭^─────────^╮│_C-t_   ^^^^^^  short hint │
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
              ("i" uniline-fill             :exit t)
              ("q"   ()                     :exit t)
              ("C-t" uniline-toggle-hints)
              ("TAB" uniline-toggle-hints)
              ("RET" ()                     :exit t))

    (defhydra uniline-hydra-alt-styles
              (:pre (rectangle-mark-mode 1) :hint nil :exit nil)
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
              ("+"             uniline-change-style-thick)
              ("<kp-add>"      uniline-change-style-thick)
              ("="             uniline-change-style-double)
              ("a"             uniline-aa2u-rectangle)
              ;; copy here the bindings for handling rectangles
              ("<right>" uniline-move-rect-ri→)
              ("<left>"  uniline-move-rect-lf←)
              ("<up>"    uniline-move-rect-up↑)
              ("<down>"  uniline-move-rect-dw↓)
              ("r"       uniline-draw-inner-rectangle)
              ("R"       uniline-draw-outer-rectangle)
              ("C-r"     uniline-overwrite-inner-rectangle)
              ("C-S-R"   uniline-overwrite-outer-rectangle)
              ("i"       uniline-fill-rectangle)
              ("f"       uniline-hydra-fonts/body :exit t)
              ("s"       uniline-hydra-moverect/body :exit t)
              ;; misc.
              ("C-x C-x" rectangle-exchange-point-and-mark)
              ("C-/"     uniline--rect-undo)
              ("C-_"     uniline--rect-undo)
              ("C-x u"   uniline--rect-undo)
              ("C-t"     uniline-toggle-hints)
              ("TAB"     uniline-toggle-hints)
              ("RET"     uniline--rect-quit :exit t))

    (defhydra uniline-hydra-moverect
              (:pre (rectangle-mark-mode 1) :hint nil :exit nil)
              ;; Docstring MUST begin with an empty line to benefit from substitutions
              "
╭^Move ^rect╮╭────^Draw^ rect────╮╭^─Rect^─╮╭^Brush^╮╭──^Misc^─────────╮
│_<right>_ →││_r_     trace inner││_c_ copy││_-_ ╭─╯││_s_   alt styles │
│_<left>_  ←││_R_     trace outer││_k_ kill││_+_ ┏━┛││_f_   choose font│
│_<up>_    ↑││_C-r_   ovewr inner││_y_ yank││_=_ ╔═╝││_C-/_ undo       │
│_<down>_  ↓││_C-S-R_ ovewr outer│╰^^┬─────╯╯_#_ ▄▄▟││_C-t_ short hint │
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

              ("c"   uniline-copy-rectangle :exit t)
              ("k"   uniline-kill-rectangle :exit t)
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

              ("C-t" uniline-toggle-hints)
              ("TAB" uniline-toggle-hints)
              ("f"     uniline-hydra-fonts/body      :exit t)
              ("s"     uniline-hydra-alt-styles/body :exit t)
              ("C-/"   uniline--rect-undo)
              ("C-_"   uniline--rect-undo)
              ("C-x u" uniline--rect-undo)
              ("C-x C-x" rectangle-exchange-point-and-mark)
              ("RET"   uniline--rect-quit :exit t))

    (defun uniline-launch-interface ()
      "Choose between two Hydras based on selection.
When selection is active, most likely user wants to act
on a rectangle.
Therefore the rectangle hydra is launched.
Otherwise, the arrows & shapes hydra is invoked."
      (interactive)
      (let ((message-log-max))       ; avoid hint copied in *Messages*
        (if (region-active-p)
            (uniline-hydra-moverect/body)
          (uniline-hydra-arrows/body))))

    (defhydra uniline-hydra-macro-exec
              (:hint nil :exit nil)
              ;; Docstring MUST begin with an empty line to benefit from substitutions
              "
╭^^Call macro in direction╶^^^^──────╮
│_<right>_ call → │_e_ usual call^^  │
│_<left>_  call ← │^ ^ ^   ^         │
│_<up>_    call ↑ │_C-t_^^ short hint│
│_<down>_  call ↓ │_q_ _RET_ exit    │
╰^^───────────────┴^─^─^───^─────────╯"
              ("e"       (kmacro-end-and-call-macro 1))
              ("<right>" uniline-call-macro-in-direction-ri→)
              ("<left>"  uniline-call-macro-in-direction-lf←)
              ("<up>"    uniline-call-macro-in-direction-up↑)
              ("<down>"  uniline-call-macro-in-direction-dw↓)
              ("C-t" uniline-toggle-hints)
              ("TAB" uniline-toggle-hints)
              ("q"   () :exit t)
              ("RET" () :exit t))

    (defun uniline-macro-exec ()
      (interactive)
      (uniline-hydra-macro-exec/body))

;;;╭───────────────────╮
;;;│Smaller hydra hints│
;;;╰───────────────────╯

    ;; Pack 2 hints in the usual uniline-hydra-*/hint variables
    ;; one is the standard hint created by `defhydra'
    ;; the other is a one-liner
    (setq
     uniline-hydra-arrows/hint
     `(if (eq uniline-hint-style t)
          ,uniline-hydra-arrows/hint
        ,(eval-when-compile
           (uniline--color-hint
            "glyph:^aAsSoOxX-+=#^ arr&tweak:^S-→←↑↓^ text-dir:^C-→←↑↓^ ^c^-ontour f-^i^-ll ^f^-onts ^C-t^")))
     uniline-hydra-fonts/hint
     `(if (eq uniline-hint-style t)
          ,uniline-hydra-fonts/hint
        ,(eval-when-compile
           (uniline--color-hint
            "choose font: ^dhcjbfsiIpPua^  config font: ^*^  hint: ^C-t^")))
     uniline-hydra-moverect/hint
     `(if (eq uniline-hint-style t)
          ,uniline-hydra-moverect/hint
        ,(eval-when-compile
           (uniline--color-hint
            "move: ^→←↑↓^ trace: ^rR C-rR^ copy-paste: ^cky^ f-^i^-ll brush: ^-+=# DEL^ ^s^tyle ^f^-onts ^C-t^")))
     uniline-hydra-macro-exec/hint
     `(if (eq uniline-hint-style t)
          ,uniline-hydra-macro-exec/hint
        ,(eval-when-compile
           (uniline--color-hint
            "macro exec usual: ^e^  directional: ^→←↑↓^  hint: ^C-t^")))
     uniline-hydra-alt-styles/hint
     `(if (eq uniline-hint-style t)
          ,uniline-hydra-alt-styles/hint
        ,(eval-when-compile
           (uniline--color-hint
            "alt styles, brush: ^-+=^, dashed: ^34^ corners: ^h^ standard: ^0^ ^a^a2u ^C-t^"))))

    (defun uniline-toggle-hints (&optional notoggle)
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
         uniline-hydra-macro-exec
         uniline-hydra-alt-styles)
       do
       (hydra-set-property
        hydra :verbosity uniline-hint-style)))

    ))

(provide 'uniline-hydra)
;;; uniline-hydra.el ends here
