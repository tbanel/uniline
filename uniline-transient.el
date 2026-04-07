;;; uniline-transient.el --- Add▶ ■─UNICODE based diagrams─■ to▶ ■─text files─■ -*- coding:utf-8; lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Thierry Banel

;; Author: Thierry Banel tbanelwebmin at free dot fr
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (transient "0.12.0"))
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
;;│ This file, uniline-transient.el, implements the Transient interface
;;│ and calls the functions defined by uniline-core.el
;;╰──────────────────────────□

;;; Requires:
(require 'uniline-core)

;;; Code:

;;;╭───────────────────╮
;;;│Transient interface│
;;;╰───────────────────╯

(require 'transient)

(defun uniline--self-insert-command (N)
  "To fool transient into thinking this is NOT self-insert-command."
  (interactive)
  (self-insert-command N))

(eval-and-compile
  (declare-function uniline-customize-hydra-or-transient (type)))

(eval-and-compile
  ;; a kludge so that Uniline works both with transient 0.13.0
  ;; or earlier, without generating warnings
  (if (and (boundp 'transient-show-popup)
           (not (boundp 'transient-show-menu)))
      (defvaralias 'transient-show-menu 'transient-show-popup)))

;; make this transient setting buffer local, so that Uniline can
;; tweak it without touching other usages like Magit for instance
(make-variable-buffer-local 'transient-show-menu)

(defun uniline-toggle-hints (&optional notoggle)
  "Toggle between styles of transient hints.
When NOTOGGLE is t, do not toggle `uniline-hint-style',
just put everything in sync."
  (interactive)
  (unless notoggle
    (setq transient-show-menu
          (cond
           ((eq transient-show-menu   t) nil)
           ((eq transient-show-menu nil)   t)
           ((numberp transient-show-menu)  t))))
  (setq uniline-hint-style
        (cond
         ((eq transient-show-menu   t)   t)
         ((eq transient-show-menu nil)   1)
         ((numberp transient-show-menu)  1))))

(transient-define-suffix uniline-toggle-transient-hints-suffix ()
  "Toggle between full and one-liner menus.
Associated with C-t, which does half the work natively in Transient:
one-liner → full menu.
Additionally, modify transient-show-menu so that the choice is remembered
for later menu invocation in the same Uniline session."
  :transient 'transient--do-exit
  (interactive)
  (uniline-toggle-hints)
  (setq transient--showp nil)
  (eval-when-compile ;; suppress compilation warning "slot :command unknown"
    (put :command 'slot-name t))
  (transient-setup (eieio-oref (transient-prefix-object) 'command )))

;; Define common command classes to control state transitions

(transient-define-suffix uniline--persistent-command (&rest args)
  "Base class for commands that should keep the transient state active."
  :transient t
  (interactive)
  args) ;; to avoid warnings

(transient-define-suffix uniline--exit-command ()
  "Base class for commands that should exit the transient state."
  :transient nil)

(transient-define-prefix uniline-transient-customize ()
  "Preferences."
  :info-manual "(uniline) Customization"
  :transient-non-suffix 'transient-quit-one
  [:class
   transient-columns
   :pad-keys t
   ["Current session"
    ("f" "choose font" uniline-transient-fonts :transient nil)
    ("h" "Hydra"
     (lambda () (interactive)
       (load-library "uniline-hydra")
       (declare-function uniline-hydra-customize/body "uniline-hydra" ())
       (uniline-hydra-customize/body))
     :transient nil)
    ("C-t" "large hints" uniline-toggle-transient-hints-suffix)
    ]
   ["Future sessions"
    ("g" "uniline group" (lambda () (interactive) (customize-group 'uniline)))
    ("H" "Hydra     (change .emacs)"
     (lambda () (interactive) (uniline-customize-hydra-or-transient "hydra"    ))
     :transient nil)
    ("T" "Transient (change .emacs)"
     (lambda () (interactive) (uniline-customize-hydra-or-transient "transient"))
     :transient nil)
    ("l" "line spacing" (lambda () (interactive) (customize-variable 'line-spacing)))
    ]]
  (interactive)
  (transient-setup 'uniline-transient-customize))

(transient-define-prefix uniline-transient-fonts ()
  "Font selection menu."
  :info-manual "(uniline) Which fonts?"
  :transient-non-suffix 'transient-quit-one
  [:class
   transient-columns
   :pad-keys t
   ["Try a font"
    ("d" (lambda () (uniline--font-name-ticked ?d)) uniline--set-font-d :transient t)
    ("h" (lambda () (uniline--font-name-ticked ?h)) uniline--set-font-h :transient t)
    ("c" (lambda () (uniline--font-name-ticked ?c)) uniline--set-font-c :transient t)
    ("j" (lambda () (uniline--font-name-ticked ?j)) uniline--set-font-j :transient t)
    ("s" (lambda () (uniline--font-name-ticked ?s)) uniline--set-font-s :transient t)]
   [""
    ("b" (lambda () (uniline--font-name-ticked ?b)) uniline--set-font-b :transient t)
    ("f" (lambda () (uniline--font-name-ticked ?f)) uniline--set-font-f :transient t)
    ("a" (lambda () (uniline--font-name-ticked ?a)) uniline--set-font-a :transient t)
    ("u" (lambda () (uniline--font-name-ticked ?u)) uniline--set-font-u :transient t)]
   [""
    ("i" (lambda () (uniline--font-name-ticked ?i)) uniline--set-font-i :transient t)
    ("I" (lambda () (uniline--font-name-ticked ?I)) uniline--set-font-I :transient t)
    ("p" (lambda () (uniline--font-name-ticked ?p)) uniline--set-font-p :transient t)
    ("P" (lambda () (uniline--font-name-ticked ?P)) uniline--set-font-P :transient t)]
   ["Actions"
    ("*"   "Configure"  uniline-customize-face)
    ("C-t" "Togg hints" uniline-toggle-transient-hints-suffix)
    ("q"   "Quit"       transient-quit-one)
    ("RET" "Quit"       (lambda () (interactive)) :transient nil)]]
  (interactive)
  (transient-setup 'uniline-transient-fonts))

(transient-define-prefix uniline-transient-arrows ()
  "Arrows and shapes interface."
  :info-manual "uniline"
  :transient-suffix 'transient--do-leave
  :transient-non-suffix 'transient--do-leave
  [:class
   transient-columns
   :pad-keys t
   ["Insert"
    ("a" "▷▶→▹▸↔"  uniline-insert-fw-arrow  :transient t)
    ("s" "□■◆◊"    uniline-insert-fw-square :transient t)
    ("o" "·●◦Øø"   uniline-insert-fw-oshape :transient t)
    ("x" "╳╱╲÷×±¤" uniline-insert-fw-cross  :transient t)
    ("SPC" " ░▒▓█" uniline-insert-fw-grey   :transient t)]
   [""
    ("A" "↔▸▹→▶▷"  uniline-insert-bw-arrow  :transient t)
    ("S" "◊◆■□"    uniline-insert-bw-square :transient t)
    ("O" "øØ◦●·"   uniline-insert-bw-oshape :transient t)
    ("X" "¤±×÷╲╱╳" uniline-insert-bw-cross  :transient t)
    ("DEL" "█▓▒░ " uniline-insert-bw-grey   :transient t)]
   [""
    ("-" "" uniline--self-insert--     :transient t)
    ("+" "" uniline--self-insert-+     :transient t)
    ("=" "" self-insert-command :transient t)
    ("#" "" self-insert-command :transient t)
    ("~" "" self-insert-command :transient t)]
   ["Rotate,tweak"
    ("S-<up>"    "↑" uniline-rotate-up↑ :transient t)
    ("S-<right>" "→" uniline-rotate-ri→ :transient t)
    ("S-<down>"  "↓" uniline-rotate-dw↓ :transient t)
    ("S-<left>"  "←" uniline-rotate-lf← :transient t)]
   ["Text dir"
    ("C-<up>"    "↑" uniline-text-direction-up↑ :transient nil)
    ("C-<right>" "→" uniline-text-direction-ri→ :transient nil)
    ("C-<down>"  "↓" uniline-text-direction-dw↓ :transient nil)
    ("C-<left>"  "←" uniline-text-direction-lf← :transient nil)]
   ["Contour,fill"
    ("c" "Draw  cnt" uniline-contour)
    ("C" "Ovwrt cnt" (lambda () (interactive) (uniline-contour t)))
    ("i" "Fill area" uniline-fill)]
   ["Navigation"
    ("*"  "Customize" uniline-transient-customize)
    ("f"  "Font"   uniline-transient-fonts)
    ("C-t" "Hints" uniline-toggle-transient-hints-suffix)
    ("RET" "Quit" (lambda () (interactive)) :transient nil)]]
  (interactive)
  ;; the purpose of this keymap handling is to regain the basic behavior
  ;; of <up> & <down>
  ;; those keys were captured by Transient to navigate in the transient menu
  ;; the desired behavior is to exit this Transient menu and trace lines
  (let ((transient-popup-navigation-map
         (define-keymap
           "<down-mouse-1>" #'transient-noop
           "C-r"    #'transient-isearch-backward
           "C-s"    #'transient-isearch-forward
           "M-RET"  #'transient-push-button)))
    (transient-setup 'uniline-transient-arrows)))

(transient-define-prefix uniline-transient-alt-styles ()
  "Change lines style interface."
  :info-manual "(uniline) Rectangular actions"
  :transient-non-suffix 'transient-quit-one
  [:class
   transient-columns
   :pad-keys t
   ["Dashes"
    ("3"    "3x2 dots" uniline-change-style-dot-3-2      :transient t)
    ("4"    "4x4 dots" uniline-change-style-dot-4-4      :transient t)
    ("h" "hard corner" uniline-change-style-hard-corners :transient t)]
   ["Thickness"
    ("-"        "thin" uniline-change-style-thin         :transient t)
    ("+"       "thick" uniline-change-style-thick        :transient t)
    ("="      "double" uniline-change-style-double       :transient t)]
   ["Base style"
    ("0"    "standard" uniline-change-style-standard     :transient t)
    ("a"        "aa2u" uniline-aa2u-rectangle            :transient t)]
   ;;["Move rectangle"
   ;; ("<right>" "→" uniline-move-rect-ri→ :transient t)
   ;; ("<left>"  "←" uniline-move-rect-lf← :transient t)
   ;; ("<up>"    "↑" uniline-move-rect-up↑ :transient t)
   ;; ("<down>"  "↓" uniline-move-rect-dw↓ :transient t)]
   ["Misc"
    ("f"        "fonts" uniline-transient-fonts)
    ("C-t" "Togg hints" uniline-toggle-transient-hints-suffix)
    ("s"         "back" uniline-transient-moverect)
    ("RET"       "exit" uniline--rect-quit)]
   ]
  (interactive)
  (rectangle-mark-mode 1)
  (transient-setup 'uniline-transient-alt-styles))

(transient-define-prefix uniline-transient-moverect ()
  "Rectangle manipulation interface."
  :info-manual "(uniline) Rectangular actions"
  :transient-non-suffix 'transient-quit-one
  [:class
   transient-columns
   :pad-keys t
   ["Move"
    ("<left>"  "←" uniline-move-rect-lf← :transient t)
    ("<right>" "→" uniline-move-rect-ri→ :transient t)
    ("<up>"    "↑" uniline-move-rect-up↑ :transient t)
    ("<down>"  "↓" uniline-move-rect-dw↓ :transient t)]
   ["Draw"
    ("r"     "Trace inner" uniline-draw-inner-rectangle      :transient t)
    ("R"     "Trace outer" uniline-draw-outer-rectangle      :transient t)
    ("C-r"   "Ovwrt inner" uniline-overwrite-inner-rectangle :transient t)
    ("C-S-R" "Ovwrt outer" uniline-overwrite-outer-rectangle :transient t)
    ("i"     "Fill"        uniline-fill-rectangle            :transient t)]
   ["Copy-paste"
    ("c" "Copy" uniline-copy-rectangle :transient nil)
    ("k" "Kill" uniline-kill-rectangle :transient nil)
    ("y" "Yank" uniline-yank-rectangle :transient t)]
   ["Brush"
    ("-"   "╭─╯" uniline-set-brush-1          :transient t)
    ("+"   "┏━┛" uniline-set-brush-2          :transient t)
    ("="   "╔═╝" uniline-set-brush-3          :transient t)
    ("#"   "▄▄▟" uniline-set-brush-block      :transient t)
    ("~"   "┄┄┄" uniline-set-brush-dot-toggle :transient t)
    ("DEL" "DEL" uniline-set-brush-0          :transient t)]
   ["Misc"
    ("s"   "Line styles" uniline-transient-alt-styles)
    ("f"   "Choose font" uniline-transient-fonts)
    ;;("C-x C-x" "Exchg point-mark" rectangle-exchange-point-and-mark :transient t)
    ("C-t" "Togg hints"  uniline-toggle-transient-hints-suffix)
    ("RET" "Exit"        uniline--rect-quit)]
   ]
  (interactive)
  (rectangle-mark-mode 1)
  (transient-setup 'uniline-transient-moverect))

;; those low-value helper-functions are needed because for an unknown reason
;; calling a macro exits a transient menu, so we have to re-enter it
(defun uniline--transient-call-macro-in-direction-up↑ ()
  (interactive)
  (uniline-call-macro-in-direction-up↑)
  (transient-setup 'uniline-transient-macro-exec))
(defun uniline--transient-call-macro-in-direction-ri→ ()
  (interactive)
  (uniline-call-macro-in-direction-ri→)
  (transient-setup 'uniline-transient-macro-exec))
(defun uniline--transient-call-macro-in-direction-dw↓ ()
  (interactive)
  (uniline-call-macro-in-direction-dw↓)
  (transient-setup 'uniline-transient-macro-exec))
(defun uniline--transient-call-macro-in-direction-lf← ()
  (interactive)
  (uniline-call-macro-in-direction-lf←)
  (transient-setup 'uniline-transient-macro-exec))
(defun uniline--transient-call-macro ()
  (interactive)
  (kmacro-end-and-call-macro 1)
  (transient-setup 'uniline-transient-macro-exec))

(defun uniline-macro-exec ()
  (interactive)
  (transient-setup 'uniline-transient-macro-exec))

(transient-define-prefix uniline-transient-macro-exec ()
  "Macro execution interface."
  :info-manual "(uniline) Macros"
  :transient-non-suffix 'transient-quit-one
  [:class
   transient-columns
   :pad-keys t
   ["Call macro in direction"
    ("<right>"   "→" uniline--transient-call-macro-in-direction-ri→)
    ("<up>"      "↑" uniline--transient-call-macro-in-direction-up↑)
    ("<down>"    "↓" uniline--transient-call-macro-in-direction-dw↓)
    ("<left>"    "←" uniline--transient-call-macro-in-direction-lf←)]
   [""
    ("e"   "Normal call" uniline--transient-call-macro)
    ("C-t" "Togg hints"  uniline-toggle-transient-hints-suffix)
    ("RET" "Quit"        transient-quit-one)
    ("q"   "Quit"        transient-quit-one)]
   ]
  (interactive)
  (transient-setup 'uniline-transient-macro-exec))

(eval-when-compile
  ;; this ugly patch removes dumb compilation warnings.
  ;; they appear when loading this file, then byte-compiling it.
  (dolist
      (s '(uniline-transient-moverect
           uniline-transient-arrows))
    (plist-put (symbol-plist s) 'interactive-only nil)))

(defun uniline-launch-interface ()
  "Choose between rectangle and arrows interface based on selection."
  (interactive)
  (if (region-active-p)
      (uniline-transient-moverect)
    (uniline-transient-arrows)))

(defvar uniline--current-interface)
(setq uniline--current-interface ?t)

(provide 'uniline-transient)
;;; uniline-transient.el ends here
