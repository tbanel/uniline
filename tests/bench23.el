;;; uniline.el --- Draw lines, boxes, & arrows with the keyboard  -*- coding:utf-8; lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Thierry Banel

;; Author: Thierry Banel tbanelwebmin at free dot fr
;; Version: 1.0
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

(uniline-bench
 "<return>
 2*<down> 5*<right>
 M-x p i c t u r e - m o d e <return>
 5*S-<down> 19*S-<right> 2*S-<left> C-c C-r
 3*<up> 10*<left>
 5*S-<down> 17*S-<right> C-c C-r
 6*<up>
 M-x M-p <end>
 ( <backspace>
 -
 r <backspace>
 e x i t <return>
 <up> <home> 2*<right> 5*<down> 10*<right>
 <insert> <kp-add>
 5*<up> 10*<left> <up>
 9*S-<down> 27*S-<right> 2*S-<right>
 <insert> s <kp-0>
 <return>
 <down> 6*<right> <up>
 M-x 2*M-p <return>
 5*S-<down> 9*S-<right> C-c C-r
 M-x 2*M-p <return>
 6*<left>
 <insert> <kp-add>
 5*<up> 10*<left> 5*<right>
 7*S-<down> 7*S-<right>
 <insert> s <kp-0> <kp-add>
 <return>
 4*<down> 8*<right>
 5*S-<down> 10*S-<right>
 <insert> s = <return>"
"\

         ┏━━━━━---+
     ╭──━┃━━━━━───|───╮
     │   ┃        |   │
     │   ┃  ┏━━───|──────────╮
     │   ┃  ┃     |   ║      │
     │   ┗━━╋━━---+   ║      │
     ╰──━━━━╋━━─══════╝      │
            ┃                │
            ╰───══════════───╯
                          
")
