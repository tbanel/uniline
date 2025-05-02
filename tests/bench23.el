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
"<return> <down> <down> <right> <right> <right> <right> <right> M-x p i c t u r e - m o d e <return> S-<down> S-<down> S-<down> S-<down> S-<down> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<left> S-<left> C-c C-r <up> <up> <up> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> S-<down> S-<down> S-<down> S-<down> S-<down> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> C-c C-r <up> <up> <up> <up> <up> <up> M-x M-p <end> ( <backspace> - r <backspace> e x i t <return> <up> <home> <right> <right> <down> <down> <down> <down> <down> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <insert> <kp-add> <up> <up> <up> <up> <up> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <up> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> <insert> s <kp-0> <return> <down> <right> <right> <right> <right> <right> <right> <up> M-x M-p M-p <return> S-<down> S-<down> S-<down> S-<down> S-<down> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> C-c C-r M-x M-p M-p <return> <left> <left> <left> <left> <left> <left> <insert> <kp-add> <up> <up> <up> <up> <up> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <right> <right> <right> <right> <right> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> <insert> s <kp-0> <kp-add> <return> <down> <down> <down> <down> <right> <right> <right> <right> <right> <right> <right> <right> S-<down> S-<down> S-<down> S-<down> S-<down> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> <insert> s = <return>"
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
