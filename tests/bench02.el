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
 "C-c C-c i n i t i a l SPC t e x t e <backspace> <return> SPC SPC g o o d <return> C-a M-x u n i l i n e - m o d e RET
 2*<up> C-SPC <down> 12*<right> <insert>
 2*<down> 3*<right> R
 <end> <return>
 <down>
 <kp-subtract> <right> q
 <return> <left>
 <kp-subtract> <down> 2*<left>
 <return> 2*<up> 2*<right> <down>
 <kp-subtract> <up> 3*<right> 3*<up>
 <return> 3*<down> 2*<up> <right> 3*<down>
 C-SPC 19*<left> 3*<up> <insert>
 <up> 2*<down> c
 <end> C-SPC <insert> y
 2*<return> 3*<down> 4*<right>
 = <down> 6*<left>
 <insert> s s s
 5*<left>
 <insert> o
 6*<left> <up>"
 "\
                    ╷ 
                    │ 
  ╭────────────╮ ╭──╯ ╭────────────╮ ╭──╯
  │initial text│ q    │initial text│ q   
  ││ good      ├─╯    ││ good      ├─╯   
  ╰──────╥─────╯      ╰───╥────────╯     
         ╚═════·════▫═════╝
")
