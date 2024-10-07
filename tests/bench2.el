;;; uniline.el --- Draw lines, boxes, & arrows with the keyboard  -*- coding:utf-8; lexical-binding: t; -*-

;; Copyright (C) 2024  Thierry Banel

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
 "C-c C-c i n i t i a l SPC t e x t e <backspace> <return> SPC SPC g o o d <return> <backspace> <backspace> M-x u n i l i n e - m o d e RET <up> <up> C-SPC <down> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <insert> <down> <down> <right> <right> <right> R <end> <return> <kp-subtract> <right> q <kp-subtract> <down> <left> <left> <return> <up> <up> <right> <right> <down> <kp-subtract> <up> <right> <right> <right> <up> <up> <up> <return> <down> <down> <down> <up> <up> <right> <down> <down> <down> C-SPC <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <up> <up> <up> <insert> <up> <down> <down> c <end> SPC SPC C-SPC <insert> y <return> <return> <down> <down> <down> <right> <right> <right> <right> = <down> <left> <left> <left> <left> <left> <left> <insert> s s s <left> <left> <left> <left> <left> <insert> o <left> <left> <left> <left> <left> <left> <up>"

 "
                    ╷ 
                    │ 
  ╭────────────╮ ╭──╯ ╭────────────╮ ╭──╯ 
  │initial text│ q    │initial text│ q    
  ││ good      ├─╯    ││ good      ├─╯    
  ╰──────╥─────╯      ╰───╥────────╯      
         ╚═════·════▫═════╝
")
