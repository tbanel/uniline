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

;; Test fine tweaking

(uniline-bench
 "<return>
 2*<down> 2*<right>
 <kp-subtract> 4*<right> <down> 3*<right>
 <kp-add> 2*<up> 3*<left>
 = 4*<left> 4*<down> <right>
 <kp-add> 5*<right> 2*<up>
 <insert> 2*S-<left>
 <return>
 <up> <return> <left> <up> 2*<left>
 <insert> S-<up> S-<left>
 <end> <return> 2*<right> <down>
 # 8*<right> 3*<left>
 <insert> S-<up>
 <down> <left>"
"\
 
  ╔══╘═━━┓   
  ╟───╮ ╻┃  ▝▀▟▀▘
  ║   ╰─┠┚
  ║     ┃
  ╚━━━━━┛
")
