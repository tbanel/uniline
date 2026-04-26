;;; uniline.el --- Draw lines, boxes, & arrows with the keyboard  -*- coding:utf-8; lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Thierry Banel

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

;; Test leakage when moving a rectangle down, from all styles of lines:
;; plain, 3-dotted, 4-dotted, thin and thick, double, block

(let ((uniline-infinite-up↑ t))
  (uniline-bench
   ""
   "<insert> s <right>
 <kp-add> 2*<right> <down> 3*<right> 3*<up> 3*<right> 2*<down>
 # 6*<right> 2*<up> 3*<right> 4*<up> 5*<right> <down> 4*<right> 4*<down> 2*<right>
 <return> 4*<right> 3*a <up> 2*<left> 6*a <up> 4*<left> 6*a 4*SPC 4*a <down> 3*<left> 2*a <down> 3*<left> <right>
 <insert> C-<left> 9*a <right>
 <kp-subtract> <insert> c
 <return> 8*<right> 2*<up> 4*r <down> 5*<right> <left> 4*r
 C-SPC 5*<right> <up>
 <insert> 3*<right> 2*<up> 3*<right>
 <insert> R
 <kp-subtract> r
 <return> 3*<down> 12*<right>
 <return> <right>
 <insert> C-<up> 5*y
 <insert> C-<right> 3*y"
   "\
                                                            yyy
                                                            y
                                               ╭─────╮      y
                                               │ rrrr│      y
                         ╭──────╮  ╭────╮      │ rrrr│      y
             ▐▀▀▙▄▖    ╭─╯aaaaaa│  │aaaa│      ╰─────┴─────╴y
      ┏━━┓   ▐    ▌   ╭╯aaaaaa╶─┴──┴╴aa╭╯         
      ┃  ┃  ▛▀    ▙▖  │aaa╭─╮aaaaaaaaa╭╯      
□╼━┓  ┃  ╹▀▀▘         ╰───╯ ╰─────────╯
   ┗━━┛
"))
