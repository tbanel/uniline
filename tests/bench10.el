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

;; a macro cannot be defined inside another macro,
;; so this macro is defined outside
(setq last-kbd-macro (kbd "<kp-subtract> <right> C-<up> C-<right> <down>"))

(uniline-bench
 "<return> 2*<down> 2*<right> 20*a
 C-a C-k C-y
 C-a <down> C-y
 C-a <down> C-y
 C-a 4*<up> <down> <up> <right> <down>
 C-x 4*e 3*<right> 3*<down> 5*<left> 3*<up>
 <return>"
 "\
  ╭╮╭╮╭╮╭╮╭╮╭╮
 ╶┼┴╯╰╯╰╯╰╯╰╯│
  ╰╴aaaaaaaaa╶╮aaaaaaa
  ╭aaaaaaaaaaa╯aaaaaaa 
  ╰╴aaaaaaaaa╶╮aaaaaaa 
  ╭╯         ╭╯
  ╰╮         ╰╮
   │╭╮╭╮╭╮╭╮╭─╯
   ╰╯╰╯╰╯╰╯╰╯
")
