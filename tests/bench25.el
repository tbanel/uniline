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
 <down> 2*<right> 5*a
 <down> <left> 5*a
 <down> M-7 <left> 5*t
 2*<up> M-5 <right>
 2*S-<down> M-7 S-<right>
 <insert> <kp-subtract> r
 2*<return>
 <down> 4*<right> <insert> 3*o
 <up> M-4 <left> 
 C-SPC 2*<down> M-7 <right>
 <insert> c
 <return>
 M-5 <left> <up>
 C-x SPC <insert> y
 2*<return>
 M-12 <left> <up> <left> <up>
 C-SPC <insert> y
 <return>
 3*<down> 3*<right> <up>
 C-SPC <insert> y
 <return>"
"\
   ╭─────╮
  a│aaa• │    ╭─────╮
   ╰──╭─────╮ │ ╭─────╮
    tt│tt • │ ╰─│───• │
      ╰─────╯   ╰─────╯
                
")
