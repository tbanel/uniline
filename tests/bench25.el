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
 "<return> <down> <right> <right> a a a a a
 <down> <left> a a a a a
 <down> M-7 <left> t t t t t
 <up> <up> M-5 <right>
 S-<down> S-<down> M-7 S-<right> <insert> <kp-subtract> r
 <return> <return>
 <down> <right> <right> <right> <right> <insert> o o o
 <up> M-4 <left> 
 C-SPC <down> <down> M-7 <right> <insert> c <return>
 M-5 <left> <up> C-x SPC <insert> y <return> <return>
 M-12 <left> <up> <left> <up> C-SPC <insert> y <return>
 <down> <down> <down> <right> <right> <right> <up> C-SPC <insert> y <return>"
"\
   ╭─────╮
  a│aaa• │    ╭─────╮
   ╰──╭─────╮ │ ╭─────╮
    tt│tt • │ ╰─│───• │
      ╰─────╯   ╰─────╯
                
")
