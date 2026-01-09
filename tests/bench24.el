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

;; Test undo

(uniline-bench
 "<return>
 3*<down> M-5 <right>
 6*u
 <down> 4*<left>
 6*u
 #
 2*<down> 2*<left> <down> 7*<left> <up> 3*<left>
 6*C-_
 2*<down>
 <return>
 4*<up>
 <kp-subtract>
 3*<left> M-4 <down> M-6 <right> 4*<up> 4*<left>
 <return> 3*<left>
 4*S-<down> M-8 S-<right>
 <insert>
 2*<right>
 s <kp-add>
 <kp-3>
 5*C-_
 2*<return>"
"\
 
 
      ╭─────╮
     uuuuuu │
      │uuuuuu▖
      │  ▗▄▄▛▘
      ╰──▐──╯
")
