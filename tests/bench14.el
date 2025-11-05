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
 "<return> <return>
 2*<down> 3*<right> 12*S-<right> 6*S-<down> # <insert> r
 <return> <insert> S-<down>
 <return> 4*<right>
 <return> 4*<right> <insert> S-<left> 5*<right>
 <insert> S-<up> S-<right> S-<left> S-<up> 3*<down>
 <insert> S-<left> 3*<down>
 <insert> 2*S-<right> S-<down> S-<up> 5*<left>
 <insert> S-<up> S-<right> 6*<left>
 <insert> S-<up> S-<right> S-<left>
 <return> 3*<up>
 <kp-subtract> 4*<right>
 <insert> a
 S-<up> 2*<right>
 <insert> a
 S-<down> 2*<right>
 <insert> a
 S-<left> 2*<right>
 <insert>
 2*S-<left>
 <return> 9*<left>
 <return> <insert> 2*S-<right> 2*S-<left> S-<up> S-<right> 2*<right>
 <insert> 2*S-<right> S-<up> S-<down>
 <return>"
"\
 
 
   █▀▀▀▀▀▛▀▀▀▀▙
   ▌          ▐
   ▌          ▐
   ▌╰─┤△─▽─◁─╴▟
   ▌          ▐
   ▌          ▐
   ▐▄▄▄▄▄█▄▄▄▄▛
")
