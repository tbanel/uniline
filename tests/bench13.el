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

;; Test glyph insertion

(uniline-bench
 "<kp-subtract> <return>
 2*<down> 4*<right>
 <kp-subtract> 4*<right> 2*<down>
 <insert> a
 2*<up> 3*<right> <up>
 <insert> s
 <down> 3*<right> 2*<down> 3*<right> 3*<left> 2*<down>
 <insert> 2*o
 2*<up> <return> 2*<left> <up> 4*<left> <up>
 <insert> 2*S-<right> 3*<right>
 <insert> 2*S-<right> 3*<right> 2*<down>
 <insert> 2*S-<down> 3*<right>
 <insert> 2*S-<right>
 <return>
 13*<left> 2*<up> <insert> 2*S-<left>"
"\
 
           □
    ╾───╮──╯──╮
        │     │
        ▽     ╰──╼
              │
              ∙
")
