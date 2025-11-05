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
 "b RET c
 - 3*<right> <down> 5*<left> <up>
 C-SPC <down> 6*<right> <insert> <down> 11*<right> 2*RET
 <down> 2*<right>
 + <down> 4*<left>
 <insert> 2*a
 3*<left>
 <insert> a S-<up>
 8*<left>
 RET 2*<up> 14*<right>
 - 3*<up>"

 "\
              │
           bc╶┴─╮
           ╰─┰──╯
━━━━━━△━━◀━━━┛
")
