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

;; Test rectangle fill

(uniline-bench
 "<return> 3*<down> 5*<right>
 6*S-<down> 11*S-<right> <insert> i SPC
 <return> 3*<down> 7*<right> <down> 6*S-<down> 8*S-<right> <insert> i y
 <return> 2*<up> 5*S-<left> S-<up> <insert> i SPC
 <return>"
"\
 
 
 
     ░░░░░░░░░░░
     ░░▒▒▒▒▒░░░░
     ░░▒▒▒▒▒░░░░
     ░░░░░░░░░░░
     ░░░░░░░yyyyyyyy
     ░░░░░░░yyyyyyyy
     ░░░░░░░yyyyyyyy
            yyyyyyyy
            yyyyyyyy
            yyyyyyyy
            yyyyyyyy
")
