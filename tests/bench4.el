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
"<return> <down> <right> <right> <right> <right> <down> <down> <right> <right> <right> <right> <down> <left> M = <up> <up> <up> <right> <right> <right> <right> <right> <right> <down> <down> <down> <left> <left> <insert> a <left> <left> <left> <left> <left> <left> <left> <insert> a a a <left> <down> <down> <down> <right> <right> <right> <right> <up> <up> <up> <return> <up> <up> <right> <right> <insert> s <down> <right> <right> <insert> s s <down> <down> <down> <left> <left> <left> <left> <left> <left> <insert> x <up> <insert> o o o o o <up> <up> <up> <up> <right> <right> <right> <right> <right> <right> <right> <right> <right> C-SPC <down> <down> <down> <down> <down> <down> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <insert> c <right> <return> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <up> <up> <up> <up> <up> <up> C-SPC <insert> y <return>"
"
 
       ╔═════╗       ╔═════╗ 
       ║ □   ║       ║ □   ║ 
       ║   ■ ║       ║   ■ ║ 
   ╔←══M═══◁═╝   ╔←══M═══◁═╝ 
   ║ ◦ ║         ║ ◦ ║       
   ║ ╳ ║         ║ ╳ ║       
   ╚═══╝         ╚═══╝       
                 
")
