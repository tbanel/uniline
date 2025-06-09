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
 "<return> <down> <right> <right> <right> <right> <down> <down> <right> <right> <right> <right> <down> <left>
 M
 <left>
 = <up> <up> <up> M-6 <right> <down> <down> <down> <left> <left>
 <insert> a
 M-7 <left>
 <insert> a a a
 <left> <down> <down> <down> M-4 <right> <up> <up> <up>
 <return> <up> <up> <right> <right>
 <insert> s
 <down> <right> <right>
 <insert> s s
 <down> <down> <down> M-6 <left>
 <insert> x
 <up>
 <insert> o o o o o
 <up> <up> <up> <up> M-9 <right>
 C-SPC M-6 <down> M-12 <left>
 <insert> c
 <right> <return> M-13 <right> M-6 <up>
 C-SPC <insert> y <return>"
"\
 
       ╔═════╗       ╔═════╗
       ║ □   ║       ║ □   ║
       ║   ■ ║       ║   ■ ║
   ╔←══M═══◁═╝   ╔←══M═══◁═╝
   ║ ◦ ║         ║ ◦ ║      
   ║ ╳ ║         ║ ╳ ║      
   ╚═══╝         ╚═══╝      
                
")
