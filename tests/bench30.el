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

;; Test leakage when moving a rectangle down, from all styles of lines:
;; plain, 3-dotted, 4-dotted, thin and thick, double, block

(uniline-bench
 "<return> 2*<down> 5*<right>
 <insert> s <return>
 <kp-subtract> 6*<right> 5*<down>
 <kp-add> 5*<right> 5*<up>
 = 5*<right> 5*<down>
 # 11*<right> 9*<up> <return>
 <right> C-SPC 5*<down> 23*<left> <insert> c
 5*<up> 24*<right> C-SPC <insert> y s <kp-3> <return>
 24*<right> C-SPC <insert> y s <kp-4> <return>
 5*<down> 25*<right> <down> C-SPC 4*<up> 74*<left> <down> <insert>
 <down>
 ~ <down>
 <return>"
"\
 
 
     □─────╮    ╔════╗    ▗  □╌╌╌╌╌╮    ╔╍╍╍╍╗    ▗  □┈┈┈┈┈╮    ╔┉┉┉┉╗    ▗
           │    ┃    ║    ▐        ┆    ┇    ┇    ▐        ┊    ┋    ┋    ▐
           │    ┃    ║    ▐        ┆    ┇    ┇    ▐        ┊    ┋    ┋    ▐  
           │    ┃    ║    ▐        ┆    ┇    ┇    ▐        ┊    ┋    ┋    ▐   
           │    ┃    ║    ▐        ┆    ┇    ┇    ▐        ┊    ┋    ┋    ▐  
           │    ┃    ║    ▐        ┆    ┇    ┇    ▐        ┊    ┋    ┋    ▐   
           │    ┃    ║    ▐        ┆    ┇    ┇    ▐        ┊    ┋    ┋    ▐   
           ┕━━━━┛    ╹▀▀▀▀▀        ┕╍╍╍╍┛    ╹▀▀▀▀▀        ┕┉┉┉┉┛    ╹▀▀▀▀▀  
                                                                             
")
