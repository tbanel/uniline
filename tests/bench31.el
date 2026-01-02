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

;; Test leakage when moving a rectangle right, and another left.
;; Test for all styles of lines:
;; plain, 3-dotted, 4-dotted, thin and thick, double, block

(uniline-bench
 "<return> 2*<down> 3*<right> <insert> 4*o <return>
 <kp-subtract> 6*<right> <down>
 <kp-add> 6*<left> <down>
 = 6*<right> <down>
 # 12*<left> 2*<down>
 <return> <down> <left>
 C-SPC 5*<up> 9*<right> <insert> c
 8*<left> 5*<down> <left> C-SPC <insert> y s <kp-3> <return>
 5*<down> C-SPC <insert> y s <kp-4> <return>
 5*<down> 9*<right> C-SPC 16*<up> 4*<left> <insert>
 <right> ~ <right> <return>
 7*<left> <right> C-SPC 16*<down> 4*<right> <left> <insert>
 <left> ~ <left> ~ <left> <return>"
"\
 
             
●──────────╮ 
┏━━━━━━━━━━┙ 
╚══════════╗ 
▛▀▀▀▀▀▀▀▀▀▀╹ 
▘            
●╌╌╌╌╌╌╌╌╌╌╮ 
┏╍╍╍╍╍╍╍╍╍╍┙ 
╚╍╍╍╍╍╍╍╍╍╍╗ 
▛▀▀▀▀▀▀▀▀▀▀╹ 
▘            
●┈┈┈┈┈┈┈┈┈┈╮ 
┏┉┉┉┉┉┉┉┉┉┉┙ 
╚┉┉┉┉┉┉┉┉┉┉╗ 
▛▀▀▀▀▀▀▀▀▀▀╹ 
▘            
             
  
")
