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

(uniline-bench
 "\


     mùq
        v
  2iii   hu
  1       u
   é      u
   au  uuuu
    uuuu
"

 "<return> 2*<down> 2*<right>
  C-SPC 6*<down> 9*<right>
  <insert> c <return>
  M-3 <right> M-5 <up>
  C-SPC <insert> y <return>
  6*<left>
  <insert> <return>
  <kp-subtract>
  <insert> c <return>
  7*<right>
  j <down> <left>
  j 2*<down>
  <insert> i SPC"
 
"\

    ╭───╮
    │mùq╰╮
 ╭──┴─┬╮v╰─╮    jmùq   
 │2iii│╰╮hu│    j░░░v  
 │1╶┬─╯ ╰╮u│  2iii░░░hu
 ╰╮é╰╮╭──╯u│  1░░░░░░░u
  │au╰╯uuuu│   é░░░░░░u
  ╰╮uuuu╭──╯   au░░uuuu
   ╰────╯       uuuu   
              
")
