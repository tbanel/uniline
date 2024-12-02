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
"<return> <down> <down> <right> <right> <right> <right> <right> m ù q <down> v <down> h u <down> <left> u <down> <left> u <down> <left> u <left> <left> u <left> <left> u <left> <left> u <down> <left> u <left> <left> u <left> <left> u <left> <left> u <up> <left> u <left> <left> a <up> <left> é <up> <left> <left> 1 <up> <left> 2 i i <down> <up> i <up> <up> <left> <left> <left> <left> C-SPC <down> <down> <down> <down> <down> <down> <right> <right> <right> <right> <right> <right> <right> <right> <right> <insert> c <up> <right> <right> <right> <up> <up> <up> <up> y <return> <left> <left> <left> <left> <left> <left> <insert> <return> <kp-subtract> <insert> c <return> <right> <right> <right> <right> <right> <right> <right> j <down> <left> j <down> <down> <insert> i SPC"
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
