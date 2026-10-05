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


     xx
    x  x
   x    x
   x    x
    x  x
     xx
"

"<return> <down> <down> <right> <right> <right> <right> <right> <kp-subtract> <insert> c <return> <down> <left> <kp-subtract> <insert> c <return> <down> <down> <down> <down> <down> <right> <right> <right> <right> C-SPC <up> <up> <up> <up> <up> <up> <up> <left> <left> <left> <left> <left> <left> <left> <left> <insert> a testsuite2 <return> <return> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <insert> v testsuite2 <return> <right> <return> <return> <down> <down> <down> <down> <down> <down> <down> <down> <down> <left> <left> <left> <left> <left> <right> <left> <insert> v testsuite2 <return> <left> <up> <return>"

"\

    ╭──╮        ╭──╮  
   ╭╯xx╰╮      ╭╯xx╰╮ 
  ╭╯x╭╮x╰╮    ╭╯x╭╮x╰╮
  │x╭╯╰╮x│    │x╭╯╰╮x│
  │x╰╮╭╯x│    │x╰╮╭╯x│
  ╰╮x╰╯x╭╯    ╰╮x╰╯x╭╯
   ╰╮xx╭╯      ╰╮xx╭╯ 
    ╰──╯        ╰──╯  
          ╭──╮  
         ╭╯xx╰╮  
        ╭╯x╭╮x╰╮ 
        │x╭╯╰╮x│ 
        │x╰╮╭╯x│ 
        ╰╮x╰╯x╭╯ 
         ╰╮xx╭╯  
          ╰──╯   
                 
         
"
'uniline-infinite-up↑ 't
'uniline-key-insert '("<insert>" "<insertchar>" "<pause>")
'uniline-prefix-for-setting-brush nil)
