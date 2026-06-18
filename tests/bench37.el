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


    aaaaaaaaa
     bbbbbbb
      ccccc
       ddd
        e"

 "<return> 2*<down> 3*<right> <up> 2*<right>
 <kp-subtract> <kp-add> = # ~
 <insert> <kp-subtract> <insert> c <return>
 <up> 7*<left>
 C-SPC 7*<down> 11*<right>
 <insert> <down> <right> <down> <right>
 <kp-add> R <return>
 <kp-add>"

 "\
              
    +━━━━━━━━━━━┓
    ┃ ╭─────╮   ┃
    ┃╭╯-+=#~╰──╮┃
    ┃│aaaaaaaaa│┃
    ┃╰╮bbbbbbb╭╯┃
    ┃ ╰╮ccccc╭╯ ┃
    ┃  ╰╮ddd╭╯  ┃
    ┃   ╰╮e╭╯   ┃
    ┃    ╰─╯    ┃
    ┗━━━━━━━━━━━┛
"

 'uniline-prefix-for-setting-brush t)
