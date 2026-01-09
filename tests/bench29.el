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
 "<return> 2*<down> 3*<right>
 - 2*<right>
 ~ 3*<right> 2*<up> 2*<right> 3*<down> 5*<left> <down> 2*<right> <up> 2*<right> <down> 3*<right>
 ~ 2*<right> 2*<up> 2*<right> <down> 4*<left> 3*<down>
 <kp-add> 5*<right> 2*<up> 5*<right>
 ~ 3*<right> 2*<down>
 ~ 5*<left> 2*<down> 4*<right> <up> <left> 2*<up> 8*<right>
 <return> 3*<right> 3*<up>
 C-SPC 2*<down> 6*<right> <insert> r
 <kp-subtract> r
 <return>
 <end> <right>
 <return> <left> 4*SPC
 6*a <down> 5*<left> 8*a <down> 6*<left> 4*a <right> 4*a <down> 3*<left> 3*a <down> 3*<left> 2*a
 <left> <kp-add> <insert> c"
"\
        ╭┄╮
        ┆ ┆                                ┏┅┅┅┅┅┅┓
   ╶─┄┄┄╯ ┆   ╭┈╮                 ╭┄┄┄┄╮   ┇aaaaaa┗┅┅┓
     ╭┄┬┄┬╯ ╭┈┼┈╯                 ┆    ┆   ┗┓aaaaaaaa┗┅┅┓
     ╰┄╯ ╰┄┄┼┈╯  ┏┉┉┉┉━━━┓        ╰┄┄┄┄╯    ┗┅┓aaaa╻aaaa┇
            ┊    ┋     ┏┅╋┅┅┅┅┅╸              ┗┅┅┅┅┻┓aaa┇
            ┕┉┉┉┉┛  ┏┅┅╋┅┛                          ┇aa┏┛
                    ┇  ┗┓                           ┗┅┅┛
                    ┗┅┅┅┛
")
