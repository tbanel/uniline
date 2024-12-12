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
"<return> <down> <down> <right> <right> <right> <right> <right> a a a a <right> a a a a <right> <right> <right> a a a <right> <right> <right> a a a a a a a a C-SPC <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <insert> c <down> <right> <right> y c <down> <right> <right> y <down> <up> c <down> <right> <right> y c <return> <up> <up> <up> <right> C-SPC <down> <down> <down> <right> <right> <right> <right> <right> <right> <right> <right> <right> <kp-subtract> <insert> C-r <return> <right> <right> <right> <right> <right> <right> <right> <down> C-SPC <down> <down> <down> <right> <right> <right> <right> <right> <right> <kp-add> <insert> C-r = R RET"
"\
 
 
     aaaa aa╭─────╦╤╤══aaaaaaaa
       aaaa │aaa  ║┏┿━━━┓aaaaaaaa
         aaa│ aaaa║┃│aaa┃║ aaaaaaaa
           a╰─────╫╂╯  a┃a   aaaaaaaa
                  ║┗━━━━┛║
                  ╚══════╝
")
