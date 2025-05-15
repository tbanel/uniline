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
"<return> <down> C-<insert> SPC SPC SPC SPC SPC SPC SPC SPC SPC <kp-add> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-add> SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC <kp-add> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-add> <return> <left> <left> <left> <left> <left> <left> / <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> / SPC R e c t a n l <backspace> g l e SPC 1 SPC | - - - - - - - - - - - - - + SPC R e c t a n g l e SPC 2 SPC SPC ` <backspace> | <return> | SPC SPC SPC SPC SPC | SPC SPC < s i n ^ g l e > SPC SPC | SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC | SPC \" q o <backspace> u o t e \" SPC SPC SPC SPC SPC SPC + - <return> | SPC SPC SPC SPC SPC \\ <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> / SPC SPC SPC / <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> \\ SPC SPC SPC + <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-add> <return> | SPC SPC SPC SPC SPC \\ <kp-subtract> <kp-subtract> <kp-add> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-add> <kp-subtract> <kp-subtract> / SPC SPC SPC SPC <insert> <backspace> | SPC SPC SPC SPC SPC <left> <left> <left> <left> v a v SPC | SPC SPC SPC + <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> \\ <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> / <kp-subtract> <kp-subtract> <kp-subtract> + <return> \\ > <kp-subtract> <kp-subtract> \\ SPC SPC SPC SPC | SPC SPC SPC SPC SPC SPC SPC | SPC SPC SPC SPC SPC SPC \\ <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> <kp-subtract> / SPC SPC SPC SPC SPC SPC SPC SPC SPC <backspace> | SPC SPC SPC SPC SPC ` <backspace> | <return> SPC SPC SPC SPC v SPC SPC SPC SPC \\ SPC SPC <backspace> <backspace> <kp-subtract> <kp-subtract> > - - - - / SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC SPC + SPC SPC <backspace> <backspace> <kp-subtract> <kp-subtract> < <kp-subtract> <kp-subtract> + SPC SPC SPC SPC SPC SPC C-<insert> C-SPC M-< <down> <down> <up> <insert> s <kp-0>"
"\
 
         ╭─────────────╮             ╭──────────────╮  
   ╭─────┤ Rectangle 1 │─────────────┤ Rectangle 2  │ 
   │     │  <sin^gle>  │             │ \"quote\"      ├─
   │     ├─────────────┤   ╭─────╮   ├──────────────┤ 
   │     ╰──┬───────┬──╯   │ vav │   ╰────┬─────┬───╯ 
   ╰▷──╮    │       │      ╰─────╯        │     │     
       ▽    ╰──▷────╯                     ╰──◁──╯      
")
