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

;; Test ASCII → Uniline base-line

(uniline-bench
"<return> <down> C-<insert>
 9*SPC <kp-add> 13*<kp-subtract> <kp-add> 13*SPC <kp-add> 14*<kp-subtract> <kp-add> <return> C-a
 3*SPC / 5*<kp-subtract> / SPC R e c t a n l <backspace> g l e SPC 1 SPC | 13*- + SPC R e c t a n g l e SPC 2 2*SPC ` <backspace> | <return> C-a
3*SPC | 5*SPC | 2*SPC < s i n ^ g l e > 2*SPC | 13*SPC | SPC \" q o <backspace> u o t e \" 6*SPC + - <return> C-a
 3*SPC | 5*SPC \\ 13*<kp-subtract> / 3*SPC / 5*<kp-subtract> \\ 3*SPC + 14*<kp-subtract> <kp-add> <return> C-a
 3*SPC | 5*SPC \\ 2*<kp-subtract> <kp-add> 7*<kp-subtract> <kp-add> 2*<kp-subtract> / 4*SPC <insert> <backspace> | 5*SPC 4*<left> v a v SPC | 3*SPC + 4*<kp-subtract> \\ 5*<kp-subtract> / 3*<kp-subtract> + <return> C-a
 3*SPC \\ > 2*<kp-subtract> \\ 4*SPC | 7*SPC | 6*SPC \\ 5*<kp-subtract> / 9*SPC <backspace> | 5*SPC ` <backspace> | <return> C-a
 7*SPC v 4*SPC \\ 2*SPC 2*<backspace> 2*<kp-subtract> > 4*- / 21*SPC + 2*SPC 2*<backspace> 2*<kp-subtract> < 2*<kp-subtract> + 6*SPC
 C-<insert> C-SPC M-< 2*<down> <up> <insert> s <kp-0> 2*<return>"
"\
 
         ╭─────────────╮             ╭──────────────╮  
   ╭─────┤ Rectangle 1 │─────────────┤ Rectangle 2  │ 
   │     │  <sin^gle>  │             │ \"quote\"      ├─
   │     ├─────────────┤   ╭─────╮   ├──────────────┤ 
   │     ╰──┬───────┬──╯   │ vav │   ╰────┬─────┬───╯ 
   ╰▷──╮    │       │      ╰─────╯        │     │     
       ▽    ╰──▷────╯                     ╰──◁──╯      
")
