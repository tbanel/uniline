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

;; Test 2.5D boxes

(uniline-bench
 ""
"<return> <down> 3*<right> <insert> 2*b 8*<right> 3*<down> 11*<right> 5*<down> 20*<left> 8*<up> <right> <return> 5*<down> 11*<right> <insert> 2*b 9*<left> 2*<down> 9*<right> 2*<up> <return> 15*<right> 5*<up> <insert> b d 10*<right> 3*<down> 10*<left> 3*<up> <return> 4*<down> 2*<right> <down> 3*<right> <insert> b t 10*<right> 4*<down> 12*<left> 4*<up> 2*<right> <return> 2*<down> <right> <up> <insert> b t <left> 2*<down> 8*<right> 2*<up> 7*<left> <delete> <down> <return> 2*<up> 2*<right> <kp-subtract> <up> 3*<left> <insert> a 2*<left> <up> <return> 2*<up> 4*<left> <right> <kp-subtract> 3*<left> 2*<down> <insert> 2*a 3*<down> 4*<left> <return> 5*<left> 3*<down> <up> <kp-subtract> 2*<down> 20*<right> <up>"
"\

  ╭────────╮                 ╭─────────╮
  │        ▐              ╭──┤         ║
  │        ▐              │  │         ║
  │        ▝──────────╮   ▼  ╰══╤══════╝
  │                   ▐   │     ╰─◁──╮
  │  ▗▄▄▄▄▄▄▄▄▖       ▐   │     ╭────┴──────╮
  │  ▐        │       ▐───╯     │ ┏━━━━━━━┑ ┃
  │  ▝────────╯       ▐         │ ┃       │ ┃
  ╰▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▟         │ ┖───────╯ ┃
                 │              ╰━━━━┯━━━━━━┛
                 ╰───────────────────╯
")
