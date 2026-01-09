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
"<return> a
 <left> <down> a
 <left> <down> a
 <left> 2*<up> <right> 3*a
 <left> <kp-add> <insert> c
 <return> 6*<right> 4*h
 <left> <down> h
 2*<left> h
 <left> <down> 9*h
 <left> <up> h
 <left> <up> 4*h
 3*<left> <down> h
 <left> <down> h
 <left>
 + <insert> c
 <return> <home> 5*<down> 4*g
 <left> <down> g
 <left> <down> g
 <left> <down> g
 <left> <down> g
 <left> <down> g
 2*<left> g
 2*<left> g
 2*<left> g
 <left> <down> 4*g
 <left> <down> g
 <left> <down> g
 <left> <down> g
 <left> <down> g
 2*<left> g
 2*<left> g
 2*<left> g
 <left> <kp-subtract> <insert> c
 <return> 10*<up> 8*<right> 8*o
 <left> <down> <right> o
 <left> <down> o
 <left> <down> o
 <left> <down> o
 <left> <down> o
 <left> <down> o
 <left> <down> o
 <left> <down> o
 2*<left> <down> o
 2*<left> o
 2*<left> o
 2*<left> o
 2*<left> o
 <left> <up> <left> o
 <left> <up> <left> o
 <left> <up> <left> o
 <left> <up> <left> o
 <left> <up> o
 <left> <up> o
 <left> <up> o
 <left> <up> o
 <left> <kp-subtract> <insert> c
 <return> <down> 2*<right> <insert> i *
 <return> 2*<up> <kp-subtract> <insert> c"
"\
aaaa╻    ╻hhhh╻    ╻hhhh╻
a┏━━┛    ┗━┓hh┗━━━━┛hh┏━┛
a┃         ┃hhhhhhhhhh┃
╺┛         ┗━━━━━━━━━━┛
 
 
╶───╮      ╭────────╮
gggg│     ╭╯oooooooo╰╮
╶─╮g│     │o╭──────╮o│
  │g│     │o│******│o│
  │g│     │o│******│o│
╶─╯g│     │o│******│o│
gggg│     │o╰╮*****│o│
gggg│     ╰╮o╰╮****│o│
╶─╮g│      ╰╮o╰╮***│o│
  │g│       ╰╮o╰───╯o│
╶─╯g│        ╰╮ooooo╭╯
gggg│         ╰─────╯
╶───╯
")
