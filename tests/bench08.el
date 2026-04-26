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

(let ((uniline-infinite-up↑ nil))
  (uniline-bench
   "\
aaaa      hhhh      hhhh
a           hh      hh
a           hhhhhhhhhh




gggg        oooooooo
   g       o        o
   g       o        o
   g       o        o
   g       o        o
gggg       o        o
gggg        o       o
   g         o      o
   g          o     o
   g           ooooo
gggg
"

   "<kp-add> <insert> c
    <return> 8*<right>
    + <insert> c
    <return>
    M-< 7*<down> 2*<right>
    <kp-subtract> <insert> c
    <return> 8*<right> <down>
    <kp-subtract> <insert> c
    <return> <down> <right> <insert> i *
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
"))
