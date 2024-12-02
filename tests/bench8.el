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
"<return> a <left> <down> a <left> <down> a <left> <up> <up> <right> a  a  a <left> <kp-add> <insert> c <return> <right> <right> <right> <right> <right> <right> h  h  h  h <left> <down> h <left> <left> h <left> <down> h  h  h  h  h  h  h  h  h <left> <up> h <left> <up> h  h  h  h <left> <left> <left> <down> h <left> <down> h <left> + <insert> c <return> <home> <down> <down> <down> <down> <down> g  g  g  g <left> <down> g <left> <down> g <left> <down> g <left> <down> g <left> <down> g <left> <left> g <left> <left> g <left> <left> g <left> <down> g  g  g  g <left> <down> g <left> <down> g <left> <down> g <left> <down> g <left> <left> g <left> <left> g <left> <left> g <left> <kp-subtract> <insert> c <return> <up> <up> <up> <up> <up> <up> <up> <up> <up> <up> <right> <right> <right> <right> <right> <right> <right> <right> o  o  o  o  o  o  o  o <left> <down> <right> o <left> <down> o <left> <down> o <left> <down> o <left> <down> o <left> <down> o <left> <down> o <left> <down> o <left> <left> <down> o <left> <left> o <left> <left> o <left> <left> o <left> <left> o <left> <up> <left> o <left> <up> <left> o <left> <up> <left> o <left> <up> <left> o <left> <up> o <left> <up> o <left> <up> o <left> <up> o <left> <kp-subtract> <insert> c <return> <down> <right> <right> <insert> i * <return> <up> <up> <kp-subtract> <insert> c"
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
