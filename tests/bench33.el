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

;; Test leakage when moving a rectangle down, from all styles of lines:
;; plain, 3-dotted, 4-dotted, thin and thick, double, block

(let ((uniline-infinite-up↑ t))
  (uniline-bench
   "<insert> s <right> <kp-add> <right> <right> <down> <right> <right> <right> <up> <up> <up> <right> <right> <right> <down> <down> # <right> <right> <right> <right> <right> <right> <up> <up> <right> <right> <right> <up> <up> <up> <up> <right> <right> <right> <right> <right> <down> <right> <right> <right> <right> <down> <down> <down> <down> <right> <right> <return> <right> <right> <right> <right> a a a <up> <left> <left> a a a a a a <up> <left> <left> <left> <left> a a a a a a SPC SPC SPC SPC a a a a <down> <left> <left> <left> a a <down> <left> <left> <left> <right> <insert> C-<left> a a a a a a a a a <right> <kp-subtract> <insert> c <return> <right> <right> <right> <right> <right> <right> <right> <right> <up> <up> r r r r <down> <right> <right> <right> <right> <right> <left> r r r r C-SPC <right> <right> <right> <right> <right> <up> <insert> <right> <right> <right> <up> <up> <right> <right> <right> <insert> R <kp-subtract> r <return> <down> <down> <down> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <return> <right> <insert> C-<up> y y y y y <insert> C-<right> y y y"
   "\
                                                            yyy
                                                            y
                                               ╭─────╮      y
                                               │ rrrr│      y
                         ╭──────╮  ╭────╮      │ rrrr│      y
             ▐▀▀▙▄▖    ╭─╯aaaaaa│  │aaaa│      ╰─────┴─────╴y
      ┏━━┓   ▐    ▌   ╭╯aaaaaa╶─┴──┴╴aa╭╯         
      ┃  ┃  ▛▀    ▙▖  │aaa╭─╮aaaaaaaaa╭╯      
□╼━┓  ┃  ╹▀▀▘         ╰───╯ ╰─────────╯
   ┗━━┛
"))
