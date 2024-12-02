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
"a a <return> <right> a a <left> <left> <left> a <right> <down> a <down> <left> a <down> <left> a a a a a a <up> <left> a <up> <left> a <up> <left> a a a a a <down> <left> <left> <home> <down> <up> a <down> <left> a <down> <left> a <down> <left> a <down> <left> a a a a <down> <left> a <down> <left> a <down> <left> a <left> <left> a <left> <left> a <left> <left> a <down> <left> a <left> <kp-subtract> <insert> c"
"\
aaaaa╷  ╷aaaaa╷
a╭─╮a│  │a╭───╯
a│ │a╰──╯a│
a│ │aaaaaa│
a╰─┴┬─────╯
aaaa│
╶─╮a│
╶─╯a│
aaaa│
a╭──╯
╶╯
")
