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
"<return> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> g <left> <up> g g g g <left> <up> g <left> <up> g g <left> <up> g <left> <left> g <left> <left> g <left> <left> g <left> <left> g <left> <up> g <left> <up> g g g g g g g <left> <up> g <left> <up> g <left> <up> g <left> <left> g <left> <left> g <left> <left> g <left> <left> g <left> <left> g <left> <left> g <left> <up> g g g g g g g g g g g <left> <down> g <left> <down> g g g g g g g g g g <left> <up> g <left> <up> g <left> <left> <left> <left> <left> <left> g <left> <down> <right> <up> g <left> <up> <right> <right> <right> <right> <right> g g g g <left> <down> <down> <down> <down> <down> <down> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <up> <up> <left> <left> p p p p p p <left> <down> p <left> p <left> <left> p <left> <left> p <left> <left> p <left> <left> p <left> <left> p <left> <up> <left> <left> <left> <left> <kp-subtract> <insert> c"
"\
ggggggggggg╷  gg  ╷ggggg╷
ggggggg╭─╮g╰──────╯g╭───╯
╶────╮g│ │gggggggggg│   
╶────╯g│ ╰──────────╯   
ggggggg│pppppp          
g╶───┬─╯pppppp          
ggggg│                  
╶─╮gg│
╶─╯g╭╯
gggg│
g╭──╯
╶╯
")
