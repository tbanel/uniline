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
"<return> 10*<down> g
 <left> <up> 4*g
 <left> <up> g
 <left> <up> 2*g
 <left> <up> g
 2*<left> g
 2*<left> g
 2*<left> g
 2*<left> g
 <left> <up> g
 <left> <up> 7*g
 <left> <up> g
 <left> <up> g
 <left> <up> g
 2*<left> g
 2*<left> g
 2*<left> g
 2*<left> g
 2*<left> g
 2*<left> g
 <left> <up> 11*g
 <left> <down> g
 <left> <down> 10*g
 <left> <up> g
 <left> <up> g
 6*<left> g
 <left> <down> <right> <up> g
 <left> <up> 5*<right> 4*g
 <left> 6*<down> 13*<left> 2*<up> 2*<left> 6*p
 <left> <down> p
 <left> p
 2*<left> p
 2*<left> p
 2*<left> p
 2*<left> p
 2*<left> p
 <left> <up> 4*<left> <kp-subtract> <insert> c"
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
