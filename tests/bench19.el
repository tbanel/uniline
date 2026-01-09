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
 "<return>
 3*<right> <insert> C-<down> 4*t
 <insert> C-<right> 6*y
 <insert> C-<up> 5*h
 <insert> C-<right> 4*g
 <insert> C-<down> 8*a
 <insert> C-<left> 14*b
 <insert> C-<up> 2*f
 <insert> C-<right> 4*f
 <home> 5*<down> 6*d
 <home> <down> 5*g
 <down> <home> 4*SPC 3*h
 <down> <home> 5*SPC 4*i
 <down> <home> 2*h
 <down> <left> 6*h
 <up> <left> h
 <home> M-< 19*<right> 2*i
 <down> <left> 2*i
 <down> <left> 2*i
 <down> <left> 4*i
 <up> <left> 2*i
 <up> <left> 2*i
 <up> <left> 2*i
 # <insert> c
 M-< <return> 3*<right> # <insert> c
 <return> <home> 12*<down> <end> C-a M-f <insert> c
 # <insert> c"
"\
  ▐t▖   ▐gggga▖   ▐ii▄▖  ▗▟ii▖
  ▐t▌   ▐h▛▀▜a▌   ▝▜ii▙▖▗▟ii▛▘
  ▐t▌   ▐h▌ ▐a▌    ▝▜ii▙▟ii▛▘
  ▐t▙▄▄▄▟h▌ ▐a▌     ▝▜iiii▛▘
  ▐yyyyyyh▌ ▐a▌      ▝▀▀▀▀▘
▗▄▟█▛▀▀▀▀▀▘ ▐a▌
ffff▌       ▐a▌
f███▙▄▄▄▄▄▄▄▟a▌
fbbbbbbbbbbbbb▌
▀▀▀▀▀▀▀▀▀▀▀▀▀▀▘
▗▄▄▄▄▄▖
dddddd▌
ggggg█▙▖
▀▀▀▜hhh▙▄▖
▗▄▖▝▜iiii▌
hh▙▄▟█h▛▀▘
▜hhhhhh▌
▝▀▀▀▀▀▀▘
")
