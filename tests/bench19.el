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
"<return> <right> <right> <right> <insert> C-<down> t t t t <insert> C-<right> y y y y y y <insert> C-<up> h h h h h <insert> C-<right> g g g g <insert> C-<down> a a a a a a a a <insert> C-<left> b b b b b b b b b b b b b b <insert> C-<up> f f <insert> C-<right> f f f f <home> <down> <down> <down> <down> <down> d d d d d d <home> <down> g g g g g <down> <home> SPC SPC SPC SPC h h h <down> <home> SPC SPC SPC SPC SPC i i i i <down> <home> h h <down> <left> h h h h h h <up> <left> h <home> M-< <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> i i <down> <left> i i <down> <left> i i <down> <left> i i i i <up> <left> i i <up> <left> i i <up> <left> i i # <insert> c M-< <return> <right> <right> <right> # <insert> c <return> <home> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <end> C-a M-f <insert> c # <insert> c"
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
