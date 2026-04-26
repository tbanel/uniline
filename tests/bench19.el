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
   ""
   "<return> 3*<right>
 <insert> C-<down>   tttt
 <insert> C-<right>  yyyyyy
 <insert> C-<up>     hhhhh
 <insert> C-<right>  gggg
 <insert> C-<down>   aaaaaaaa
 <insert> C-<left>   bbbbbbbbbbbbbb
 <insert> C-<up>     ff
 <insert> C-<right>  ffff
 <home> 5*<down> dddddd
 <home>   <down> ggggg
 <down>   <home> 4*SPC hhh
 <down>   <home> 5*SPC iiii
 <down>   <home> hh
 <down>   <left> hhhhhh
 <up>     <left> h
 <home>
 M-<   19*<right> ii
 <down>   <left>  ii
 <down>   <left>  ii
 <down>   <left>  iiii
 <up>     <left>  ii
 <up>     <left>  ii
 <up>     <left>  ii
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
"))
