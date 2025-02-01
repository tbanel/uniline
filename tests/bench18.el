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
"<return> <down> <right> <right> <right> t t t t t t <down> <left> <left> <left> y y <down> <left> <left> <left> <left> <left> r r r r <down> <insert> C-<left> o o o o <right> <down> o o o o o <insert> C-<right> <up> <up> <right> <right> <right> <right> <right> # <insert> c"
"\
  ▗▄▄▄▄▄▄▖
  ▐tttttt▌
  ▐███yy▛▘
  ▐rrrr█▌
▗▄▟█oooo▌
ooooo▛▀▀▘
▀▀▀▀▀▘
")
