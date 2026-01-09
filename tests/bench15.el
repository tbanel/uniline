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
 "<return> 3*<down> 10*<right>
 <kp-subtract>
 2*<up> 3*<right> <insert> a
 <right> <insert> 2*a
 <right> <insert> 3*a
 <right> <insert> 4*a
 <right> <insert> 5*a
 <right> <insert> 6*a
 3*<right> <down> <insert> A
 <down> <insert> 2*A
 <down> <insert> a
 <down> <insert> 4*A
 <down> <insert> a S-<right>
 <down> 3*<left> <insert> A
 <left> <home>
 <return> 18*<right> <left> <insert> A S-<up>
 <left> <insert> a
 2*<left>
 <kp-subtract> 2*<left> <insert> 4*A
 2*<left> <up> <insert> 3*a S-<left>
 <up> <insert> a S-<right>
 <up> <insert> 3*a
 <up> <insert> 5*a
 <up> <insert> 6*a S-<right>
 3*<right>
 <return>
 <insert> a
 3*<right> <up> <insert> S-<down>
 3*<down> 5*<right> <insert> S-<right>
 <return>"
"\
 
          ╭──▷▶→▿▸↔──╮
          ↔──▷       ↕
          ▴          ▾
          ↑          ▷
          ▷          ↓
          ←          ▷
          ╰─←─╴ ◁↕↔──╯
")
