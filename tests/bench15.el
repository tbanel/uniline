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
"<return> <down> <down> <down> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <kp-subtract> <up> <up> <right> <right> <right> <insert> a <right> <insert> a a <right> <insert> a a a <right> <insert> a a a a <right> <insert> a a a a a <right> <insert> a a a a a a <right> <right> <right> <down> <insert> A <down> <insert> A A <down> <insert> a <down> <insert> A A A A <down> <insert> a S-<right> <down> <left> <left> <left> <insert> A <left> <home> <return> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <left> <insert> A S-<up> <left> <insert> a <left> <left> <kp-subtract> <left> <left> <insert> A A A A <left> <left> <up> <insert> a a a S-<left> <up> <insert> a S-<right> <up> <insert> a a a <up> <insert> a a a a a <up> <insert> a a a a a a S-<right> <right> <right> <right> <return> <insert> a <right> <right> <right> <up> <insert> S-<down> <down> <down> <down> <right> <right> <right> <right> <right> <insert> S-<right> <return>"
"\
 
          ╭──▷▶→▿▸↔──╮
          ↔──▷       ↕
          ▴          ▾
          ↑          ▷
          ▷          ↓
          ←          ▷
          ╰─←─╴ ◁↕↔──╯
")
