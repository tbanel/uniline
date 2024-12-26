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
"<return> <down> <right> <right> <right> <right> <right> <insert> a <right> <right> <insert> A <right> <right> <insert> a S-<up> <right> <right> <insert> A S-<up> <down> C-a <up> C-k C-y <down> <home> C-y <left> <insert> a <left> <left> <insert> a <left> <left> <insert> a <left> <left> <insert> a C-a C-k C-y <right> <down> C-a C-y C-a <right> <right> <right> <right> <right> <insert> A A <right> <right> <insert> A <right> <right> <insert> A A <right> <right> <insert> A"
"\
 
     ▷ ↔ △ ↕ 
     ▶ ◁ ▲ ◁               
     ↔ ↔ ↕ ↔                                        ")
