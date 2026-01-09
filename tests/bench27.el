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


;; Test drawing right onto a TAB and also on the trail of a TAB
;; (drawing should occur)
;; Test writing on those 2 characters ╜┡ which are in 2-entries buckets
;; in the uniline--char-to-4halfs hash-table

(uniline-bench
 "<escape> 3 0 <right>
 <escape> 2 0 <down>
 <return> 16*<up> 8*<left>
 = 2*<down> <left> <right>
 <kp-add> <down> <right> <left> <down> <return> <up>
 <insert> 2*S-<down>
 <up>
 <insert> S-<down> S-<left>
 C-x h M-x t a b i f y <return>
 <kp-subtract> 8*<right> 10*<down> 14*<right> <return> 2*<up> 2*<right>
 C-SPC 4*C-p 3*C-b
 <insert> c
 <return>
 4*<right>
 C-SPC <insert> y
 <return>
 <right> <up> <kp-subtract> 7*<down> 4*<left>"
"\
╶───────┬─────────────────────╮
	│       	      │
	│       	      │
	│       	  ╷   │
	│             ╻	  │   │
	│             ║	  ║   │
	│            ╺╜	 ╺┤   │
	│             ┡╸  ┝╸  │
	│             ╹	  │   │
	│                 │   │
	╰─────────────────╯   │
			      │
			      │
			      │
			      │
			      │
			      │
			      │
			      │
			      │
			      ╵
")
