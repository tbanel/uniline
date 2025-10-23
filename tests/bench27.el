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


;; Test drawing right onto a TAB and also on the trail of a TAB
;; (drawing should occur)
;; Test writing on those 2 characters ╜┡ which are in 2-entries buckets
;; in the uniline--char-to-4halfs hash-table

(uniline-bench
"<escape> 3 0 <right> <escape> 2 0 <down> <return> <up> <up> <up> <up> <up> <up> <up> <up> <up> <up> <up> <up> <up> <up> <up> <up> <left> <left> <left> <left> <left> <left> <left> <left> = <down> <down> <left> <right> <kp-add> <down> <right> <left> <down> <return> <up> <insert> S-<down> S-<down> <up> <insert> S-<down> S-<left> C-x h M-x t a b i f y <return> <kp-subtract> <right> <right> <right> <right> <right> <right> <right> <right> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <return> <up> <up> <right> <right> C-SPC C-p C-p C-p C-p C-b C-b C-b <insert> c <return> <right> <right> <right> <right> C-SPC <insert> y <return> <right> <up> <kp-subtract> <down> <down> <down> <down> <down> <down> <down> <left> <left> <left> <left>"
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
