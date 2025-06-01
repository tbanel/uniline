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
 "<return>
 <down> <down> <down> M-5 <right>
 u u u u u u
 <down> <left> <left> <left> <left>
 u u u u u u
 #
 <down> <down> <left> <left> <down> <left> <left> <left> <left> <left> <left> <left> <up> <left> <left> <left>
 C-_ C-_ C-_ C-_ C-_ C-_
 <down> <down>
 <return>
 <up> <up> <up> <up>
 <kp-subtract>
 <left> <left> <left> M-4 <down> M-6 <right> <up> <up> <up> <up> <left> <left> <left> <left>
 <return> <left> <left> <left>
 S-<down> S-<down> S-<down> S-<down> M-8 S-<right>
 <insert>
 <right> <right>
 s <kp-add>
 <down> <down>
 s <kp-3>
 <right>
 s <kp-3>
 <down>
 C-_ C-_ C-_ C-_ C-_ C-_ C-_ C-_ C-_
 i SPC
 C-_
 <return>"
"\
 
 
      ╭─────╮
     uuuuuu │
      │uuuuuu▖
      │  ▗▄▄▛▘
      ╰──▐──╯
")
