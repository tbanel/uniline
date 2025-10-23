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
 "2*<return>
 2*<down> 3*<right>
 # 6*<right> 3*<down> 6*<right> 3*<up> 6*<right> 3*<down> 6*<right> 4*<up> 3*<right> 6*<down> 12*<left>
 <return> <up> 15*<right> <up>
 C-SPC 4*<down> 12*<right> <insert>
 r # R
 <return> <kp-subtract> 4*<down> 11*<right> 4*<up> 11*<left>
 <return> 23*<left> 2*<up>
 C-SPC 4*<down> 16*<right> <insert>
 2*<down> 3*<right>
 <return> C-c C-c"
 "\
                                      
                        ▗▄▄▄▄▄▄▄▄▄▄▄▄▖
                        ▐╭──────────╮▌
                  ▄▄    ▐│          │▌
      ▝▀▀▌  ▛▀▀▌  ▌▐    ▐│          │▌
         ▙▄▄▌  ▙▄▄▌▐    ▐│          │▌
             ▗▄▄▄▄▄▟    ▐╰──────────╯▌
                        ▝▀▀▀▀▀▀▀▀▀▀▀▀▘
")
