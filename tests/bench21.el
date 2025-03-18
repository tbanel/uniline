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
"<return> <down> <down> <down> <down> <down> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<left> S-<left> # <insert> R <return> <return> <down> <right> <right> <right> <left> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<left> S-<right> <insert> # r <return> <return> <down> <down> <right> <right> <right> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> <insert> <kp-subtract> r <return> <return> <down> <down> <right> <right> <right> S-<down> S-<down> S-<down> S-<down> S-<down> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> <insert> S-<right> = r <return> <return> <up> <up> <up> <up> <up> <down> <down> <right> <right> <right> <up> <up> <up> <up> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<left> S-<left> S-<left> S-<left> S-<left> S-<left> S-<left> S-<left> S-<left> S-<left> S-<left> S-<left> S-<left> S-<left> S-<left> S-<left> <insert> <left> <left> <return> <return> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> <insert> <up> <up> <return> <return> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <up> <up> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> S-<down> <insert> <down> <down> <return> <return> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <down> <down> <down> <down> <down> <down> <down> <down> <down> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<up> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> S-<right> <insert> <right> <right> <return>"
"\
 
                                               
        ▗▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▖    
        ▐                                 ▌    
        ▐  ▛▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▜  ▌    
        ▐  ▌                           ▐  ▌    
        ▐  ▌  ╭────────────────────╮   ▐  ▌    
        ▐  ▌  │                    │   ▐  ▌    
        ▐  ▌  │   ╔═════════════╗  │   ▐  ▌    
        ▐  ▌  │   ║             ║  │   ▐  ▌    
        ▐  ▌  │   ║             ║  │   ▐  ▌    
        ▐  ▌  │   ║             ║  │   ▐  ▌    
        ▐  ▌  │   ║             ║  │   ▐  ▌    
        ▐  ▌  │   ║             ║  │   ▐  ▌    
        ▐  ▌  │   ║             ║  │   ▐  ▌    
        ▐  ▌  │   ║             ║  │   ▐  ▌    
        ▐  ▌  │   ║             ║  │   ▐  ▌    
        ▐  ▌  │   ╚═════════════╝  │   ▐  ▌    
        ▐  ▌  │                    │   ▐  ▌    
        ▐  ▌  ╰────────────────────╯   ▐  ▌    
        ▐  ▌                           ▐  ▌    
        ▐  ▙▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▟  ▌    
        ▐                                 ▌    
        ▝▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▘    
                                               
                                               
")
