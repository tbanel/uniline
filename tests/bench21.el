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

;; Test rectangle tracing & moving

(uniline-bench
 "<return>
 5*<down> 11*<right>
 15*S-<down> 31*S-<right> 2*S-<left>
 # <insert> R
 2*<return>
 <down> <right>
 <down> 3*<right> <left>
 13*S-<down> 25*S-<right> S-<left> S-<right>
 <insert> # r
 2*<return>
 2*<down> 3*<right>
 9*S-<down> 18*S-<right>
 <insert> <kp-subtract> r
 2*<return>
 2*<down> 3*<right>
 5*S-<down> 11*S-<right>
 <insert> S-<right> = r
 2*<return>
 5*<up> 2*<down> 3*<right> 4*<up>
 19*S-<down> 16*S-<left>
 <insert> 2*<left>
 2*<return>
 9*S-<down> 27*S-<right> 12*S-<right>
 <insert> 2*<up>
 2*<return>
 15*<down> 2*<up>
 27*S-<right> 12*S-<right> 9*S-<down>
 <insert> 2*<down>
 2*<return>
 19*<right> 9*<down>
 24*S-<up> 21*S-<right>
 <insert> 2*<right>
 <return>"
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
