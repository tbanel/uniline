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
 "<return> <down> <down> M-3 <right>
 <kp-subtract> M-9 <right> M-3 <down> M-5 <left> M-4 <up> M-11 <right> <down> <down>
 <insert> a
 <down> <left> <left> <left> <down>
 <insert> a a
 <down> <left> <left> <left>
 <insert> a a a
 <left> <left> <left>
 <insert> a a a a
 <left> <left> <left> <left> <up>
 <insert> a a a a a
 <up> <up> <left> <left> <up>
 <return> <up>
 C-x SPC M-5 <down> M-16 <right>
 <insert> c <return>
 M-7 <down> M-0 <left>
 C-SPC <insert> y s 3 <return>
 <return> <down> <up> M-20 <right>
 C-SPC <insert> y s 4 <return>
 <up> <up> <up> <up> <up> <up> <up>
 C-SPC <insert> y s h <return>
 M-15 <down> M-20 <left>
 C-x SPC <insert> y s + <return>
 M-21 <right> <left>
 C-x SPC y <backspace>
 C-x SPC <insert> y s 3 <return>
 C-SPC M-5 <down> M-16 <right>
 <insert> s 3 + <return>
 M-15 <up> M-20 <right>
 C-SPC <insert> y s 4 + <return>
 <down> <down> <down> <return> M-5 <down> <up> <up> <down> <left> <right> <left>
 <return>
 C-x SPC M-16 <right> M-5 <down> M-5 <up>
 <return>
 M-15 <left>
 C-x SPC <insert> y s + <return>
 C-x SPC M-5 <down> M-16 <right>
 <insert> s 3 + 0 <return>"
"\
 
       ╭──────────╮        ┌──────────┐        ┏┉┉┉┉┉┉┉┉┉┉┓
   ╭───┼────╮     │    ┌───┼────┐     │    ┏┉┉┉╋┉┉┉┉┓     ┋
   ╰─╮ │    │     ▽    └─┐ │    │     ▽    ┗┉┓ ┋    ┋     ▼
     │ │    │  ╭──╯      │ │    │  ┌──┘      ┋ ┋    ┋  ┏┉┉┛
     ▴ ╰────╯  ▼         ▴ └────┘  ▼         ▴ ┗┉┉┉┉┛  ▼   
     ╰───◃──←──╯         └───◃──←──┘         ┗┉┉┉◂┉┉←┉┉┛   
                                            
       ╭╌╌╌╌╌╌╌╌╌╌╮        ╭┈┈┈┈┈┈┈┈┈┈╮       ┏━━━━━━━━━━┓
   ╭╌╌╌┼╌╌╌╌╮     ┆    ╭┈┈┈┼┈┈┈┈╮     ┊   ┏━━━╋━━━━┓     ┃
   ╰╌╮ ┆    ┆     ▽    ╰┈╮ ┊    ┊     ▽   ┗━┓ ┃    ┃     ▼
     ┆ ┆    ┆  ╭╌╌╯      ┊ ┊    ┊  ╭┈┈╯     ┃ ┃    ┃  ┏━━┛
     ▴ ╰╌╌╌╌╯  ▼         ▴ ╰┈┈┈┈╯  ▼        ▴ ┗━━━━┛  ▼   
     ╰╌╌╌◃╌╌←╌╌╯         ╰┈┈┈◃┈┈←┈┈╯        ┗━━━◂━━←━━┛   
                                          
                        
       ┏━━━━━━━━━━┓        ┏╍╍╍╍╍╍╍╍╍╍┓
   ┏━━━╋━━━━┓     ┃    ┏╍╍╍╋╍╍╍╍┓     ┇
   ┗━┓ ┃    ┃     ▼    ┗╍┓ ┇    ┇     ▼
     ┃ ┃    ┃  ┏━━┛      ┇ ┇    ┇  ┏╍╍┛
     ▴ ┗━━━━┛  ▼         ▴ ┗╍╍╍╍┛  ▼   
     ┗━━━◂━━←━━┛         ┗╍╍╍◂╍╍←╍╍┛   
                       
")
