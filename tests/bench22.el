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
"<return> <down> <down> <right> <right> <right> <kp-subtract> <right> <right> <right> <right> <right> <right> <right> <right> <right> <down> <down> <down> <left> <left> <left> <left> <left> <up> <up> <up> <up> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <down> <down> <insert> a <down> <left> <left> <left> <down> <insert> a a <down> <left> <left> <left> <insert> a a a <left> <left> <left> <insert> a a a a <left> <left> <left> <left> <up> <insert> a a a a a <up> <up> <left> <left> <up> <return> <up> C-x SPC <down> <down> <down> <down> <down> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <insert> c <return> <down> <down> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> C-SPC <insert> y s 3 <return> <return> <down> <up> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> C-SPC <insert> y s 4 <return> <up> <up> <up> <up> <up> <up> <up> C-SPC <insert> y s h <return> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <down> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> C-x SPC <insert> y s + <return> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <left> C-x SPC y <backspace> C-x SPC <insert> y 3 <return> C-SPC <down> <down> <down> <down> <down> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <insert> s 3 + <return> <up> <up> <up> <up> <up> <up> <up> <up> <up> <up> <up> <up> <up> <up> <up> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> C-SPC <insert> y s 4 + <return> <down> <down> <down> <return> <down> <down> <down> <down> <down> <up> <up> <down> <left> <right> <left> <return> C-x SPC <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <down> <down> <down> <down> <down> <up> <up> <up> <up> <up> <return> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> C-x SPC <insert> y s + C-x SPC <down> <down> <down> <down> <down> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <insert> s 3 + 0 <return>"
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
