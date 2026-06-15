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

(uniline-bench
 "aa\t\ta\t\ta\t\t\t\t    \t\t   a
"

 "2*C-k 29*C-y
 M-< 5*<right> <down> 3*<right> <down> 2*<right>
 7*<right> 2*<down> 9*<left> <up> 16*<right> 2*<up> 12*<right> 3*<down> 10*<left>
 5*C-n 14*r <return>
 12*<left> <down> 16*r
 14*<left> <down> 12*r
 2*C-b <kp-subtract> <insert> c
 <return> 12*<right> 3*<down> 11*r
 <down> 9*<left> 12*r
 <down> 8*<left> 8*r
 <down> 2*<left> 2*r
 <down> <left> 3*r
 C-SPC 3*<left> <up> 2*<left> 3*<up> 11*<left> <insert>
 2*<right> c
 <return> 4*<right> 6*<down> 10*<left>
 C-SPC <insert> y
 <down> <right> <return>"

 "\
aa───╮  	a	       ╷a╷      			    		   a
aa   ╰──╮       a	╭──────┼a┼──╮   			    		   a
aa	╰───────a╮      │      │a│  │   			    		   a
aa	╭───────a┼──────╯      │a│  │   			    		   a
aa	╰───────a╯        ╶────┼a┼──╯   			    		   a
aa		a	       │a│      			    		   a
aa		a	       │a│      			    		   a
aa		a	       │a│      			    		   a
aa		a      ╭───────╯a╰────╮ 			    		   a
aa		a      │rrrrrrrrrrrrrr╰───╮     		    		   a
aa		a      ╰─╮rrrrrrrrrrrrrrrr│     		    		   a
aa		a	 ╰─╮rrrrrrrrrrrr╭─╯             	    		   a
aa		a	   ╰───╮a╭──────╯               	    		   a
aa		a	       │a│      	        	    		   a
aa              a              │a│                    rrrrrrrrrrr                  a
aa              a              │a│                      rrrrrrrrrrrr               a
aa              a              │a│                          rrrrrrrr               a
aa              a              │a│                                rr               a
aa              a              │a│                                 rrr             a
aa		a	       │a│      		            		   a
aa              a              │a│                                                 a
aa              a              │a│               rrrrrrrrrrr                       a
aa              a              │a│                 rrrrrrrrrrrr                    a
aa              a              │a│                     rrrrrrrr                    a
aa              a              │a│                           rr                    a
aa              a              │a│                            rrr                  a
aa		a	       │a│      			    		   a
aa		a	       │a│      			    		   a
aa		a	       │a│      			    		   a
                               ╰─╯
"

'uniline-infinite-up↑ nil)

