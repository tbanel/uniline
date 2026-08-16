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
"\


      xxxxx      xxxxx      xxxxx      xxxxx
      x   x      x   x      x   x      x   x
      x a x      x b x      x A x      x B x
      x   x      x   x      x   x      x   x
      xxxxx      xxxxx      xxxxx      xxxxx




      xxxxx      xxxxx      xxxxx      xxxxx
      x   x      x   x      x   x      x   x
      x c x      x d x      x C x      x D x
      x   x      x   x      x   x      x   x
      xxxxx      xxxxx      xxxxx      xxxxx




      xxxxx      xxxxx      xxxxx      xxxxx
      x   x      x   x      x   x      x   x
      x s x      x t x      x S x      x T x
      x   x      x   x      x   x      x   x
      xxxxx      xxxxx      xxxxx      xxxxx




      xxxxx      xxxxx      xxxxx      xxxxx
      x   x      x   x      x   x      x   x
      x - x      x + x      x = x      x # x
      x   x      x   x      x   x      x   x
      xxxxx      xxxxx      xxxxx      xxxxx

"

"<return> <down> <down>
 <right> <right> <right> <right> <right> <right> <insert> b a <insert> c <return>
 <right> <right> <right> <right> <right> <right> <insert> b b <insert> c <return>
 <right> <right> <right> <right> <right> <right> <insert> b A <insert> c <return>
 <right> <right> <right> <right> <right> <right> <insert> b B <insert> c <return>
 <home> <down> <down> <down> <down> <down> <down> <down> <down> <down>
 <right> <right> <right> <right> <right> <right> <insert> b c <insert> c <return>
 <right> <right> <right> <right> <right> <right> <insert> b d <insert> c <return>
 <right> <right> <right> <right> <right> <right> <insert> b C <insert> c <return>
 <right> <right> <right> <right> <right> <right> <insert> b D <insert> c <return>
 <home> <down> <down> <down> <down> <down> <down> <down> <down> <down>
 <right> <right> <right> <right> <right> <right> <insert> b s <insert> c <return>
 <right> <right> <right> <right> <right> <right> <insert> b t <insert> c <return>
 <right> <right> <right> <right> <right> <right> <insert> b S <insert> c <return>
 <right> <right> <right> <right> <right> <right> <insert> b T <insert> c <return>
 <home> <down> <down> <down> <down> <down> <down> <down> <down> <down>
 <right> <right> <right> <right> <right> <right> <insert> <kp-subtract> <insert> c <return>
 <right> <right> <right> <right> <right> <right> <insert> <kp-add>      <insert> c <return>
 <right> <right> <right> <right> <right> <right> <insert> =             <insert> c <return>
 <right> <right> <right> <right> <right> <right> <insert> #             <insert> c <return>"

"\

     ╭─────╮    ╭─────╮    ▖─────╮    ╭─────▗
     ▌xxxxx┤    │xxxxx╮    ▌xxxxx┤    │xxxxx▐
     ▌x   x│    │x   x▐    ▌x   x│    │x   x▐
     ▌x a x│    │x b x▐    ▌x A x│    │x B x▐
     ▌x   x│    │x   x▐    ▌x   x│    │x   x▐
     ▌xxxxx│    │xxxxx▐    ▌xxxxx│    │xxxxx▐
     ▙▄▄▄▄▄╯    ╰▄▄▄▄▄▟    ▙▄▄▄▄▄▖    ▗▄▄▄▄▄▟


     ╭─────╮    ╭─────╮    ╓─────╮    ╭─────╖
     ║xxxxx┤    │xxxxx╢    ║xxxxx┤    │xxxxx╢
     ║x   x│    │x   x║    ║x   x│    │x   x║
     ║x c x│    │x d x║    ║x C x│    │x D x║
     ║x   x│    │x   x║    ║x   x│    │x   x║
     ║xxxxx│    │xxxxx║    ║xxxxx│    │xxxxx║
     ╚═════╯    ╰═════╝    ╚═════╛    ╘═════╝


     ╭─────╮    ╭─────╮    ┎─────╮    ╭─────┒
     ┃xxxxx┤    │xxxxx┦    ┃xxxxx┤    │xxxxx┨
     ┃x   x│    │x   x┃    ┃x   x│    │x   x┃
     ┃x s x│    │x t x┃    ┃x S x│    │x T x┃
     ┃x   x│    │x   x┃    ┃x   x│    │x   x┃
     ┃xxxxx│    │xxxxx┃    ┃xxxxx│    │xxxxx┃
     ┗━━━━━╯    ╰━━━━━┛    ┗━━━━━┙    ┕━━━━━┛


     ╭─────╮    ┏━━━━━┓    ╔═════╗    ▗▄▄▄▄▄▖
     │xxxxx│    ┃xxxxx┃    ║xxxxx║    ▐xxxxx▌
     │x   x│    ┃x   x┃    ║x   x║    ▐x   x▌
     │x - x│    ┃x + x┃    ║x = x║    ▐x # x▌
     │x   x│    ┃x   x┃    ║x   x║    ▐x   x▌
     │xxxxx│    ┃xxxxx┃    ║xxxxx║    ▐xxxxx▌
     ╰─────╯    ┗━━━━━┛    ╚═════╝    ▝▀▀▀▀▀▘
"
'uniline-infinite-up↑ t
'uniline-prefix-for-setting-brush t)
