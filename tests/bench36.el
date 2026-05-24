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

;; Test the ASCII ↔ UNICODE filters

(uniline-bench
"\
                                                                                                      │         
  ╒══════ruĝema═════════╕ ╒══════bluema══════════╕ ╒════════nigrema═══════╕ ╒════════klarema══════╕   │         
  ╵                     ╵ ╵                      ╵ ╵                      ╵ ╵                     ╵   │         
╸ ╒════╕ ╒══════╕ ╒═════╕ ╒═════╕ ╒════╕ ╒═══════╕ ╒═════╕ ╒═════╕ ╒══════╕ ╒══════╕ ╒════╕ ╒═════╕ ╺═╧═╸       
  │ruĝa├─┼oranĝa├─┼flava├─┼verda├─┼blua├─┼violeta├─┼nigra├─┼graja├─┼blanka├─┼marona├─┼rosa├─┼ciana│             
  ╰─┬──╯ ╰─┬────╯ ╰─┬───╯ ╰─┬───╯ ╰─┬──╯ ╰─┬─────╯ ╰─┬───╯ ╰─┬───╯ ╰─┬────╯ ╰─┬────╯ ╰─┬──╯ ╰─┬───╯             
    ▽      ▽        ▽       ▽       ▽      ▽         ▽       ▽       ▽        ▽        ▽      ▽                 
    □      □        □       □       □      □         □       □       □        □        □      □                 
                                                                                                                
 0         1         2         3         4         5         6         7         8         9         10         
 ╷         ╷         ╷         ╷         ╷         ╷         ╷         ╷         ╷         ╷         ╷          
 │    ╷    │    ╷    │    ╷    │    ╷    │    ╷    │    ╷    │    ╷    │    ╷    │    ╷    │    ╷    │    ╷     
 │╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷ 
╺╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧┵┴┴┴┴┴┴┴┴╴"

"C-SPC M-> <insert> c <return> M-> C-q C-j C-q C-j C-SPC <insert> y s A <insert> c M-> C-q C-j C-q C-j C-SPC <insert> y s <kp-0> <return>"

"\
                                                                                                      │         
  ╒══════ruĝema═════════╕ ╒══════bluema══════════╕ ╒════════nigrema═══════╕ ╒════════klarema══════╕   │         
  ╵                     ╵ ╵                      ╵ ╵                      ╵ ╵                     ╵   │         
╸ ╒════╕ ╒══════╕ ╒═════╕ ╒═════╕ ╒════╕ ╒═══════╕ ╒═════╕ ╒═════╕ ╒══════╕ ╒══════╕ ╒════╕ ╒═════╕ ╺═╧═╸       
  │ruĝa├─┼oranĝa├─┼flava├─┼verda├─┼blua├─┼violeta├─┼nigra├─┼graja├─┼blanka├─┼marona├─┼rosa├─┼ciana│             
  ╰─┬──╯ ╰─┬────╯ ╰─┬───╯ ╰─┬───╯ ╰─┬──╯ ╰─┬─────╯ ╰─┬───╯ ╰─┬───╯ ╰─┬────╯ ╰─┬────╯ ╰─┬──╯ ╰─┬───╯             
    ▽      ▽        ▽       ▽       ▽      ▽         ▽       ▽       ▽        ▽        ▽      ▽                 
    □      □        □       □       □      □         □       □       □        □        □      □                 
                                                                                                                
 0         1         2         3         4         5         6         7         8         9         10         
 ╷         ╷         ╷         ╷         ╷         ╷         ╷         ╷         ╷         ╷         ╷          
 │    ╷    │    ╷    │    ╷    │    ╷    │    ╷    │    ╷    │    ╷    │    ╷    │    ╷    │    ╷    │    ╷     
 │╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷│╷╷╷╷ 
╺╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧╧┵┴┴┴┴┴┴┴┴╴

                                                                                                      |         
  /======ruĝema=========\\ /======bluema==========\\ /========nigrema=======\\ /========klarema======\\   |         
  '                     ' '                      ' '                      ' '                     '   |         
- /====\\ /======\\ /=====\\ /=====\\ /====\\ /=======\\ /=====\\ /=====\\ /======\\ /======\\ /====\\ /=====\\ -=+=-       
  |ruĝa+-+oranĝa+-+flava+-+verda+-+blua+-+violeta+-+nigra+-+graja+-+blanka+-+marona+-+rosa+-+ciana|             
  \\-+--/ \\-+----/ \\-+---/ \\-+---/ \\-+--/ \\-+-----/ \\-+---/ \\-+---/ \\-+----/ \\-+----/ \\-+--/ \\-+---/             
    v      v        v       v       v      v         v       v       v        v        v      v                 
    o      o        o       o       o      o         o       o       o        o        o      o                 
                                                                                                                
 0         1         2         3         4         5         6         7         8         9         10         
 |         |         |         |         |         |         |         |         |         |         |          
 |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |     
 |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| 
-++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-


                                                                                                      │         
  ╒══════ruĝema═════════╕ ╒══════bluema══════════╕ ╒════════nigrema═══════╕ ╒════════klarema══════╕   │         
  │                     │ │                      │ │                      │ │                     │   │         
- ╞════╕ ╒══════╕ ╒═════╡ ╞═════╕ ╒════╕ ╒═══════╡ ╞═════╕ ╒═════╕ ╒══════╡ ╞══════╕ ╒════╕ ╒═════╡ ─═╧═─       
  │ruĝa├─┤oranĝa├─┤flava├─┤verda├─┤blua├─┤violeta├─┤nigra├─┤graja├─┤blanka├─┤marona├─┤r◦sa├─┤ciana│             
  ╰─┬──╯ ╰─┬────╯ ╰─┬───╯ ╰─┬───╯ ╰─┬──╯ ╰─┬─────╯ ╰─┬───╯ ╰─┬───╯ ╰─┬────╯ ╰─┬────╯ ╰─┬──╯ ╰─┬───╯             
    ▽      ▽        ▽       ▽       ▽      ▽         ▽       ▽       ▽        ▽        ▽      ▽                 
    ◦      ◦        ◦       ◦       ◦      ◦         ◦       ◦       ◦        ◦        ◦      ◦                 
                                                                                                                
 0         1         2         3         4         5         6         7         8         9         10         
 │         │         │         │         │         │         │         │         │         │         │          
 │    │    │    │    │    │    │    │    │    │    │    │    │    │    │    │    │    │    │    │    │    │     
 ││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││ 
─┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴─
")
