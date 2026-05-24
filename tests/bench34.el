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

                         ╭──────╮                               ╭────────╮     
                       ╭─┤  Top ├───╮                           │    L   │     
                       │ ╰───┬──╯   │                           ╰────────╯     
            ╭──────────╯     │      ╰──────────╮                               
        ╭───┴──╮         ╭───┴──╮           ╭──┴──╮                            
        │   A  │         │  B   │           │  C  │                            
        ╰─┬───┬╯         ╰───┬──╯           ╰┬───┬╯                            
          │   ╰───╮          │              ╭╯   ╰──╮                          
      ╭───╯       ╰─╮        │             ╭╯       ╰──╮                       
   ╭──┴──╮        ╭─┴──╮   ╭─┴───╮     ╭───┴─╮       ╭─┴───╮                   
   │  D  │        │ E  │   │  F  │     │  G  │       │  H  │                   
   ╰─────╯        ╰─┬──╯   ╰─────╯     ╰─────╯       ╰─────╯                   
                    │                                                          
                    │                                                          
                   ╭┴───╮                                                      
                   │ I  │                                                      
                   ╰────╯                                                      End"

"C-SPC M-> <insert> c <home> <return> <down> <down> C-SPC <insert> y s A <insert> c M-> <return> <down> C-SPC <insert> y s <kp-0> <return>"

"\

                         ╭──────╮                               ╭────────╮     
                       ╭─┤  Top ├───╮                           │    L   │     
                       │ ╰───┬──╯   │                           ╰────────╯     
            ╭──────────╯     │      ╰──────────╮                               
        ╭───┴──╮         ╭───┴──╮           ╭──┴──╮                            
        │   A  │         │  B   │           │  C  │                            
        ╰─┬───┬╯         ╰───┬──╯           ╰┬───┬╯                            
          │   ╰───╮          │              ╭╯   ╰──╮                          
      ╭───╯       ╰─╮        │             ╭╯       ╰──╮                       
   ╭──┴──╮        ╭─┴──╮   ╭─┴───╮     ╭───┴─╮       ╭─┴───╮                   
   │  D  │        │ E  │   │  F  │     │  G  │       │  H  │                   
   ╰─────╯        ╰─┬──╯   ╰─────╯     ╰─────╯       ╰─────╯                   
                    │                                                          
                    │                                                          
                   ╭┴───╮                                                      
                   │ I  │                                                      
                   ╰────╯                                                      End

                                                                                  
                         /------\\                               /--------\\        
                       /-+  Top +---\\                           |    L   |        
                       | \\---+--/   |                           \\--------/        
            /----------/     |      \\----------\\                                  
        /---+--\\         /---+--\\           /--+--\\                               
        |   A  |         |  B   |           |  C  |                               
        \\-+---+/         \\---+--/           \\+---+/                               
          |   \\---\\          |              //   \\--\\                             
      /---/       \\-\\        |             //       \\--\\                          
   /--+--\\        /-+--\\   /-+---\\     /---+-\\       /-+---\\                      
   |  D  |        | E  |   |  F  |     |  G  |       |  H  |                      
   \\-----/        \\-+--/   \\-----/     \\-----/       \\-----/                      
                    |                                                             
                    |                                                             
                   /+---\\                                                         
                   | I  |                                                         
                   \\----/                                                      End

                                                                                  
                         ╭──────╮                               ╭────────╮        
                       ╭─┤  T◦p ├───╮                           │    L   │        
                       │ ╰───┬──╯   │                           ╰────────╯        
            ╭──────────╯     │      ╰──────────╮                                  
        ╭───┴──╮         ╭───┴──╮           ╭──┴──╮                               
        │   A  │         │  B   │           │  C  │                               
        ╰─┬───┬╯         ╰───┬──╯           ├┬───┬╯                               
          │   ╰───╮          │              ├╯   ╰──╮                             
      ╭───╯       ├─╮        │             ╭╯       ╰──╮                          
   ╭──┴──╮        ├─┴──╮   ╭─┴───╮     ╭───┴─╮       ╭─┴───╮                      
   │  D  │        │ E  │   │  F  │     │  G  │       │  H  │                      
   ╰─────╯        ╰─┬──╯   ╰─────╯     ╰─────╯       ╰─────╯                      
                    │                                                             
                    │                                                             
                   ╭┴───╮                                                         
                   │ I  │                                                         
                   ╰────╯                                                      End
")
