(load-file "bench.el")

(bench
 "C-c C-c i n i t i a l SPC t e x t e <backspace> <return> SPC SPC g o o d <return> <backspace> <backspace> M-x u n i l i n e - m o d e RET <up> <up> C-SPC <down> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <insert> <down> <down> <right> <right> <right> R <end> <return> <kp-subtract> <right> q <kp-subtract> <down> <left> <left> <return> <up> <up> <right> <right> <down> <kp-subtract> <up> <right> <right> <right> <up> <up> <up> <return> <down> <down> <down> <up> <up> <right> <down> <down> <down> C-SPC <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <left> <up> <up> <up> <insert> <up> <down> <down> c <end> SPC SPC C-SPC <insert> y <return> <return> <down> <down> <down> <right> <right> <right> <right> = <down> <left> <left> <left> <left> <left> <left> <insert> s s s <left> <left> <left> <left> <left> <insert> o <left> <left> <left> <left> <left> <left> <up>"

 "
                    ╷ 
                    │ 
  ╭────────────╮ ╭──╯ ╭────────────╮ ╭──╯ 
  │initial text│ q    │initial text│ q    
  ││ good      ├─╯    ││ good      ├─╯    
  ╰──────╥─────╯      ╰───╥────────╯      
         ╚═════·════▫═════╝
")
