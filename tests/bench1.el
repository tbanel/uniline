(load-file "bench.el")

(bench
 "a b RET <right> c <right> - <right> <right> <right> <down> <left> <left> <left> <left> <left> <up> C-SPC <down> <right> <right> <right> <right> <right> <right> <insert> <down> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> RET RET <down> <right> <right> + <down> <left> <left> <left> <left> <insert> a a <left> <left> <left> <insert> a S-<up> <left> <left> <left> <left> <left> <left> <left> <left> RET <up> <up> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> <right> - <up> <up> <up>"

 "
              │
           bc╶┴─╮
           ╰─┰──╯ 
━━━━━━△━━◀━━━┛   
")
