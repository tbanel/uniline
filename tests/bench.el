(defun bench (commands result)
  (setq result
        (substring result (1+ (string-search "\n" result))))

  (eval
   `(defalias 'bench-function (kmacro ,commands)))
  
  (delete-file "/tmp/ttt.txt")
  (ignore-errors (kill-buffer "ttt.txt"))
  (find-file "/tmp/ttt.txt")

  (uniline-mode 1)
  (bench-function)

  (goto-char (point-min))
  (save-buffer)

  (delete-file "/tmp/ttt1.txt")
  (ignore-errors (kill-buffer "ttt1.txt"))
  (find-file-other-window "/tmp/ttt1.txt")
  (insert result)

  (goto-char (point-min))
  (save-buffer)

  (compare-windows nil)
  
  (if (string-equal
       (with-current-buffer "ttt.txt"
         (buffer-substring (point-min) (point-max)))
       (with-current-buffer "ttt1.txt"
         (buffer-substring (point-min) (point-max))))
      (message "test PASSED")
    (message "test FAILED"))
  )


(defun bench-create ()
  (interactive)
  (delete-file "/tmp/ttt.txt")
  (ignore-errors (kill-buffer "ttt.txt"))
  (find-file "/tmp/ttt.txt")
  
  (uniline-mode)

  (local-set-key "$" 'bench-collect)

  (kmacro-start-macro nil))

(defun bench-collect ()
  (interactive)

  (kmacro-end-macro 1)
  (save-buffer)

  (ignore-errors (kill-buffer "b.el"))
  (find-file "b.el")

  (insert "(load-file \"bench.el\")\n")
  
  (insert "(bench\n\"")

  (insert (key-description (kmacro--keys (kmacro last-kbd-macro))))

  (insert "\"\n\"\n")
  
  (insert
   (with-current-buffer "ttt.txt"
     (buffer-substring (point-min) (point-max))))

  (insert "\")\n")
  )
