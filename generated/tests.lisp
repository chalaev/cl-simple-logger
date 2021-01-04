(deftest format-msg.1
(let* ((fstr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
       (just-a-variable 345)
       (typical-values (make-hash-table)))
;;(SL:start)
(SHM:hset typical-values 'start-time '(3818619513 . 5385255))
(SHM:hset typical-values 'get-internal-real-time-ms 5432832)
(let((msg(list SL:debug (gethash 'get-internal-real-time-ms typical-values) (list "abc ~d" just-a-variable))))
(with-output-to-string (test-stream fstr)
  (SL::format-msg test-stream msg (gethash 'start-time typical-values)))
fstr))
"01/02 18:39:20.577 DEBUG abc 345
")
