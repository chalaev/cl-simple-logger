(deftest format-msg.1
(let* ((fstr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
       (just-a-variable 345)
       (typical-values (make-hash-table)))
;;(SL:start)
(shalaev/macros:hset typical-values 'start-time '(3815366330000 . 14448687))
(shalaev/macros:hset typical-values 'get-internal-real-time 121111111)
(let ((msg (list SL:debug (gethash 'get-internal-real-time typical-values) (list "abc ~d" just-a-variable))))
(with-output-to-string (test-stream fstr)
  (SL::format-msg test-stream msg (gethash 'start-time typical-values)))
fstr))
"02/13 22:51:12.992 DEBUG abc 345
")
