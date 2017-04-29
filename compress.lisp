(defun str-get-head (str)
  (cond
    (
      (eq (length str) 0)
      nil
    )

    (t
      (subseq str 0 1)
    )
  )
)


(defun str-get-tail (str)
  (cond
    (
      (eq (length str) 0)
      nil
    )

    (t
      (subseq str 1)
    )
  )
)


(defun str-is-empty? (str)
  (string= str "")
)

(defun str-iterate (str)
  (let 
    (
      (curch nil)
    )
  )

  (lambda ()
    (setf curch (str-get-head str))
    (setf str (str-get-tail str))
    curch
  )
)

;; tests
(setq next-char (str-iterate "test"))

(print (funcall next-char))     ; => "t"
(print (funcall next-char))     ; => "e"
(print (funcall next-char))     ; => "s"
(print (funcall next-char))     ; => "t"
(print (funcall next-char))     ; => NIL
(print (funcall next-char))     ; => NIL

(print (str-is-empty? "test"))  ; => NIL
(print (str-is-empty? ""))      ; => T
