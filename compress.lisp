(defun str-get-head (str)
  (cond
    (
      (or
        (eq (length str) 0)
        (null str)
      )

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
      (or
        (eq (length str) 0)
        (null str)
      )

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


(defun str-compress (str)
  (let
    (
      (compressed nil)
      (curch (str-get-head str))
      (str-tail (str-get-tail str))
    )

    (defun str-compress-closure-1 ()
      (let
        (
          (amount 1)
        )

        (defun str-compress-closure-2 ()
          (let
            (
              (nextch (str-get-head str-tail))
            )

            (setf str-tail (str-get-tail str-tail))

            (cond
              ((null curch)
                compressed
              )

              ((string= nextch curch)
                (setf amount (1+ amount))
                (str-compress-closure-2)
              )

              (t
                (setf compressed (append compressed (cons (cons curch amount) nil)))  ; adding dotted pair: (string . number)
                (setf curch nextch)
                (str-compress-closure-1)
              )
            )
          )
        )

        (str-compress-closure-2)
      )
    )

    (str-compress-closure-1)
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

(print (str-compress "aaabbbbccccc"))   ; => (("a" . 3) ("b" . 4) ("c" . 5))
(print (str-compress ""))               ; => NIL
