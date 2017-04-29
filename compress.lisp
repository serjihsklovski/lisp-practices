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


(defun lst-iterate (lst)
  (let 
    (
      (curval nil)
    )
  )

  (lambda ()
    (setf curval (car lst))
    ; (setf lst (cdr lst))
    curval
  )
)


(defun str-repeat (str times)
  (let
    (
      (res "")
    )

    (defun str-repeat-closure ()
      (cond
        ((> times 0)
          (setf res (concatenate 'string res str))
          (setf times (1- times))
          (str-repeat-closure)
        )

        (t res)
      )
    )

    (str-repeat-closure)
  )
)


(defun str-compress (str)
  (let
    (
      (compressed nil)
      (curch (str-get-head str))
      (str-tail (str-get-tail str))
      (amount 1)
    )

    (defun str-compress-closure ()
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
            (str-compress-closure)
          )

          (t
            (setf compressed (append compressed (cons (cons curch amount) nil)))  ; adding dotted pair: (string . number)
            (setf curch nextch)
            (setf amount 1)
            (str-compress-closure)
          )
        )
      )
    )

    (str-compress-closure)
  )
)


(defun str-decompress (lst)
  (let
    (
      (decompressed "")
    )

    (defun str-decompress-closure ()
      (cond
        ((null lst)
          decompressed
        )

        (t
          (let
            (
              (curpair (car lst))
            )

            (setf
              decompressed

              (concatenate 'string
                decompressed

                (str-repeat
                  (car curpair)
                  (cdr curpair)
                )
              )
            )

            (setf lst (cdr lst))

            (str-decompress-closure)
          )
        )
      )
    )

    (str-decompress-closure)
  )
)


;; tests
(setq next (str-iterate "test"))

(print (funcall next))      ; => "t"
(print (funcall next))      ; => "e"
(print (funcall next))      ; => "s"
(print (funcall next))      ; => "t"
(print (funcall next))      ; => NIL
(print (funcall next))      ; => NIL


(setq next (lst-iterate '(1 2 3 4 5)))

(print (funcall next))      ; => 1
(print (funcall next))      ; => 2
(print (funcall next))      ; => 3
(print (funcall next))      ; => 4
(print (funcall next))      ; => 5
(print (funcall next))      ; => NIL


(print (str-is-empty? "test"))  ; => NIL
(print (str-is-empty? ""))      ; => T

(print (str-compress "aaabbbbccccc"))   ; => (("a" . 3) ("b" . 4) ("c" . 5))
(print (str-compress ""))               ; => NIL

(print (str-decompress '(("a" . 3) ("b" . 4) ("c" . 5))))   ; => "aaabbbbccccc"
(print (str-decompress nil))            ; => ""
