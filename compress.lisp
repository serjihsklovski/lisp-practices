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

(format t "1. String-Iterator~%~%")

(let
  (
    (next (str-iterate "test"))
  )

  (format t "  str = \"test\"~%")
  (format t "  str.next() => ~A~%" (funcall next))
  (format t "  str.next() => ~A~%" (funcall next))
  (format t "  str.next() => ~A~%" (funcall next))
  (format t "  str.next() => ~A~%" (funcall next))
  (format t "  str.next() => ~A~%~%" (funcall next))
)

(let
  (
    (test-str-src "aaabbbbccccc")
    (test-lst-compressed nil)
    (test-str-decompressed nil)
  )

  (format t "2. Compress string~%~%")

  (format t "  str = \"~A\"~%" test-str-src)

  (setf test-lst-compressed (str-compress test-str-src))

  (format t "  compressed = ~A~%~%" test-lst-compressed)


  (format t "3. Decompress string~%~%")

  (setf test-str-decompressed (str-decompress test-lst-compressed))

  (format t "  decompressed = \"~A\"~%~%" test-str-decompressed)
)
