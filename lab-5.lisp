; last
(defun fetch-last (lst)
  (cond
    ((null (cdr lst))
      (car lst)
    )

    (t
      (fetch-last (cdr lst))
    )
  )
)

; delete last
(defun without-last (lst)
  (cond
    ((null (cdr lst))
      nil
    )

    (t
      (cons (car lst) (without-last (cdr lst)))
    )
  )
)

; member
(defun has-value (lst val)
  (cond
    ((eql (car lst) val)
      t
    )

    ((null (cdr lst))
      nil
    )

    (t
      (has-value (cdr lst) val)
    )
  )
)

; reverse
(defun reverse-list (lst)
  (let
    (
      (temp-lst (cons lst nil))
    )

    (defun reverse-list-closure ()
      (cond
        ((null (car temp-lst))
          (cdr temp-lst)
        )

        (t
          (setf temp-lst
            (cons
              (without-last (car temp-lst))
              (append
                (cdr temp-lst)
                (list (fetch-last (car temp-lst)))
              )
            )
          )

          (reverse-list-closure)
        )
      )
    )
  )

  (reverse-list-closure)
)

; length
(defun get-size (lst)
  (let
    (
      (size 0)
    )

    (defun get-size-closure ()
      (cond
        ((null (car lst))
          size
        )

        (t
          (setf size (1+ size))
          (setf lst (cdr lst))
          (get-size-closure)
        )
      )
    )

    (get-size-closure)
  )
)

;; tests

(print (without-last '(1 2 3 4 5)))
(print (fetch-last '(1 2 3 4 5)))
(print (has-value '(1 2 3 4 5) 4))
(print (has-value '(1 2 3 4 5) 8))
(print (reverse-list '(1 2 3 4 5)))

; `get-size` test
(let
  (
    (lst-1 '(1 2 3))
    (lst-2 '(1 2 3))
  )
  
  (cond
    ((> (get-size lst-1) (get-size lst-2))
      (print "lst-1.size > lst-2.size")
    )

    (t
      (print "lst-1.size <= lst-2.size")
    )
  )
)
