;; 1. user `mapcar` & `maplist`

; my `mapcar`

(defun List-Iterator (lst)
  (let 
    (
      (curval nil)
    )
  )

  (lambda ()
    (setf curval (car lst))
    (setf lst (cdr lst))
    curval
  )
)


(defun Matrix-Column-Iterator (mtrx)
  (let*
    (
      (iterators nil)
      (row-iterator (List-Iterator mtrx))
      (current-row (funcall row-iterator))
      (column-iterator nil)
    )

    (defun Matrix-Column-Iterator-init-iterators ()
      (cond
        ((not (null current-row))
          (setf iterators (append iterators (list (List-Iterator current-row))))
          (setf current-row (funcall row-iterator))
          (Matrix-Column-Iterator-init-iterators)
        )
      )
    )

    (Matrix-Column-Iterator-init-iterators)

    (lambda ()
      (let
        (
          (current-column nil)
          (curval nil)
          (curiter nil)
        )

        (setf column-iterator (List-Iterator iterators))

        (defun Matrix-Column-Iterator-init-column ()
          (setf curiter (funcall column-iterator))

          (cond
            ((not (null curiter))
              (setf curval (funcall curiter))

              (cond
                ((not (null curval))
                  (setf current-column (append current-column (list curval)))
                  (Matrix-Column-Iterator-init-column)
                )
              )
            )
          )
        )

        (Matrix-Column-Iterator-init-column)

        current-column
      )
    )
  )
)


(defun my-map (func &rest lst-args)
  (let
    (
      (result nil)
      (argiter (Matrix-Column-Iterator lst-args))
      (args nil)
    )

    (defun my-map-closure ()
      (setf args (funcall argiter))

      (cond
        ((not (null args))
          (setf result (append result (list (apply func args))))
          (my-map-closure)
        )
      )
    )

    (my-map-closure)

    result
  )
)


; my `maplist`

(defun my-maplist (lst func)
  (let
    (
      (result nil)
    )

    (defun my-maplist-init-result (templst)
      (setf result (append result (list (apply func (list templst)))))

      (cond
        ((not (null (cdr templst)))
          (my-maplist-init-result (cdr templst))
        )
      )
    )

    (my-maplist-init-result lst)

    result
  )
)


;; 2. my `funcall`

(defun call (func &rest args)
  (apply func args)
)


;; 3. cartesian product

(defun cartesian-product (set-1 set-2)
  (let
    (
      (result nil)
    )

    (mapcar
      (lambda (x)
        (mapcar
          (lambda (y)
            (setf result (append result (list (cons x y))))
          )

          set-2
        )
      )

      set-1
    )

    result
  )
)


;; 4. Fibonacci generator

(defun Fibonacci-Generator ()
  (let*
    (
      (a 0)
      (b 1)
      (c b)
    )
      
    (lambda ()
      (setf c b)
      (setf b (+ a b))
      (setf a c)
    )
  )
)


;; 5. Natural numbers generator

(defun N-Generator ()
  (let
    (
      (n 0)
    )

    (lambda ()
      (setf n (1+ n))
    )
  )
)


;; tests

(format t "1. User `mapcar` & `maplist`~%~%")

(format t "1.1. My `mapcar`~%~%")

(format t "  f(x, y) = x * y~%")
(format t "  X = {1, 2, 3, 4, 5}~%")
(format t "  Y = {6, 7, 8, 9, 10}~%")
(format t "  map(f, X, Y) = ~A~%~%"
  (my-map (lambda (x y) (* x y)) '(1 2 3 4 5) '(6 7 8 9 10))
)


(format t "1.2. My `maplist`~%~%")

(format t "  f(X) = reverse~%")
(format t "  X = {1, 2, 3, 4, 5, 6}~%")
(format t "  maplist(X, f) = ~A~%~%"
  (my-maplist '(1 2 3 4 5 6) #'reverse)
)


(format t "2. User `funcall`~%~%")

(format t "  f(x, y) = x * y~%")
(format t "  call(f, 4, 5) = ~A~%~%"
  (call (lambda (x y) (* x y)) 4 5)
)


(format t "3. Cartesian product of two sets~%~%")

(format t "  X = {1, 2, 3}~%")
(format t "  Y = {4, 5, 6}~%")
(format t "  cartesian-product(X, Y) = ~A~%~%"
  (cartesian-product '(1 2 3) '(4 5 6))
)


(format t "4. Fibonacci generator~%~%")

(let
  (
    (fibgen-1 (Fibonacci-Generator))
    (fibgen-2 (Fibonacci-Generator))
  )

  (format t "  fibgen-1: ~A~%" (funcall fibgen-1))
  (format t "  fibgen-1: ~A~%" (funcall fibgen-1))
  (format t "  fibgen-1: ~A~%~%" (funcall fibgen-1))

  (format t "  fibgen-2: ~A~%" (funcall fibgen-2))
  (format t "  fibgen-2: ~A~%" (funcall fibgen-2))
  (format t "  fibgen-2: ~A~%~%" (funcall fibgen-2))

  (format t "  fibgen-1: ~A~%" (funcall fibgen-1))
  (format t "  fibgen-1: ~A~%" (funcall fibgen-1))
  (format t "  fibgen-1: ~A~%~%" (funcall fibgen-1))

  (format t "  fibgen-2: ~A~%" (funcall fibgen-2))
  (format t "  fibgen-2: ~A~%" (funcall fibgen-2))
  (format t "  fibgen-2: ~A~%~%" (funcall fibgen-2))

  (format t "  fibgen-1: ~A~%" (funcall fibgen-1))
  (format t "  fibgen-1: ~A~%" (funcall fibgen-1))
  (format t "  fibgen-1: ~A~%~%" (funcall fibgen-1))

  (format t "  fibgen-2: ~A~%" (funcall fibgen-2))
  (format t "  fibgen-2: ~A~%" (funcall fibgen-2))
  (format t "  fibgen-2: ~A~%~%" (funcall fibgen-2))
)


(format t "5. Natural numbers generator~%~%")

(let
  (
    (ngen-1 (N-Generator))
    (ngen-2 (N-Generator))
  )

  (format t "  ngen-1: ~A~%" (funcall ngen-1))
  (format t "  ngen-1: ~A~%" (funcall ngen-1))
  (format t "  ngen-1: ~A~%" (funcall ngen-1))
  (format t "  ngen-1: ~A~%~%" (funcall ngen-1))

  (format t "  ngen-2: ~A~%" (funcall ngen-2))
  (format t "  ngen-2: ~A~%" (funcall ngen-2))
  (format t "  ngen-2: ~A~%" (funcall ngen-2))
  (format t "  ngen-2: ~A~%~%" (funcall ngen-2))

  (format t "  ngen-1: ~A~%" (funcall ngen-1))
  (format t "  ngen-1: ~A~%" (funcall ngen-1))
  (format t "  ngen-1: ~A~%" (funcall ngen-1))
  (format t "  ngen-1: ~A~%~%" (funcall ngen-1))

  (format t "  ngen-2: ~A~%" (funcall ngen-2))
  (format t "  ngen-2: ~A~%" (funcall ngen-2))
  (format t "  ngen-2: ~A~%" (funcall ngen-2))
  (format t "  ngen-2: ~A~%~%" (funcall ngen-2))
)
