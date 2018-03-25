;2. Определите функцию, возвращающую последний элемент списка.
(defun last-elem (L)
    (if (null (cdr L))
        (car L)
        (last-elem (cdr L))
    )
)
;7. Определите функцию, удаляющую из исходного списка элементы с четными номерами.
(defun del-even (L)
    (if (null (cadr L))
        NIL
        (cons (cadr L) (del-even (cddr L)))
    )
)

;16. Определите функцию, добавляющую элементы одного списка во второй список, начиная с заданной позиции.
(defun insert-list(L1 L2 N)
    (cond ((and (= N 0) (null L2)) L1)
          ((= N 0) (cons (car L2) (insert-list L1 (cdr L2) N)))
          (t (cons (car L1) (insert-list (cdr L1) L2 (- N 1))))
    )
)

;20. Определите функцию ПЕРВЫЙ-АТОМ, результатом которой будет первый атом списка
(defun first-atom(L1)
    (if (atom (car L1)) (car L1)
        (first-atom (car L1)))
)

;21. Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне
(defun del(L to-del)
    (if (equal (car L) to-del)
        (cdr L)
        (cons (car L) (del (cdr L) to-del)))
)

;25. Определите функцию, удаляющую из списка каждый четный элемент.
(defun del-evens(L)
    (cond ((null L) NIL)
        ((atom (car L))
          (if (numberp (car L))
              (if (= 0 (mod (car L) 2))
                  (del-evens (cdr L))
                  (cons (car L) (del-evens (cdr L))))
              (cons (car L) (del-evens (cdr L)))
          ))
          (T (cons (del-evens (car L)) (del-evens (cdr L))))
    )
)

;42. Определите функцию, находящую максимальное из значений, находящихся в вершинах дерева.
(defun max-sheet (L)
    (if (and (null (cadr L)) (null (caddr L)))
        (car L)
        (max (max-sheet (cadr L)) (max-sheet (caddr L)))
    )
)
;(print (max-sheet `(3 (4 nil nil) (5 (3 nil nil) (2 nil nil)) ) ))

(setf (get `paris `x) 1)
(setf (get `paris `y) 1)
(setf (get `amsterdam `x) 5)
(setf (get `amsterdam `y) 3)
(setf (get `stambul `x) 1)
(setf (get `stambul `y) 2)

(defun distance (A B)
    (sqrt (+
    (* (- (get A `x) (get B `x)) (- (get A `x) (get B `x)))
    (* (- (get A `y) (get B `y)) (- (get A `y) (get B `y)))
    ))
)

;(print (distance `paris `stambul))
