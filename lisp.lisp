;2. Определите функцию, возвращающую последни q элемент списка.
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
