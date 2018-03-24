;Последний элемент списка
(defun last-elem (L)
    (if (null (cdr L))
        (car L)
        (last-elem (cdr L))
    )
)
;Удаление элементов с четными номерами
(defun del-even (L)
    (if (null (cadr L))
        NIL
        (cons (cadr L) (del-even (cddr L)))
    )
)

;Вставка второго списка в первый наиная с указанного элемента
(defun insert-list(L1 L2 N)
    (cond ((and (= N 0) (null L2)) L1)
          ((= N 0) (cons (car L2) (insert-list L1 (cdr L2) N)))
          (t (cons (car L1) (insert-list (cdr L1) L2 (- N 1))))
    )
)
