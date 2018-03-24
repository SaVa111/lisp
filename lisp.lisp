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
