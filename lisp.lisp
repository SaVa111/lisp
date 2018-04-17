;2. Определите функцию, возвращающую последний элемент списка.
(defun last-elem (L)
    (if (null (cdr L))
        (car L)
        (last-elem (cdr L))
    )
)
;(print (last-elem `(1 2 3)))

;7. Определите функцию, удаляющую из исходного списка элементы с четными номерами.
(defun del-even (L)
    (if (null (cadr L))
        NIL
        (cons (cadr L) (del-even (cddr L)))
    )
)
;(print (del-even `(0 1 2 3 4 5)))

;16. Определите функцию, добавляющую элементы одного списка во второй список, начиная с заданной позиции.
(defun insert-list(L1 L2 N)
    (cond ((and (= N 0) (null L2)) L1)
          ((= N 0) (cons (car L2) (insert-list L1 (cdr L2) N)))
          (t (cons (car L1) (insert-list (cdr L1) L2 (- N 1))))
    )
)
;(print (insert-list `(1 2 3 7 8) `(4 5 6) 3))

;20. Определите функцию ПЕРВЫЙ-АТОМ, результатом которой будет первый атом списка
(defun first-atom(L1)
    ((lambda (E) (if (atom E) E
                 (first-atom E))
     ) (car L1))
)
;(print (first-atom `(((1 2) 3) 4 5)))

;21. Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне
(defun del(L to-del)
    (if (null L)
    nil
    ((lambda (Head Tail)
        (if (equal Head to-del)
            Tail
            (cons Head (del Tail to-del)))
            ) (car L) (cdr L)))
)
;(print (del `(1 2 (3 3) 3) 3))

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
;(print (del-evens `(1 2 3 4 (a b c (5 4)) (4 5))))

;42. Определите функцию, находящую максимальное из значений, находящихся в вершинах дерева.
(defun max-sheet (L)
    ((lambda (left right)
             (if (and (null left) (null right))
                 (car L)
                 (max (max-sheet left) (max-sheet right))
             )
     ) (cadr L) (caddr L))
)
;(print (max-sheet `(3 (4 nil nil) (5 (3 nil nil) (2 nil nil)) ) ))

(setf (get `paris `x) 1)
(setf (get `paris `y) 1)
(setf (get `paris `z) nil)
(setf (get `amsterdam `x) 5)
(setf (get `amsterdam `y) 3)
(setf (get `stambul `x) 1)
(setf (get `stambul `y) 2)

(defun distance (A B)
    ((lambda (x1 y1 x2 y2) (sqrt (+
    (* (- x1 x2) (- x1 x2))
    (* (- y1 y2) (- y1 y2))
    ))) (get A `x) (get A `y) (get B `x) (get B `y))
)
;(print (distance `paris `stambul))

;Вспомогательная функция определяющая входит ли элемент в список
(defun find-in-list(L E)
    (cond  ((null L) nil)
           ((equal (car L) E) T)
           (T (find-in-list (cdr L) E))
           )
)
;48. Напишите предикат (ИМЕЕТ-СВОЙСТВО символ свойство), который проверяет, обладает ли символ данным свойством.
(defun has-property(Elem Param)
    (find-in-list (del-even (cons 0 (symbol-plist Elem))) Param)
)
;(print (has-property `paris `x))
;(print (has-property `paris `z))
;(print (has-property `paris `r))

;Определите функицонал (MAPLIST fn список) для одного списочного аргумента.
(defun MAPLI (fn L)
    ((lambda (fn-l tail)(if (null tail) (list fn-l)
                      (cons fn-l (MAPLI fn tail))
                      )) (funcall fn L) (cdr L))
)
;(MAPLI `print `(1 2 3))

(defun MAPCA (fn L)
    ((lambda (fn-l tail)(if (null tail) (list fn-l)
                      (cons fn-l (MAPCA fn tail))
                      )) (funcall fn (car L)) (cdr L))
)
;(MAPCA `print `(1 2 3))

(defun All-t (L)
    (if (null (cdr L)) (car L)
        (and (car L) (All-t (cdr L)))
    )
)
;4. Определите функциональный предикат (КАЖДЫй пред список), который исти-
;   нен в том и только в том случае, когда, являющейся функциональным аргу-
;   ментом предикат пред истинен для всех элементов списка список.
(defun TEach (fn L)
    (All-t (MAPCA fn L))
)
;(print (TEach (lambda (A)(< A 5)) `(1 2 3 4)))

;6. Определите фильтр (УДАЛйЬ-ЕСЛИ пред список), удаляющий из списка список
;   все элементы, которые обладают свойством, наличие которого проверяет предикат пред.
(defun Del-If (fn L)
    (MAPCAN (lambda (X)
                   (if (funcall fn X) nil (list X))
           ) L)
)
;(print (Del-If (lambda (x)(< x 5)) `(1 5 1 5 2 2)))
;(print (Del-If (lambda (x)(= x 5)) `(1 5 5 5 5 2)))
;(print (Del-If (lambda (x)(= 0 (mod x 2))) `(1 2 3 5 4 1 5 2 2)))

;9. Напишите генератор порождения чисел Фибоначчи:
(defun fibgen ()
    (let ((a -1) (b 1))
        (lambda ()
            ((lambda (x y) x)
                (setq b (+ a b)) (setq a (- b a))
            )
        )
    )
)
;8. Напишите генератор натуральных чисел
(defun make-counter ()
    (let ( (x -1) )
        (lambda () (setq x (+ x 1)))
    )
)

(setq g1 (fibgen))
(print (funcall g1))
(print (funcall g1))
(print (funcall g1))
(print (funcall g1))
(print (funcall g1))
(print (funcall g1))
(print (funcall g1))

;10. Напишите генератор, порождающий последовательность (A), (B A), (A B A)df
(defun abagen ()
    (let ( (l nil) )
        (lambda ()
                (setq l 
                      (cond
                          ((equal `a (car l)) (cons `b l))
                          (t (cons `a l))
                      )
                )
        )
    )
)
(setq g1 (abagen))
(print (funcall g1))
(print (funcall g1))
(print (funcall g1))

;3. Определите лисповскую форму (IF условие p q) в виде макроса.
(defmacro if2 (condition p q)
    (list `if condition p q)
)

(defmacro if3 (condition p q)
    (list `cond (list condition p) (list t q))
)

(print (if3 (> 5 1) 1 2))
;4. Определите в виде макроса форму (FIF тест отр нуль полож).
(defmacro fif (state n z p)
    (list `cond (list (< state 0) n)
                (list (> state 0) p)
                (list t z)
    )
)

(print (fif -4 -1 0 1))


