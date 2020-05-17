(ref: bool (list 3 4 2)) //Test error for referencing a bool
(set! (ref: num 7) (list)) //Test error for assigning a wrong type
(cdr (car (list: num 1 2 3))) //Test error for cdr on an elt
(list: num 1 2 3 4 5 6 7 8 9) //Test for correct list
(+ (list: num 1 2 3) #t) //Test for wrong types in Compound Arithmetic Exp
(= #t 1) //Test for wrong types in Binary Comparison
(if 6 #t #f) //Test for wrong cond
(let ((x: num 300) (y: num 40) (z: num 2) (e: bool 0)) (+ x y z)) //Test for wrong type in assignment within Let
(define pie: (num -> list) (lambda (rad: num) (list: num (* 2 rad) (* 2 rad 3.14) (* (* rad rad) 3.14)))) //For callExp Below
(pie #t) //Test for wrong param type
(pie 4) //Test for right param type