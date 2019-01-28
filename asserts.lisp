(load 'main.lisp)

;not working, should return true if it can be evaluated to true, else it show not evaluated param
(defmacro test(param)
    `(if ,(equal param T)
        T
        param
    )
)

(defun testall ()
    (and 
        (equal 'true (solve '(not false) T))
        (equal 'false (solve '(not true) T))
        (equal 'true (solve '(and true true) T))
        (equal 'false (solve '(and true false) T))
        (equal 'false (solve '(and false true) T))
        (equal 'false (solve '(and false false) T))
        (equal 'true (solve '(or true true) T))
        (equal 'true (solve '(or true false) T))
        (equal 'true (solve '(or false true) T))
        (equal 'false (solve '(or false false) T))
        (equal 'true (solve '(and (xor false true) (imply true (or false true))) T))
    )
)