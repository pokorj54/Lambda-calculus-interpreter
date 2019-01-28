; Valid notation of λ function is (λ param body) or (λ param (body))
; there can be only one parameter
; list with first element 'λ, second param and expresion or lists of expresions
; variables/parameteres can be any string, but it is prefered to be only one char
; some names are reserved for defined functions to make input easier
;-----------------------------------------------

;detects all variables and prints if they are free or bounded
(defun detectVariables (lst &optional bvars)
    (if (isLambda lst)
        (mapcar (lambda (x) (detectVariables x (append bvars (list (cadr lst))))) (cddr lst))
        (printVariables lst bvars)
    )
)

;prints if variable is free or bound
(defun printVariables(v bvars) 
    (cond
        ((listp v)
            (mapcar (lambda (x) (printVariables x bvars)) v))
        ((not (find v bvars))
            (progn (write v)(write-line " free")))
        (T 
            (progn (write v)(write-line " bound")))
    )
)

;returns list of free variables in λ expression
;some varaibles may be in result multipe times
(defun getFreeVars (lst &optional bvars)
    (cond
        ((null lst)
            nil)
        ((atom lst)
            (list lst))
        ((isLambda lst)
            (getFreeVars (cddr lst) (cons (cadr lst) bvars)))
        (T
            (append
                (getFreeVars (car lst) bvars)
                (getFreeVars (cdr lst) bvars)))
    )
)

;returns true if given list is λ function 
; - first element is λ and second element is not nil
(defun isLambda (lst)
    (and (listp lst) (equal (car lst) 'λ))
)

;solves λ expression and shows steps
;standartize doesn't have to be used all the time, but 
(defun solve (lst &optional silent)
    (let ((oneStep (standartize (aplicateFind lst))))
        (if (equal oneStep lst)
            (progn
                (if (not silent) 
                    (write-line (write-to-string lst))
                    nil)
                lst)
            (progn
                (if (not silent) 
                    (write-line (write-to-string lst)) ;better formating? - list show weardly
                    nil) 
                (solve oneStep silent))
        )
    )
)

;aplies first needed operation for λ expression - aplication or rename 
(defun aplicateFind (lst &optional bvars)
    (cond
        ((atom lst)
             lst)
        ((isLambda lst)
            (append 
                (list (car lst) (cadr lst)) 
                (aplicateFind (cddr lst) 
                    (cons (cadr lst) bvars))))
        ((and (isLambda (car lst)) (not (null (cdr lst))))
            (append 
                (analyze (car lst) (cadr lst) bvars)
                (cddr lst)))
        ((and (isExpandable (car lst)) (not (null (cdr lst))))
            (append 
                (list (expand (car lst)))
                (cdr lst)))
        ((null (cdr lst))
            (list (aplicateFind (car lst) bvars)))
        (T 
            (let ((evaluatedLeft (aplicateFind (car lst) bvars)))
                (if (equal evaluatedLeft (car lst))
                    (append 
                        (list(car lst)) 
                        (aplicateFind (cdr lst) bvars))
                    (append 
                        (list evaluatedLeft)
                        (cdr lst))
                )))
    )
)

;decides if is rename needed for some inner λ function or can be used aplication
;returns either renamed or aplicated λ expression
(defun analyze (lambda arg bvars)
    (let ((renamed (renameIfNeeded (cddr lambda) (cadr lambda) bvars (getFreeVars arg))))
        (if (equal renamed (cddr lambda))
            (replaceParam (cddr lambda) (cadr lambda) arg)
            (append
                (list (cons 'λ (cons (cadr lambda) renamed)))
                (list arg))
        )
    )
)

;decides if rename is needed before application
(defun renameIfNeeded (lst param bvars argfreevars)
    (cond
        ((or (null argfreevars) (atom lst))
            lst)
        ((and (isLambda lst) (contains argfreevars (cadr lst)) (containsVariable (cddr lst) param))
            (renameLambda lst (cadr lst) (getNewVar (cons param (append bvars argfreevars)))))
        ((and (isLambda lst) (containsVariable lst param))
            (cons 'λ (cons (cadr lst) (mapcar 
                (lambda (sublist) 
                    (renameIfNeeded sublist param (cons (cadr lst) bvars) argfreevars))
                    (cddr lst)))))
        ((containsVariable lst param)
            (mapcar 
                (lambda (sublist) 
                    (renameIfNeeded sublist param bvars argfreevars))
                    lst))
        (T 
            lst)
    )
)

;returns T if list contains value othervise nil
(defun contains (lst value)
    (eval (cons 'or (mapcar (lambda (x) (equal x value)) lst)))  
)

;renames parameter in λ function with new name
(defun renameLambda (lst oldname newname &optional (first T))
    (cond
        ((atom lst)
            (if (equal lst oldname)
                newname
                lst
            ))
        ((and (isLambda lst) (equal oldname (cadr lst)) (not first))
            lst)
        (T 
            (mapcar (lambda (sublist) (renameLambda sublist oldname newname nil)) lst))
    )
)

;checks if λ expression contains given variable
(defun containsVariable (lst var)
    (cond 
        ((and (isLambda lst) (equal var (cadr lst)))
            nil)
        ((listp lst)
            (eval (cons 
                'or 
                (mapcar (lambda (sublist) (containsVariable sublist var)) lst))))
        (T
            (equal lst var))
    )
)

;replaces all parameters in λ expression with given argument
;assumes there can't be two same parameters
(defun replaceParam (lst param arg)
    (if (atom lst)
        (if (equal lst param)
            arg
            lst
        ) 
        (append
            (list (replaceParam (car lst) param arg))
            (replaceParam (cdr lst) param arg))
    )
)

;checks if λ expression is in normal state
(defun isNormal (lst)
    (isNormalInner (standartize lst))
)

;logic for checking if λ expression is in normal state
(defun isNormalInner (lst)
    (cond
        ((atom lst)
            T)
        ((and (isExpandable (car lst)) (not (null (cdr lst))))
            nil)
        ((isLambda lst)
            (isNormal (cddr lst)))
        ((and (isLambda (car lst)) (not (null (cadr lst))))
            nil)
        ((and (listp lst) )
            (and (isNormal (car lst)) (isNormal (cdr lst))))
        (T
            (isNormal (cdr lst)))
    )
)

;gets rid of unnecessary parentheses of λ expression
(defun standartize (lst &optional(isFirst T))
    (cond
        ((atom lst)
             lst)
        ((isLambda lst)
            (append (list (car lst) (cadr lst)) (list (standartize (cddr lst) T))))
        ((and (null (cdr lst)) (list (car lst)) isFirst)
            (standartize (car lst) T))
        (T
            (append (list (standartize (car lst) T)) (standartize (cdr lst) nil)))
    )
)

;checks if value is one of the defined functions
(defun isExpandable (value)
    (cond
        ((null value)
            nil)
        ((numberp value)
            T)
        (T
            (equal 
                (find value (mapcar #'first (functions)))
                value))
    )
)

;expands defined value to it's representation as λ function
(defun expand (value)
    (if (numberp value)
        (getNumber value)
        (mymap value (functions))
    )
)

;list of defined function structured in pairs
;first: name of function, second: representation as λ function 
(defun functions ()
    '(
        (true (λ t (λ f t)))
        (false (λ t (λ f f)))
        (not (λ x x false true))
        (and (λ x (λ y x y false)))
        (or (λ x (λ y x true y)))
        (xor (λ x (λ y x (not y) y)))
        (imply (λ x (λ y (or (not x) y))))
        (zero (λ x (x (λ y false) true)))
        (YK (λ f (λ x f (x x)) (λ x f (x x))))
        (+ (λ x (λ y (λ s (λ z x s (y s z))))))
        (* (λ x (λ y (λ s (λ z x (y s) z)))))
        (exp (λ m (λ n (λ s (λ z (n m) s z)))))
        (pred (λ x (λ s (λ z x (λ f (λ g g (f s))) (λ h z) (λ m m)))))
        (- (λ m (λ n n pred m)))
        (eql (λ x (λ y (and (zero (- x y)) (zero (- y x))))))
        (fact (λ f (λ n (zero n 1 (* n (f (pred n)))))))
        (fib (λ f (λ n (zero n 1 (zero (pred n) 1 (+ (f (pred n)) (f (pred (pred n)))))))))
    )
)

;map: list of pairs(in list)
;searches through the pairs
;if it finds a pair of which first element is equal to value
;then it returns the second element
(defun mymap (value map)
    (cond
        ((null map)
            nil)
        ((equal (caar map) value)
            (cadar map))
        (T 
            (mymap value (cdr map)))
    )
)

;return representation of not negative integer as λ function
(defun getNumber(num &optional (current 0) (res 'z))
    (if (equal current num)
            `(λ s (λ z ,res))
            (getNumber num  (+ current 1) `(s ,res))
    )
)

(defun possibleVars ()
    '(a b c d e f g h i j k l m n o p q r s t u v w x y z)
)

;gets new variable from list of possible variables that are not taken
;not the right solution
(defun getNewVar(taken &optional (from (possibleVars)))
    (if (null (find (car from) taken))
        (car from)
        (getNewVar taken (cdr from))
    )
)

;if lst represents a number in lambda calculus then returns the number in digit representation
(defun lambdaToNumber(lst )
    (if (and (isLambda lst) (isLambda (caddr lst)))
        (lambdaToNumberInner (caddr (caddr lst)) (cadr lst) (cadr (caddr lst)) 0)
        nil
    )
)
(defun lambdaToNumberInner(lst delimCounter delimEnd counter)
    (cond 
        ((equal lst delimEnd)
            counter)
        ((and (list lst) (equal (car lst) delimCounter))
            (lambdaToNumberInner (cadr lst) delimCounter delimEnd (+ counter 1)))
        (T 
            nil)
    )
)

;compilation of every function so the stack won't overflow
(compile 'detectVariables)
(compile 'printVariables)
(compile 'getFreeVars)
(compile 'isLambda)
(compile 'solve)
(compile 'aplicateFind)
(compile 'analyze)
(compile 'renameIfNeeded)
(compile 'contains)
(compile 'renameLambda)
(compile 'containsVariable)
(compile 'replaceParam)
(compile 'isNormal)
(compile 'isNormalInner)
(compile 'standartize)
(compile 'isExpandable)
(compile 'expand)
(compile 'functions)
(compile 'getNumber)
(compile 'possibleVars)
(compile 'getNewVar)
