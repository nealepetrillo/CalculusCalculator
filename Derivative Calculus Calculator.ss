;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "reader.ss" "plai" "lang")

;var ::= any scheme-recognized alphanumeric symbol

;int ::= an integer literal

;DerivExpr ::= (der <var> <AlgExpr>)
;            | (der <int> <var> <AlgExpr>)
(define-type DerivExpr
  (der (var symbol?) (expr AlgExpr?))
  (derMult (num integer?) (var symbol?) (expr AlgExpr?))
  )

;parseDerivExpr :: s-expression -> DerivExpr
;parseDerivExpr takes an s-expression representing the concrete syntax of a derivative expression and parses it into the abstract DerivExpr type
;Example: Input = (der x (+ x 1)) -> Output = (der x (binaryOp + #<procedure:+> (var x) (num 1)))
(define (parseDerivExpr sexp)
  (cond
    [(list? sexp) (cond 
                    [(symbol=? 'der (first sexp))
                     (cond [(symbol? (second sexp)) (der (second sexp)
                                                         (parseAlgExpr (third sexp)))]
                           [(integer? (second sexp)) (derMult (second sexp)
                                                          (third sexp)
                                                          (parseAlgExpr (fourth sexp)))]
                           [else (error 'parseDerivExpr
                                        "invalid derivative expression: first argument not a variable or integer")])]
                     [else (error 'parseDerivExpr "invalid derivative expression: does not beign with 'der'")])]
    [else (error 'parseDerivExpr "invalid derivative expression: not a list")])
  )
 
;RealNum ::= a real number literal
;          | e
;          | pi

;ArithOp ::= +
;          | -
;          | *
;          | /

;RealFun ::= cos
;          | sin
;          | tan
;          | ln

;AlgExpr ::= <RealNum>
;          | <var>
;          | (<ArithOp> <AlgExpr> <AlgExpr>)
;          | (^ <AlgExpr> <RealNum>)
;          | (<fun> <AlgExpr>)
(define-type AlgExpr
  (num (val number?))
  (var (name symbol?))
  (binaryOp (name symbol?) (op procedure?) (lhs AlgExpr?) (rhs AlgExpr?))
  (power (expr AlgExpr?) (p num?))
  (unaryOp (name symbol?) (op procedure?) (arg AlgExpr?))
  )

;parseAlgExpr :: s-expression -> AlgExpr
;parseAlgExpr takes an s-expression representing the concrete syntax of an algebraic expression and parses it into the abstract AlgExpr type
;Example: Input = (+ x 1) -> Output = (binaryOp + #<procedure:+> (var x) (num 1))
(define (parseAlgExpr sexp)
  (cond
    [(symbol? sexp) 
     (cond
       [(symbol=? 'pi sexp) (num pi)]
       [(symbol=? 'e sexp) (num (exp 1))]
       [else (var sexp)])]
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (cond
       [(symbol? (first sexp))
        (cond 
          [(symbol=? '+ (first sexp)) (binaryOp '+ +
                                               (parseAlgExpr (second sexp))
                                               (parseAlgExpr (third sexp)))]
          [(symbol=? '- (first sexp)) (binaryOp '- -
                                               (parseAlgExpr (second sexp))
                                               (parseAlgExpr (third sexp)))]
          [(symbol=? '* (first sexp)) (binaryOp '* *
                                               (parseAlgExpr (second sexp))
                                               (parseAlgExpr (third sexp)))]
          [(symbol=? '/ (first sexp)) (binaryOp '/ /
                                               (parseAlgExpr (second sexp))
                                               (parseAlgExpr (third sexp)))]
          [(symbol=? '^ (first sexp)) (power (parseAlgExpr (second sexp))
                                             (parseAlgExpr (third sexp)))]
          [(symbol=? 'sin (first sexp)) (unaryOp 'sin sin
                                                  (parseAlgExpr (second sexp)))]
          [(symbol=? 'cos (first sexp)) (unaryOp 'cos cos
                                                  (parseAlgExpr (second sexp)))]
          [(symbol=? 'tan (first sexp)) (unaryOp 'tan tan
                                                  (parseAlgExpr (second sexp)))]
          [(symbol=? 'ln (first sexp)) (unaryOp 'ln log
                                                 (parseAlgExpr (second sexp)))]
          [else (error 'parseAlgExpr "invalid algebraic expression: list starting with an unrecognized symbol")])]
       [else (error 'parseAlgExpr "invalid algebraic expression: not a number, variable or list")])])
  )

;parse :: s-expression -> DerivExpr OR AlgExpr
;parse takes an s-expression, determines if it's an algebraic expression or a derivative expression, and passes it to parseAlgExpr or parseDerivExpr accordingly
(define (parse sexp)
  (if (and (list? sexp) (symbol=? (first sexp) 'der))
      (parseDerivExpr sexp)
      (parseAlgExpr sexp)))

;displayExpr :: DerivExpr OR AlgExpr -> string
;displayExpr takes an abstract expression (either derivative or algebraic) and parses it into a human-readable string
;Example: Input = (binaryOp + #<procedure:+> (var x) (num 1)) -> Output = "(x + 1)"
(define (displayExpr expr)
  (cond
    [(DerivExpr? expr) (type-case DerivExpr expr
                         [der (var algExpr) (string-append "d ( " 
                                                           (displayExpr algExpr)
                                                           ") /d"
                                                           (symbol->string var))]
                         [derMult (num var algExpr) (string-append "d^"
                                                                   (number->string num)
                                                                   " ( "
                                                                   (displayExpr algExpr)
                                                                   "/d"
                                                                   (symbol->string var))])]
    [(AlgExpr? expr) (type-case AlgExpr expr
                       [num (n) (number->string n)]
                       [var (v) (symbol->string v)]
                       [binaryOp (name op l r)
                                 (case name
                                   [(*)
                                    (cond
                                      [(equal? l (num -1)) (string-append "-" (displayExpr r))]
                                      [(and (num? l) (exact? (num-val l)))
                                       (string-append (displayExpr l) (displayExpr r))]
                                      [else (string-append "(" (displayExpr l) " " 
                                                            (symbol->string name) " "
                                                            (displayExpr r) ")"
                                                            )])]
                                   [else (string-append "(" (displayExpr l) " "
                                                            (symbol->string name) " "
                                                            (displayExpr r) ")"
                                                            )])]
                       [power (arg p) (cond
                                            [(unaryOp? arg)
                                             (string-append (symbol->string (unaryOp-name arg)) "^" 
                                                            (displayExpr p)
                                                            "(" (displayExpr (unaryOp-arg)) ")"
                                                            )]
                                            [else (string-append (displayExpr arg) "^"
                                                                 (displayExpr p)
                                                                 )])]
                       [unaryOp (name op arg) (string-append (symbol->string name)
                                                             "("
                                                             (displayExpr arg)
                                                             ")"
                                                             )])])
  )

;calcExpr :: DerivExpr OR AlgExpr -> AlgExpr
;calcExpr takes an abstract expression and if it's a derivative expression passes it to calcDeriv, otherwise it returns the expression unchanged
(define (calcExpr expr)
  (if (DerivExpr? expr) (calcDeriv expr) expr)
  )

;calcDeriv :: DerivExpr -> AlgExpr
;calcDeriv takes an abstract derivative expression and returns the calculated derivative (as an abstract algebraic expression)
;Example: Input = (der x (binaryOp + #<procedure:+> (var x) (num 1))) -> Output = (num 1)
(define (calcDeriv dExpr)
  (type-case DerivExpr dExpr
    [derMult (n var expr) (case n
                            [(0) expr]
                            [(1) (calcDeriv (der var expr))]
                            [else (calcDeriv (derMult (- n 1) var (calcDeriv (der var expr))))])]
    [der (dVar expr) (type-case AlgExpr expr
                      [num (n) (num 0)]
                      [var (v) (if (symbol=? v dVar) (num 1) (num 0))]
                      [binaryOp (name op l r) (case name
                                                [(+ -) (binaryOp name op (calcDeriv (der dVar l)) (calcDeriv (der dVar r)))]
                                                [(*) (binaryOp '+ +
                                                                (binaryOp '* * l (calcDeriv (der dVar r)))
                                                                (binaryOp '* * r (calcDeriv (der dVar l))))]
                                                [(/) (binaryOp '/ /
                                                                (binaryOp '- -
                                                                          (binaryOp '* *
                                                                                    (calcDeriv (der dVar l))
                                                                                    r)
                                                                          (binaryOp '* *
                                                                                    l
                                                                                    (calcDeriv (der dVar r))))
                                                                (power r (num 2)))])]
                       [power (arg p) (binaryOp '* *
                                                p
                                                (binaryOp '* *
                                                          (power arg (num (- (num-val p) 1)))
                                                          (calcDeriv (der dVar arg))))]
                       [unaryOp (name op arg) (case name
                                                [(sin) (binaryOp '* *
                                                                  (unaryOp 'cos cos arg)
                                                                  (calcDeriv (der dVar arg)))]
                                                [(cos) (binaryOp '* *
                                                                  (binaryOp '* *
                                                                            (num -1)
                                                                            (unaryOp 'sin sin arg))
                                                                  (calcDeriv (der dVar arg)))]
                                                [(tan) (calcDeriv (der dVar (binaryOp '/ /
                                                                                       (unaryOp 'sin sin arg)
                                                                                       (unaryOp 'cos cos arg))))]
                                                [(ln) (binaryOp '/ /
                                                                 (calcDeriv dVar arg)
                                                                 (var dVar))])])])
  )

;reduceExpr :: AlgExpr -> AlgExpr
;reduceExpr takes an abstract algebraic expression and performs some simple reductions on it, returning a new (mathematically equivalent) algebraic expression
;Example: Input = (binaryOp + #<procedure:+> (num 1) (num 2)) -> Output = (num 3)
(define (reduceExpr expr)
  (type-case AlgExpr expr
    [num (n) expr]
    [var (v) expr]
    [binaryOp (name op l r) (local [(define nextL (reduceExpr l))
                                    (define nextR (reduceExpr r))
                                    (define changed? (or (not (equal? l nextL))
                                                         (not (equal? r nextR))))
                                    (define newBinOp (if changed? (reduceExpr (binaryOp name op nextL nextR)) expr))]
                              (cond
                                [(and (num? l) (num? r)) (num (op (num-val l) (num-val r)))]
                                [(and (or (symbol=? name '+) (symbol=? name '*)) (num? r)) 
                                 (reduceExpr (binaryOp name op r l))]
                                [(symbol=? '+ name) (cond [(equal? l (num 0)) nextR]
                                                          [(equal? r (num 0)) nextL]
                                                          [else newBinOp])]
                                [(symbol=? '- name) (cond [(equal? l (num 0)) (negateExpr nextR)]
                                                          [(equal? r (num 0)) nextL]
                                                          [else newBinOp])]
                                [(symbol=? '* name) (cond [(equal? l (num 1)) nextR]
                                                          [(equal? r (num 1)) nextL]
                                                          [(or (equal? l (num 0)) (equal? r (num 0))) (num 0)]
                                                          [(num? l)
                                                           (if (binaryOp? nextR) 
                                                               (local
                                                                 [(define r-name (binaryOp-name nextR))
                                                                  (define r-op (binaryOp-op nextR))
                                                                  (define r-rhs (binaryOp-rhs nextR))
                                                                  (define r-lhs (binaryOp-lhs nextR))]
                                                                 (case r-name
                                                                   [(+ -) (reduceExpr
                                                                           (binaryOp r-name r-op
                                                                                     (binaryOp '* * l r-lhs)
                                                                                     (binaryOp '* * l r-rhs)))]
                                                                   [(* /) (reduceExpr
                                                                          (binaryOp r-name r-op
                                                                                    (binaryOp '* * l r-lhs)
                                                                                    r-rhs))]))
                                                                newBinOp)]
                                                          [else newBinOp])]
                                [(symbol=? '/ name) (cond [(equal? r (num 1)) nextL]
                                                          [(equal? l (num 0)) (num 0)]
                                                          [else newBinOp])]
                                [else newBinOp]))]
    [power (arg p) (if (num? arg)
                       (num (expt (num-val arg) (num-val p)))
                       (case (num-val p)
                         [(0) (num 1)]
                         [(1) (reduceExpr arg)]
                         [else (power (reduceExpr arg) p)]))]
    [unaryOp (name op arg) (unaryOp name op (reduceExpr arg))])
  )

;interpret :: s-expression -> string
;interpret takes an s-expression and parses it, calculates it, reduces it, and then displays it (using the corresponding procedures defined above)
;Example: Input = (der x (+ x 1)) -> Output = "1"
(define (interpret sexp)
  (displayExpr (reduceExpr (calcExpr (parse sexp))))
  )

;negateExpr :: AlgExpr -> AlgExpr
;negateExpr takes an algebraic expression p and returns an algebraic expression q that satifies (p = -1 * q) mathematically.
;Example: Input = (var x) -> Ouput = (binaryOp * #<procedure:*> (num -1) (var x))
(define (negateExpr expr)
  (type-case AlgExpr expr
    [num (n) (num (* -1 n))]
    [var (v) (binaryOp '* * (num -1) v)]
    [binaryOp (name op l r) (case name
                              [(+) (binaryOp name op (negateExpr l) (negateExpr r))]
                              [(-) (binaryOp '+ + (negateExpr l) r)]
                              [(* /) (binaryOp name op (negateExpr l) r)]
                              [else (error "negateExpr not implemented for this binary operator")])]
    [power (arg p) (power (negateExpr arg) p)]
    [unaryOp (name op arg) (binaryOp '* * (num -1) expr)])
  )
