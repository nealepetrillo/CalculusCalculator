Calculus Calculator
==================

The CalculusCalculator allows a user to enter a seris of calulus expressions and produces the result.

It is most useful for expanding derivative series.

Syntax
------------------
All expressions are written in Polish notation. The 'der' keyword represents derivateive. 

To perform simple arithmatic operations input: 

(+ 1 2)

The result will be: 

3

To perform compound arithmatic enter:

(+ 1 (/ 4 2))

The result will be: 

3

To derive the expression (x + 1) with respect to x, input: 

(der x (+ x 1))

The output will be: 

1

To derive the expression (cos(x + 1)) with repsect to x, input:

(der x (cos (+ x 1)))

The result will be: 

-sin (x +1)


Available Expressions
------------------

Variable: Any scheme-recognized alphanumeric symbol

Integer: An integer literal

Real Num: The following real number literals
      * e
      * pi

Arithmatic Operator: any of the following operators
      * +
      * -
      * *
      * /

Real Functions: Any of the following functions as applied to the real numbers
      * cos
      * sin
      * tan
      * ln

Algebraic Expressions: Any of the following formats
      * <Real Number>
      * <Variable>
      * (<Aritmatic Operator> <Algebraic Expression> <Algebraic Expression>)
      * (^ <Algebraic Expression> <Real Number>)
      * (<Real Function> <Algebraic Expression>)

