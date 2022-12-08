#lang pl
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#|
Task 2:
We used the instructions in foldl along with map to define a sum of squares function that takes a list of numbers as input,
and produces a number that is the sum of the squares of all the numbers in the list.

Input:The function receives a list of elements, checks if all elements are numbers or null
Output :sum of the squares of all the numbers in the list
Problems we encountered: closing parentheses, due to the fact that only in the first assignment I experimented with this programming language
In addition, we learned how map and lambda work in racket.
Time: 40 minutes
|#
(: sum-of-squares : (Listof Number) -> Number);; The function receives a list of elements and checks if all elements are numbers or all elements are null
(define (sum-of-squares lst) 
  (foldl + 0 (map (lambda ([num : Number])(* num num )) lst))) ;;All elements that are numbers different from null are squared and added together

(test (sum-of-squares '()) => 0) ;;empty
(test (sum-of-squares '(3)) => 9)
(test (sum-of-squares null) => 0) ;;empty
(test (sum-of-squares '(0)) => 0)
(test (sum-of-squares '(6)) => 36)
(test (sum-of-squares '(0.5)) => 0.25) ;;fraction
(test (sum-of-squares '(1/2)) => 1/4) ;;fraction
(test (sum-of-squares '(3.0)) => 9.0)
(test (sum-of-squares '(6.0)) => 36.0)
(test (sum-of-squares '(1 2 3)) => 14) 
(test (sum-of-squares '(-9)) => 81) ;;negative
(test (sum-of-squares '(1/2 1/2 1/2)) => 3/4) ;;fraction
(test (sum-of-squares '(0.5 0.5 0.5)) => 0.75) ;;fraction
(test (sum-of-squares '(-10000)) => 100000000) ;;negative
(test (sum-of-squares '(0 0 0 0 0 0 0 0)) => 0)
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------#|
#|
Task 3.1:
We were asked to create a function called createPolynomial that takes as arguments a list of 𝑘 numbers 𝑎0, … , 𝑎𝑘−1 and returns as a function output.
The returned function takes a number 𝑥0 and returns the value of the polynomial 𝑎0 ⋅ 𝑥0 + ⋯ + 𝑎𝑘−1 ⋅ 𝑥𝑛−1 in 𝑥0.
We followed the instructions in the built-in pl expt function to take two numbers 𝑎 and 𝑏, and return 𝑎𝑏.

Input: list of k numbers
Output: polynomial a0 ⋅ x0 + ⋯ + ak-1 ⋅ xn-1 in x0.
Problems we encountered :We had difficulty understanding in what order to put the variables, how to arrange everything in the template and how to calculate the polynomial rules.
Time: 70 minutes
|#

(: createPolynomial : (Listof Number) -> (Number -> Number)) 
(define (createPolynomial coeffs) 
  (: poly : (Listof Number) Number Integer Number -> Number) 
    (define (poly argsL x power accum) 
      (if (null? argsL)accum
          (poly (rest argsL) x (+ power 1) (+  accum (* (expt x power) (first argsL))))))
  (: polyX : Number -> Number) 
  (define (polyX x) 
   (poly coeffs x 0 0)) polyX)


(define p2345 (createPolynomial '(2 3 4 5))) 
(test (p2345 0) =>  (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3))))
 ;;negative
(define p2-345 (createPolynomial '(2 -3 4 5))) 
(test (p2-345 0) =>  (+ (* 2 (expt 0 0)) (* -3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3))))
(define p-2-3-4-5 (createPolynomial '(-2 -3 -4 -5))) 
(test (p-2-3-4-5 0) =>  (+ (* -2 (expt 0 0)) (* -3 (expt 0 1)) (* -4 (expt 0 2)) (* -5 (expt 0 3))))
 ;;fraction
(define p0.5345 (createPolynomial '(0.5 3 4 5))) 
(test (p0.5345 0) =>  (+ (* 0.5 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3))))
(define p0.50.30.40.5 (createPolynomial '(0.5 0.3 0.4 0.5))) 
(test (p0.50.30.40.5 0) =>  (+ (* 0.5 (expt 0 0)) (* 0.3 (expt 0 1)) (* 0.4 (expt 0 2)) (* 0.5 (expt 0 3))))

(test (p2345 4) => (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3)))) 
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3)))) 
(test (p2345 100) => (+ (* 2 (expt 100 0)) (* 3 (expt 100 1)) (* 4 (expt 100 2)) (* 5 (expt 100 3))))  
(define p536 (createPolynomial '(5 3 6))) 
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2))))
(test (p536 100) => (+ (* 5 (expt 100 0)) (* 3 (expt 100 1)) (* 6 (expt 100 2))))

(define p_0 (createPolynomial '())) 
(test (p_0 4) => 0)
(test (p_0 0) => 0) 
(test (p_0 -1) => 0) 
(test (p_0 0.5) => 0)

;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------#|
#|
Task 3.2.i:
The PLANG language definition that supports evaluating a polynomial over a sequence of points (numbers).
We based the solution on the interpreter we wrote for the AE language.
Time: 10 minutes


The grammar:
  <PLANG> ::=  {{Poly <AEs>} {<AEs}};;;;;;;;;;;check;;;;;;;;;;;;

 <AEs>   ::=  <AE>
             | <AE> <AEs>
  
  Taken from the lecture:

  <AE>    ::=  <Num>
             | { + <AE> <AE> }
             | { - <AE> <AE> }
             | { * <AE> <AE> }
             | { / <AE> <AE> }


|#

;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------#|
#|
Task 3.2.ii:
parser for the new language
Problems we encountered: We had problems with the placement of the brackets.
Time: 60 minutes
|#

(define-type PLANG 
    [Poly (Listof AE) (Listof AE)]) 
 
  (define-type AE 
    [Num  Number] 
    [Add  AE AE] 
    [Sub  AE AE] 
    [Mul  AE AE] 
    [Div  AE AE]) 
 
  (: parse-sexpr : Sexpr -> AE) 
  ;; to convert s-expressions into AEs 
  (define (parse-sexpr sexpr) 
    (match sexpr 
      [(number: n) (Num n)] 
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))] 
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))] 
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))] 
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))] 
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) 
   
  (: parse : String -> PLANG) 
  ;; parses a string containing a PLANG expression   to a PLANG AST
  (define (parse str) 
    (let ([code (string->sexpr str)])
      (match code
        [(list (cons 'poly '()) (list rhs ...)) (error 'parse "at least one coefficient is required in ~s" code)] ;;A pattern where the left variable is empty and the right is not
        [(list (cons 'poly lhs) '()) (error 'parse "at least one point is required in ~s" code)] ;;A pattern where the right variable is empty and the left one is not
        [(list (cons 'poly lhs) (list rhs ...)) (Poly (map parse-sexpr lhs) (map parse-sexpr rhs))] ;;A pattern where the right variable and the left variable are not empty
        [else (error 'parse "bad syntax in ~s" code)]
       )))


(test (parse "{{poly 1 2 3} {1 2 3}}")=> (Poly (list (Num 1) (Num 2) (Num 3))(list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly 0 0} {0 0}}")=> (Poly (list (Num 0) (Num 0))(list (Num 0) (Num 0))))

;;errors
(test (parse "{{poly } {1 2} }") =error> "parse: at least one coefficient is required in ((poly) (1 2))") 
(test (parse "{{poly 1 2} {} }") =error> "parse: at least one point is required in ((poly 1 2) ())")
(test (parse "{{poly } {}}")=error> "parse: at least one coefficient is required in ((poly) ())")
(test (parse "{poly Gal Raz 100}") =error> "parse: bad syntax in (poly Gal Raz 100)")
(test (parse "{poly}") =error> "parse: bad syntax in (poly)")

 ;;negative
(test (parse "{{poly -1 -2 -3} {1 2 3}}")=> (Poly (list (Num -1) (Num -2) (Num -3))(list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly -1 -2 -3} {-1 -2 -3}}")=> (Poly (list (Num -1) (Num -2) (Num -3))(list (Num -1) (Num -2) (Num -3))))

 ;;fraction
(test (parse "{{poly 1/2 2 3} {1 2 3}}")=> (Poly (list (Num 1/2) (Num 2) (Num 3))(list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly 1/8 2 3} {1 2 3}}")=> (Poly (list (Num 1/8) (Num 2) (Num 3))(list (Num 1) (Num 2) (Num 3))))

;;arithmetic
(test (parse "{{poly {+ 1 1}} {1 2 3}}") => (Poly (list (Add (Num 1) (Num 1))) (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly {- 1 1}} {1 2 3}}") => (Poly (list (Sub (Num 1) (Num 1))) (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly {* 1 1}} {1 2 3}}") => (Poly (list (Mul (Num 1) (Num 1))) (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly {/ 1 1}} {1 2 3}}") => (Poly (list (Div (Num 1) (Num 1))) (list (Num 1) (Num 2) (Num 3))))

 ;;arithmetic-fraction
(test (parse "{{poly {+ 1/2 1}} {1 2 3}}") => (Poly (list (Add (Num 1/2) (Num 1))) (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly {- 1/2 1}} {1 2 3}}") => (Poly (list (Sub (Num 1/2) (Num 1))) (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly {* 1/2 1}} {1 2 3}}") => (Poly (list (Mul (Num 1/2) (Num 1))) (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly {/ 1/2 1}} {1 2 3}}") => (Poly (list (Div (Num 1/2) (Num 1))) (list (Num 1) (Num 2) (Num 3))))

 ;;arithmetic-negative
(test (parse "{{poly {+ -1 1}} {1 2 3}}") => (Poly (list (Add (Num -1) (Num 1))) (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly {- -1 1}} {1 2 3}}") => (Poly (list (Sub (Num -1) (Num 1))) (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly {* -1 1}} {1 2 3}}") => (Poly (list (Mul (Num -1) (Num 1))) (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly {/ -1 1}} {1 2 3}}") => (Poly (list (Div (Num -1) (Num 1))) (list (Num 1) (Num 2) (Num 3))))

;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------#|
#|
Task 3.2.iii:
In this task we were asked to write the evaluation process. In order to leave the AE evaluation unchanged, we wrap it in the eval-poly function (which will be the core of the
estimator).
Input:  PLANG
Output: List of Numbers
Problems we encountered : We had problems with the evaluation process, we had to repeat the lectures and exercises again to understand in depth. We had to think about how to go through the variables and then we remembered the "map" function that we used in previous sections and we used it.
Time: 3 hours
|#
;; evaluates AE expressions to numbers
(: eval : AE -> Number)

(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]))

#| The eval-poly function converts a PLANG to a list of numbers and evaluates them. In addition, the function uses the "eval" function
   and the createPolynomial function from question 3.1. |#
(: eval-poly : PLANG -> (Listof Number))
(define (eval-poly p-expr)
 (cases p-expr
   [(Poly lhs rhs) (map (createPolynomial (map eval lhs)) (map eval rhs))])) ;;We used the "map" function to iterate through all the items in the lists.

#| This function takes a string,
Converts the string to PLANG using a parse function and returns a Listof number generated using the eve-poly function
|#
(: run : String -> (Listof Number))
;; evaluate a FLANG program contained in a string
(define (run str)
(eval-poly (parse str)))
