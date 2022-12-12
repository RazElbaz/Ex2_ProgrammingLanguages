#lang pl 02
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#|
Task 1.a:
In class we saw the grammar of AE - a simple language for "arithmetic expressions".
We were asked to write a BNF for "SE": a similar simple language of "string expressions". Legal 'programs' (ie, SE language words) should follow the lines of pl-expressions for strings, with two exceptions:
 1. Only digits 0,...,9 are allowed as legal characters within strings;
 2. We will have two types of expressions that are not available in the pl language (see below expressions of type 'stringinsert' and 'number->string'). The valid operators that can be used in these expressions are string, string-length and string-append, as well as string-insert and number->string.
It is also legal to accept expressions of the form "<D>", where <D> represents a (finite) sequence of digits. Simple values in the language are characters (of digits) and natural numbers, so the following expressions are also valid expressions: a sequence of digits
(eg, 347226) and an expression of the form #\v, where v is a digit.

We used λ to denote the empty string.
Problems we encountered: We had difficulty understanding how to define the SE, we had difficulty defining the STRING according to the examples.
Time: a whole day


 BNF for “SE”:
 
 <SE> ::=  <CHARS> | <NUM> | <STRING> 
 <NUM> ::= <DIGIT> | <DIGIT> <NUM> | {string-length <STRING>} | λ     ;;Taken from the lecture
 <DIGIT> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9                    ;;Taken from the lecture
 <CHARS> ::= #\<DIGIT> | #\<DIGIT> <CHARS> | λ


 <STRING> ::=
         <NUM>
         | "<NUM>"                                ;; ( "12344") 
         | {string <CHARS>}                       ;; ( string #\1 #\2 #\4 ) 
         | {string-append <STRING>}               ;; (string-append "45")
         | {string-append <STRING> <STRING>}      ;; (string-append ( string #\1 #\2 #\4 ) "12" ) 
         | {string-append <STRING> <CHARS>}       ;; (string-append "45"  #\4 #\5)
         | {string-insert <STRING> <CHARS> <NUM>} ;; ( string-insert "1357" #\4 66 ) 
         | {number->string <STRING>}              ;; ( number->string ( string-length "0033344" ) ) 
         
         
           

|#
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#|
Task 1.b:
We were asked to present a derivation process for 3 different SE expressions, so that each operator (eg, string-append, string-length and
number->string) appears in at least one of these expressions.

We used λ to denote the empty string.
Problems we encountered: We had difficulty understanding how to define the SE, we had difficulty defining the STRING according to the examples.
Time: 4 hours

1)(string #\1 #\2 #\3)


                                                                      
<SE> =>
        <STRING> =>
                   => {string <CHARS>}
                   => {string  #\<DIGIT> <CHARS> }
                   => {string  #\1 #\<DIGIT> <CHARS> }
                   => {string  #\1 #\2 #\<DIGIT>}
                   => {string  #\1 #\2 #\3}

2)( string-insert (string #\1 #\3 #\5 #\7) #\4 66 )
{string <CHARS>}
<SE> =>
        <STRING> =>
                   =>{string-insert <STRING> <CHARS> <NUM>}
                   =>{string-insert {string <CHARS>}  #\<DIGIT>  <DIGIT><NUM>}
                   =>{string-insert {string  #\<DIGIT> <CHARS>}  #\4  6<DIGIT>}
                   =>{string-insert {string  #\1 #\<DIGIT> <CHARS>}  #\4  66}
                   =>{string-insert {string  #\1 #\3 #\<DIGIT> <CHARS>}  #\4  66}
                   =>{string-insert {string  #\1 #\3 #\5  #\<DIGIT>}  #\4  66}
                   =>{string-insert {string  #\1 #\3 #\5  #\7}  #\4  66}

3) ( number->string ( string-length "00" ) )
<SE> =>
        <STRING> =>
                   => {number->string <NUM>}   
                   => {number->string {string-length <STRING>}}
                   => {number->string {string-length  "<NUM>"}}
                   => {number->string {string-length  " <DIGIT><NUM> "}}
                   => {number->string {string-length  "0<DIGIT>"}}
                   => {number->string {string-length  "00"}}


|#
;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#|
Task 2:
We used the instructions in foldl along with map to define a sum of squares function that takes a list of numbers as input,
and produces a number that is the sum of the squares of all the numbers in the list.

Input:The function receives a list of elements.
Output :sum of the squares of all the numbers in the list.
Problems we encountered:- closing parentheses
                        - we didn't know the "map" function in racket before, so we had to learn it and understand how it works

Time: 40 minutes
|#

;;A helper function that squares each number in the list
(: square : Number -> Number)
(define (square x)
  (* x x))

(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares list)
  (foldl + 0 (map square list))) ;;Using the foldl function according to the instructions and calling an auxiliary function and a scheme of all the numbers

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
  <PLANG> ::=  {poly <AEs>} {<AEs>}

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
(test (parse "{{poly {+ 1 2 3} {* 1 2}} {{- 1 2}}}") =error> "parse-sexpr: bad syntax in (+ 1 2 3)")
(test (parse "{poly}") =error> "parse: bad syntax in (poly)")
(test(parse "{{} {}}") =error> "parse: bad syntax in (() ())")
(test(parse "{{} }") =error> "parse: bad syntax in (())")
(test(parse "{{} {}}") =error> "parse: bad syntax")
(test(parse "{{} }") =error> "parse: bad syntax")

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
One of the ways to deal with the evaluation was to write down for ourselves the way to calculate in front of our eyes (we presented it in a note) and thus we were able to create many tests.
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

#| The eval-poly function converts a PLANG to a list of numbers and evaluates them.
   Additionally, the function uses the createPolynomial function from question 3.1 because we needed to create a polynomial. |#
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

#|
Explanation of the calculation:
{{poly x1 x2 x3}{y1 y2 y3}}=>'(x1*y1^0+x2*y1^1+x3*y1^2  x1*y2^0+x2*y2^1+x3*y2^2  x1*y3^0+x2*y3^1+x3*y3^2)
|#
(test (run "{{poly 1 2 3} {1 2 3}}")  => '(6 17 34))
(test (run "{{poly 4 2 7} {1 4 9}}")  => '(13 124 589)) 
(test (run "{{poly 1 2 3} {1 2 3}}")   => '(6 17 34)) 
(test (run "{{poly 4/5 } {1/2 2/3 3}}")  => '(4/5 4/5 4/5)) 
(test (run "{{poly 2 3} {4}}")  => '(14)) 
(test (run "{{poly 1 1 0} {-1 3 3}}")  => '(0 4 4))  
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") => '(14)) 
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}") => '(0 4 4))
(test (run "{{poly 10 10} {1}}") => '(20))
(test (run "{{poly 0} {1}}")=> '(0))

 ;;negative
(test (run "{{poly -1} {-1}}") => '(-1))
(test (run "{{poly -1 -1 -1 -1} {-1 -1 -1 -1}}") => '(0 0 0 0))
(test (run "{{poly -1  -1 -1} { -1 -1 -1}}") => '(-1 -1 -1))
(test (run "{{poly -11 -22 -33} {1 2 3}}") => '(-66 -187 -374))


 ;;fraction
(test (run "{{poly 1/2} {1 1 1}}") => '(1/2 1/2 1/2))
(test (run "{{poly 1} {1/2 1/2 1/2}}") => '(1 1 1))
(test (run "{{poly 1/2} {1/2 1/2 1/2}}") => '(1/2 1/2 1/2))
(test (run "{{poly 1 0} {-1 6 3}}")=> '(1 1 1))

;;big numbers
(test (run "{{poly 111 222 333} {1 2 3}}")=> '(666 1887 3774))
(test (run "{{poly 100 100 100} {1 2 3}}")=> '(300 700 1300))
(test (run "{{poly 10 10 10} {10 20 30}}")=> '(1110 4210 9310))
(test (run "{{poly 123 123 123} {1 2 3}}")=> '(369 861 1599))
(test (run "{{poly 100 100 100} {100 200 300}}")=> '(1010100 4020100 9030100))

 ;;arithmetic
(test (run "{{poly {+ 10 10} 10 {* 10 9}} {4 3 {/ 27 9}}}")=> '(1500 860 860))
(test (run "{{poly {+ 10 10} 10 {* 10 9}} {{+ 2 2} 3 {/ 27 9}}}")=> '(1500 860 860))
(test (run "{{poly 1/2 } {{+ 1/2 1/2} 1/2 {/ 1/2 1/2}}}")=> '(1/2 1/2 1/2))

;;errors
;;We added error tests because we wanted to check if the "run" function would return a "parse" function error like it should
(test (run "{{poly } {1 2} }") =error> "parse: at least one coefficient is required in ((poly) (1 2))") 
(test (run "{{poly 1 2} {} }") =error> "parse: at least one point is required in ((poly 1 2) ())")
(test (run "{{poly } {}}")=error> "parse: at least one coefficient is required in ((poly) ())")
(test (run "{poly Gal Raz 100}") =error> "parse: bad syntax in (poly Gal Raz 100)")

(test (run "{poly}") =error> "parse: bad syntax in (poly)")
(test (run "{poly}") =error> "parse: bad syntax")