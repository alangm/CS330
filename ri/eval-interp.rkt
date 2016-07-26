Assignment: Rudimentary Interpreter
Name: Alan Moody

Instructions: 
* Open this file in DrRacket (or your favorite plain-text editor) and add your answers 
  at the end of each line for each question. If you want to add more explanation or 
  justification, you may add one or more lines under the question.  Remember to add 
  your name as well.  Once complete, submit this to Learning Suite as a plain text file.
* For each of the documentation questions, indicate Yes (Y) or No (N).
* For each of the test case questions, indicate the line number of the corresponding
  test (or tests) usingthe number of the line.  For example, a test on line 61 of the
  file would be "61".  If you don't have a particular test, put "N".
* If you need to add any more explanation of justification, just add it on a line
  underneath the respective question.

Function: parse

 * Is the function correct? Y L48
 * Is the function documented correctly (i.e. contract and purpose statement)? Y

 Feature: literals
 * Is there an example of parsing a number expression properly? L82
     (eg, (parse '5))
 * Is there a test case for a literal that is not a number? (i.e., not a number, a list, or a symbol?) N
     (eg, (parse "hello"))

 Feature: binary operators
 * Is there an example of parsing a + expression properly? L82
 * Is there an example of parsing a - expression properly? L83
 * Is there an example of parsing a * expression properly? L84
 * Is there an example of parsing a / expression properly? L85
 * Is there a test case for: too few pieces? N
     (eg, (+ 5))
 * Is there a test case for: too many pieces? N
     (eg, (+ 1 2 3))

 Feature: with
 * Is there an example of parsing a with expression properly? N
 * Is there a test case for: too few pieces in the expression?  N
     (eg, [with ((x 5))] )
 * Is there a test case for: too many pieces in the expression?  N
     (eg, [with ((x 5)) (+ 1 x) (+ 2 x)] )
 * Is there a test case for: invalid bindings list (not a list)?  N
     (eg, [with x (+ 1 x) (+ 2 x)] )
 * Is there a test case for: invalid binding within the bindings (not a list)? N 
     (eg, [with (x 5) (+ 1 x) (+ 2 x)] )
 * Is there a test case for: invalid binding (too few pieces)?  N
     (eg, [with ((x)) (+ 1 x)] )
 * Is there a test case for: invalid binding (too many pieces)?  N
     (eg, [with ((x 5 6)) (+ 1 x)] )
 * Is there a test case for: invalid binding (first item not a symbol)?  N
     (eg, [with ((42 5)) (+ 1 x)] )

 Feature: id
 * Is there an example of parsing a id expression properly? N
 * Is there a test case for: not an id (+)? N
 * Is there a test case for: not an id (-)? N
 * Is there a test case for: not an id (*)? N
 * Is there a test case for: not an id (/)? N
 * Is there a test case for: not an id (with)? N
 
 Other:
 * Is there a test case for an expression with no operator (an empty list)?  N
     (eg, (parse '()) )

Function: calc
 * Is the function correct? Y
 * Is the function documented correctly (i.e. contract and purpose statement)? Y
 * Is there a number case test? N
 * Is there a + case test? N
 * Is there a - case test? N
 * Is there a * case test? N
 * Is there a / case test? N
 * Is there a divide by zero case test? N
 * Is there a case that shows referencing an identifier? N
 * Is there an id (unbound) case test? N
 * Is there a with (basic, bound id) case test? N 
     (eg, [with ((x 5)) (+ x 5)] )
 * Is there a with (shadowing) case test?  N
     (eg, [with ((x 5)) (with ((x 6)) (+ x 5) ]] )
 * Is there a with (shadowing in body but not in initialization expression) case test? N 
     (eg, [with ((x 5)) (with ((x (+ x 1))) (+ x 5) ]] )
 * Is there a duplicate bindings test case?  N
     (eg, [with ((x 5) (x 6)) (+ x x)] )
 