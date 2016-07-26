Assignment: Extended Interpreter
Name: Alan Moody

Instructions: 
* Open this file in DrRacket (or your favorite plain-text editor) and add your answers 
  at the end of each line for each question. If you want to add more explanation or 
  justification, you may add one or more lines under the question.  Remember to add 
  your name as well.  Once complete, submit this to Learning Suite as a plain text file.
* For each of the documentation questions, indicate Yes (Y) or No (N).
* For each of the test case questions, indicate the line number of the corresponding
  test (or tests) using "L" and the number of the line.  For example, a test on
  line 61 of the file would be "L61".  If you don't have a particular test, put "N".
* If you need to add any more explanation of justification, just add it on a line
  underneath the respective question.

Function: parse

 General:
 * Is the function correct? Y
 * Is the function documented correctly (i.e. contract and purpose statement)? Y

 Feature: literals
 * Is there an example of parsing a number expression properly? L235
 * Is there a test case for a literal that is not a number? L236

 Feature: binary operators
 * Is there an example of parsing a + expression properly? L238
 * Is there an example of parsing a - expression properly? L240
 * Is there an example of parsing a * expression properly? L242
 * Is there an example of parsing a / expression properly? L244
 * Is there a test case for: too few pieces? L246
 * Is there a test case for: too many pieces? L247

 Feature: id
 * Is there an example of parsing a id expression properly? L249
 * Is there a test case for: not an id (+)? L250
 * Is there a test case for: not an id (-)? L251
 * Is there a test case for: not an id (*)? L252
 * Is there a test case for: not an id (/)? L253
 * Is there a test case for: not an id (with)? L254
 * Is there a test case for: not an id (if0)? L255
 * Is there a test case for: not an id (fun)? L256

 Feature: if0
 * Is there an example of parsing a if0 expression properly? L258
 * Is there a test case for: too few pieces? L260
 * Is there a test case for: too many pieces? L261

 Feature: with
 * Is there an example of parsing a with expression properly? L263
 * Is there a test case for: too few pieces? L264
 * Is there a test case for: too many pieces? L265
 * Is there a test case for: invalid bindings list (not a list)? N
 * Is there a test case for: invalid binding (not a list)? N
 * Is there a test case for: invalid binding (too few pieces)? L266
 * Is there a test case for: invalid binding (too many pieces)? L267
 * Is there a test case for: invalid binding (not a symbol)? N
 * Is there a test case for: invalid binding (not a valid id)? N
 * Is there a test case for: invalid binding (duplicated id)? N

 Feature: fun
 * Is there an example of parsing a fun expression properly? L269
 * Is there a test case for: too few pieces? L270
 * Is there a test case for: too many pieces? L271
 * Is there a test case for: invalid parameters (not a list)? N
 * Is there a test case for: invalid parameter (not a symbol)? N
 * Is there a test case for: invalid parameter (not a valid id)? N
 * Is there a test case for: invalid parameter (duplicated id)? N

 Feature: app
 * Is there an example of parsing an app expression properly? L274

 Other:
 * Is there a test case for an expression with no operator (an empty list)? L276


Function: interp

 General:
 * Is the function correct? Y
 * Is the function documented correctly (i.e. contract and purpose statement)? Y
 
 Feature: literals
 * Is there a number case test? L280
 
 Feature: binary operators
 * Is there a + case test? L282
 * Is there a + (catch non-number, lhs) case test? N
 * Is there a + (catch non-number, rhs) case test? N
 * Is there a - case test? L284
 * Is there a - (catch non-number, lhs) case test? N
 * Is there a - (catch non-number, rhs) case test? N
 * Is there a * case test? L286
 * Is there a * (catch non-number, lhs) case test? N
 * Is there a * (catch non-number, rhs) case test? N
 * Is there a / case test? L288
 * Is there a / (catch non-number, lhs) case test? N
 * Is there a / (catch non-number, rhs) case test? N
 * Is there a / (catch div by 0) case test? L289
 
 Feature: id
 * Is there an id (unbound) case test? L291
 
 Feature: if0
 * Is there an if0 (true) case test? L293
 * Is there an if0 (false) case test? L294
 * Is there an if0 (catch non-number) case test? N
 
 Feature: with
 * Is there a with (basic, bound id) case test?
 * Is there a with (shadowing) case test?
 * Is there a with (shadowing in body but not in initialization expression) case test?
 
 Feature: fun
 * Is there a fun (evaluates to closure) case test?
 * Is there a fun (evaluates to closure with captured binding) case test?
 
 Feature: app
 * Is there a working app test case?
 * Is there an app (catches non-function) case test?
 * Is there an app (catches too few args) case test?
 * Is there an app (catches too many args) case test?
 * Is there an app (static, not dynamic scope) case test?

