# Credits

The following people helped me with this project, either offering suggestions for the code, or insight into the nature of the project, or review of my information.

- [Denver Ellis](https://github.com/DenverEllis) - Project idea, lisp advice, and insight into functional programming
- [River](https://github.com/r-tae) - Function refactors

## Function Refactors

The following function refactors were offered by River:

Original function

```lisp
(defun add-extension-if-not-present(filename ext)
  "Returns filename with ext at the end, if ext wasn't there already"
  (defparameter result-filename filename)
  (if (not (check-for-extension filename ext))
    (setf result-filename (concatenate 'string filename ext)))
  result-filename)
```

Refactored function

```lisp
(defun add-extension-if-not-present (filename ext)
  "Returns filename with ext at the end, if ext wasn't there already"
  (if (not (check-for-extension filename ext))
    (concatenate 'string filename ext)
    filename))
```

In lisp, everything is an "expression" (something that returns a value).
This includes things like *control structures* like if statements and loops.
At some point in testing, I had checked the value an if expression returned,
and it was `nil`.
However, only `if` called with 1 argument (i.e. single-branch if) returns `nil`;
`if` called with 2 arguments (if-else) returns the value 
the expression of the branch evaluates to.

Because of this property of expressions,
lisp functions are often just a single expression.
The builtin functions of lisp facilitate this well 
(take for example `mapcar`; which lets one make a function that returns a "mutated"
list in merely a single expression that returns said adjusted list).
Assigning to a variable to then return that variable, or even using `return-from`,
is usually akin to saying

```py
def doStuff(someParam):
    x = someFunc(someParam, 4)
    y = x # why the heck is this assignment here? Why not just make x called `y`?
    return y
```

---

Original function

```lisp
(defun generate-list-of-floats(len)
  "Generates list of floats from 0-1, inclusive, based on the size of the list"
    (defvar final-list '())
    (loop for i from 1 to len do
      (push (coerce (/ (- len i) len) 'float) final-list))
    final-list)
```

Refactored function

```lisp
(defun generate-list-of-floats (len)
  "Generates list of floats from 0-1, inclusive, based on the size of the list"
  (loop for i from 1 to len collect
    (coerce (/ (- len i) len) 'float)))
```

Similar to before, functions are best when they are a single expression.
Prior, I was doing a variable manipulation (ergo using a function with a side effects)
to add to an empty list before returning it.
While this would be a normal pattern in a object oriented or imperative language,
in a functional language it's better opt for solutions involving a single function
which calls other functions.

The `collect` syntax handily solves this problem by collecting the results of the loop
into a new list.
Furthermore, the loop + collect syntax permits parallel looping of lists.
While I have not modified the program to use this,
in future endeavours my code would be notably simplified by using this feature.

However, because `collect` collects things *in order*; appending,
and pushing to a list prepends to a list.
As such, I replaced `(- len i)` with just `i`,
so the list counted up instead of down.

```lisp
(defun generate-list-of-floats (len)
  "Generates list of floats from 0-1, inclusive, based on the size of the list"
  (loop for i from 0 to len collect
    (coerce (/ i len) 'float)))
```

