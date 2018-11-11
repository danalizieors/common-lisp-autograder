;;;; Automated verification tool for the 5th week Common Lisp test


;;; Colored printing functionality
(setq NORMAL 0)
(setq RED 31)
(setq YELLOW 33)
(setq GREEN 32)

(defun display (color format &rest strings)
    (switch-to color)
    (apply #'format (append (list t format) strings))
    (switch-to NORMAL)
)

(defun switch-to (color)
    (when colored-print
        (format t "~A[~Dm" (string (int-char #x1B)) (if (symbolp color) (eval color) color))
    )
)


;;; Methods used for verifying the solutions
(setq PASS GREEN)
(setq WARN YELLOW)
(setq FAIL RED)

(defun verify-exam (exam)
    (let ((corrected (verify-problems (getf exam 'problems))))
        (append (calculate-solved-given-score-maximum-exam (second corrected))
                corrected
                exam)
    )
)

(defun calculate-solved-given-score-maximum-exam (problems)
    (append (calculate-sum  problems 'solved)
            (calculate-sum  problems 'given)
            (calculate-sum  problems 'score)
            (calculate-sum  problems 'maximum))
)

(defun calculate-sum (problems field)
    (list field (apply '+ (mapcar (lambda (problem) (getf problem field)) problems)))
)

(defun verify-problems (problems)
    (list 'problems (mapcar #'verify-problem problems))
)

(defun verify-problem (problem)
    (let ((corrected (verify-tests (getf problem 'tests))))
        (append (calculate-solved-given-score-problem (second corrected) (getf problem 'maximum))
                corrected
                problem)
    )
)

(defun calculate-solved-given-score-problem (tests maximum)
    (let ((solved (count-if #'test-passed? tests)))
        (list 'solved solved
              'given (length tests)
              'score (scale solved (length tests) maximum))
    )
)

(defun scale (solved given maximum)
    (* (/ solved given) maximum)
)

(defun test-passed? (test)
    (eq (getf test 'status) 'PASS)
)

(defun verify-tests (tests)
    (list 'tests (mapcar #'verify-test tests))
)

(defun verify-test (test)
    (append (verify-function (getf test 'function) (getf test 'parameters) (getf test 'result))
            test)
)

(defun verify-function (function parameters result)
    (let ((returned (eval function)))
        (list 'returned returned
              'status (verify-result returned result))
    )
)

(defun verify-result (returned result)
    (cond
        ((eq returned 'NOT-IMPLEMENTED) 'WARN)
        ((equal returned result) 'PASS)
        (T 'FAIL)
    )
)


;;; Methods used for displaying the results
(defun display-exam (exam)
    (display-intro (getf exam 'intro))
    (display-problems (getf exam 'problems))
    (display-outro (getf exam 'solved) (getf exam 'given) (getf exam 'score) (getf exam 'maximum))
)

(defun display-intro (intro)
    (display NORMAL "~%~%")
    (display-line) (display NORMAL "~%")
    (display NORMAL "                      --== 5th week Common Lisp test ==--") (display NORMAL "~%")
    (display-line) (display NORMAL "~%~%~%")
)

(defun display-problems (problems)
    (mapcar #'display-problem problems)
)

(defun display-outro (solved given score maximum)
    (display-line)
    (display NORMAL "~%")
    (display (grading score) "    Results: ")
    (display-score (grading score) solved given score maximum)
    (display NORMAL "~%")
    (display-line)
    (display NORMAL "~%~%")
)

(defun display-problem (problem)
    (display-problem-title (getf problem 'solved) (getf problem 'given) (getf problem 'score) (getf problem 'maximum) (getf problem 'title))
    (display NORMAL "~A~%~%" (getf problem 'description))
    (display-tests (getf problem 'tests))
    (display NORMAL "~%~%")
)

(defun display-problem-title (solved given score maximum title)
    (display-score (grading-problem score maximum) solved given score maximum)
    (display NORMAL "~A~%~%" title)
)

(defun display-tests (tests)
    (mapcar #'display-test tests)
)

(defun display-test (test)
    (display (getf test 'status) "    ~A  " (getf test 'status))
    (display NORMAL "~A~%" (getf test 'name))
    (display-function (getf test 'function) (getf test 'parameters) (getf test 'result) (getf test 'returned) (getf test 'status))
    (display NORMAL "~%")
)

(defun display-function (function parameters result returned status)
    (display NORMAL "          ~A" function)
    (unless (eq status 'WARN) 
        (display status " => ~A" returned)
        (unless (eq status 'PASS)
            (display NORMAL " | ")
            (display PASS "~A" result)
        )
    )
    (display NORMAL "~%")
)

(defun display-score (color solved given score maximum)
    (display color "[~D/~D]" solved given)
    (display NORMAL " | ")
    (display color "[~,2F%/~,2F%]    " score maximum)
)

(defun display-line ()
    (display NORMAL "--------------------------------------------------------------------------------")
)

(defun grading (score)
    (if (< score 50)  'FAIL 'PASS)
)

(defun grading-problem (solved given)
    (cond 
        ((eq solved 0) 'FAIL) 
        ((eq solved given) 'PASS) 
        (T 'WARN) 
    )
)


;;; Define the problems and the soultions
(setq exam '(
problems (

    (
        title "A fisherman's tale"
        description "Before the beginning of the end of time, there was a fisherman. Minding his own business, he was living peacefully near the lake. Suddenly, a huge shadow appeared, something was obscuring the Sun: a capitalist giant appeared and demanded more fish. The fisherman, having no choice, packed all his belongings and went to a world trip to satisfy the giant's needs: The Mighty's Quest of Fishing to Bring End to the Rule of the Colossal Beast."
        maximum 30
        tests (
            (name "It's already in your hand" function (my-hand 'fish) result fish)
            (name "Left pocket" function (my-left-pocket '(fish . phone)) result fish)
            (name "Right pocket" function (my-right-pocket '(keys . fish)) result fish)
            (name "Fishes are growing on binary trees" function (my-picking-fish-from-tree '(((monkey . (deer . owl)) . (sandwich . book)) . ((boots . (hammer . fish)) . (money . orange)))) result fish)
            (name "Pothole on Calea Turzii" function (my-from-pothole '(fish)) result fish)
            (name "The farthest corner of the supermarket" function (my-from-farthest-corner '(eggs salad potato robots milk soda toothpaste costumes chips mayonnaise fish)) result fish)
            (name "The deeps of the Black Sea" function (my-from-deep-sea '(humans waves ship (bubbles (rocks shark (submarine fish)) mine) (coral) sand)) result fish)
            (name "In the darkness of the Mariana Trench" function (my-from-deep-trench '((((((rock (((((unkwown . fish)) creature)))))))))) result fish)
        )
    )

    (
        title "Dictionaries from lists"
        description "Implement your own dictionary using lists. The dictionary will contain key-value pairs in an alternating manner. Everything on even indices will be considered as key, everything on odd indices will be considered as value (we will consider the idencies starting from 0). Example: '(key1 value1 key2 value2 key3 value3 key4 value4). At the end you will be able to add a new entry to the dictionary, retrieve all the keys and all the values of the dictionary, get a value based on a key and get a key based on a value, remove entries based on their key and remove the duplicated entries. None of these operations should mutate the list given as parameter, aliasing is permitted though (you should build a new list via recursion instead of using setf). A new entry will be always added at the beginning of the list. Entries with the same key may exist, removing with a given key should remove all of these entries. When removing duplicates from a dictionary only the most recently added key-value pairs will be kept, all other entries with matching keys will be deleted."
        maximum 30
        tests (
            (name "Add a key-value pair - empty dictionary" function (my-add 'a 1 ()) result (a 1))
            (name "Add a key-value pair - malformed dictionary" function (my-add 'a 1 '(b)) result (a 1 b))
            (name "Add a key-value pair - one entry" function (my-add 'a 1 '(b 2)) result (a 1 b 2))
            (name "Add a key-value pair - many entries" function (my-add 'a 1 '(b 2 c 3 c 4)) result (a 1 b 2 c 3 c 4))
            (name "Get all the keys - empty dictionary" function (my-get-all-keys ()) result ())
            (name "Get all the keys - malformed dictionary" function (my-get-all-keys '(a)) result ())
            (name "Get all the keys - one entry" function (my-get-all-keys '(a 1)) result (a))
            (name "Get all the keys - many entries" function (my-get-all-keys '(a 1 b 2 c 3 d 4)) result (a b c d))
            (name "Get all the keys - malformed end" function (my-get-all-keys '(a 1 b 2 c 3 d 4 e)) result (a b c d))
            (name "Get all the values - empty dictionary" function (my-get-all-values '()) result ())
            (name "Get all the values - malformed dictionary" function (my-get-all-values '(a)) result ())
            (name "Get all the values - one entry" function (my-get-all-values '(a 1)) result (1))
            (name "Get all the values - many entries" function (my-get-all-values '(a 1 b 2 c 3 d 4)) result (1 2 3 4))
            (name "Get all the values - malformed end" function (my-get-all-values '(a 1 b 2 c 3 d 4 e)) result (1 2 3 4))
            (name "Get a value with a given key - empty dictionary" function (my-get-value 'c ()) result nil)
            (name "Get a value with a given key - malformed dictionary" function (my-get-value 'c '(c)) result nil)
            (name "Get a value with a given key - one entry" function (my-get-value 'c '(c 3)) result 3)
            (name "Get a value with a given key - key not found" function (my-get-value 'c '(a 1 b 2 d 4)) result nil)
            (name "Get a value with a given key - key found" function (my-get-value 'c '(a 1 b 2 c 3 d 4)) result 3)
            (name "Get a value with a given key - key is given as value" function (my-get-value 'c '(a c b 2 c 3 d 4)) result 3)
            (name "Get a value with a given key - key not an atom" function (my-get-value '(c e) '(a 1 b 2 (c e) 3 d 4)) result 3)
            (name "Get a key with a given value - empty dictionary" function (my-get-key 3 ()) result nil)
            (name "Get a key with a given value - malformed dictionary" function (my-get-key 3 '(3)) result nil)
            (name "Get a key with a given value - one entry" function (my-get-key 3 '(c 3)) result c)
            (name "Get a key with a given value - value not found" function (my-get-key 3 '(a 1 b 2 d 4)) result nil)
            (name "Get a key with a given value - value found" function (my-get-key 3 '(a 1 b 2 c 3 d 4)) result c)
            (name "Get a key with a given value - value is given as key" function (my-get-key 3 '(a 1 3 2 c 3 d 4)) result c)
            (name "Get a key with a given value - value not an atom" function (my-get-key '(3 4) '(a 1 b 2 c (3 4) d 4)) result c)
            (name "Remove entries with a given key - empty dictionary" function (my-remove-with-key 'c ()) result ())
            (name "Remove entries with a given key - malformed dictionary" function (my-remove-with-key'c '(c)) result (c))
            (name "Remove entries with a given key - one entry" function (my-remove-with-key 'c '(c 3)) result ())
            (name "Remove entries with a given key - key not found" function (my-remove-with-key 'c '(a 1 b 2 d 4)) result (a 1 b 2 d 4))
            (name "Remove entries with a given key - key found" function (my-remove-with-key 'c '(a 1 b 2 c 3 d 4)) result (a 1 b 2 d 4))
            (name "Remove entries with a given key - duplicated keys found" function (my-remove-with-key 'c '(c 31 a 1 c 32 b 2 c 33 d 4 c 34)) result (a 1 b 2 d 4))
            (name "Remove entries with a given key - key is given as value" function (my-remove-with-key 'c '(a c b 2 c 3 d 4)) result (a c b 2 d 4))
            (name "Remove entries with a given key - key not an atom" function (my-remove-with-key '(c e) '(a 1 b 2 (c e) 3 d 4)) result (a 1 b 2 d 4))
            (name "Remove duplicated entries - empty dictionary" function (my-optimize '()) result ())
            (name "Remove duplicated entries - malformed dictionary" function (my-optimize '(a)) result ())
            (name "Remove duplicated entries - one entry" function (my-optimize '(a 1)) result (a 1))
            (name "Remove duplicated entries - optimized dictionary" function (my-optimize '(a 1 b 2 c 3 d 4)) result (a 1 b 2 c 3 d 4))
            (name "Remove duplicated entries - duplicated elements" function (my-optimize '(c 1 a 2 b 3 a 4 c 5 d 6 b 7 c 8 e 9)) result (c 1 a 2 b 3 d 6 e 9))
            (name "Remove duplicated entries - keys are put as values" function (my-optimize '(c d a 2 b a a 4 c b d 6 b 7 c a e 9)) result (c d a 2 b a d 6 e 9))
            (name "Remove duplicated entries - malformed end" function (my-optimize '(c 1 a 2 b c a 4 c 5 d 6 b 7 c 8 e 9 f)) result (c 1 a 2 b c d 6 e 9))
        )
    )

    (
        title "The flattest of the maps"
        description "Write a higher order function, namely map, which applies a given function to each element of a list. Implement a function, flatten, which transforms a nested list into a simple list by making the parentheses disappear. Combine the aforementioned functions into one, flatmap. Write a version of map, deepmap, which applies a function to each atom of a nested list."
        maximum 20
        tests (
            (name "Increment each element of the list by 10" function (my-increment-each-by10 '(1 2 3 4 5 6 7 8 9)) result (11 12 13 14 15 16 17 18 19))
            (name "Implement map: apply a function to each element of the list" function (my-map add3 '(1 2 3 4 5 6 7 8 9)) result (4 5 6 7 8 9 10 11 12))
            (name "Flatten a nested list: make the parentheses inside the list disappear" function (my-flatten '((1 2) ((3) 4) (5 ((6))) (() ((7))) ((()) ()) (8 9))) result (1 2 3 4 5 6 7 8 9))
            (name "Implement flatmap: map the flattened list" function (my-flatmap add3 '((1 2) ((3) 4) (5 ((6))) (() ((7))) ((()) ()) (8 9))) result (4 5 6 7 8 9 10 11 12))
            (name "Implement deepmap: apply a function to each atom of the nested list" function (my-deepmap add3 '((1 2) ((3) 4) (5 ((6))) (() ((7))) ((()) ()) (8 9))) result ((4 5) ((6) 7) (8 ((9))) (() ((10))) ((()) ()) (11 12)))
        )
    )

    (
        title "Wolfgang Amadeus Mozart"
        description "Compose functions! First, you will transform a value by applying a list of functions on it: starting from left, going to right, then returning the result in the end. Later you will compose functions, you will create a function that expects a list of functions as parameter and returns a lambda. This lambda, when called, will apply the list of functions on the given parameter."
        maximum 20
        tests (
            (name "Call two functions on a value" function (my-call-two 10 add3 mul2) result 26)
            (name "Call a list of functions on a value" function (my-call-list 10 (list add10 add3 mul2 add10)) result 56)
            (name "Compose two functions" function (funcall (my-compose-two add3 mul2) 10) result 26)
            (name "Compose a list of functions - tail recursion" function (funcall (my-compose-list (list add10 add3 mul2 add10)) 10) result 56)
            (name "Compose the functions received as parameters" function (funcall (my-compose add10 add3 mul2 add10) 10) result 56)
        )
    )

)))

(setq add3 (lambda (value)
    (+ value 3)
))

(setq add10 (lambda (value)
    (+ value 10)
))

(setq mul2 (lambda (value)
    (* value 2)
))


;;; Load and test the functions
(load "impl.lsp")
(display-exam (verify-exam exam))
