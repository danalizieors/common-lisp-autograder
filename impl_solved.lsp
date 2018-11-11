;;;; Write your solutions here

; command to run the test:
; clisp "test.lsp"

; colored printing enabled
(setq colored-print T)


;;; A fisherman's tale

(defun my-hand (value)
    value
)

(defun my-left-pocket (value)
    (car value)
)

(defun my-right-pocket (value)
    (cdr value)
)

(defun my-picking-fish-from-tree (value)
    (cddadr value)
)

(defun my-from-pothole (value)
    (first value)
)

(defun my-from-farthest-corner (value)
    (first (last value))
)

(defun my-from-deep-sea (value)
    (second (third (second (fourth value))))
)

(defun my-from-deep-trench (value)
    (cdaaar (caadar (caaaar value)))
)


;;; Dictionaries from lists

(defun my-add (key value dictionary)
    (append (list key value) dictionary) 
)

(defun my-get-all-keys (dictionary)
    (if (< 1 (length dictionary))
        (cons (first dictionary)
              (my-get-all-keys (cddr dictionary))
        )
    )
)

(defun my-get-all-values (dictionary)
    (if (< 1 (length dictionary))
        (cons (second dictionary)
              (my-get-all-values (cddr dictionary))
        )
    )
)

(defun my-get-value (key dictionary)
    (if (< 1 (length dictionary))
        (if (equal key (first dictionary))
            (second dictionary)
            (my-get-value key (cddr dictionary))
        )
    )
)

(defun my-get-key (value dictionary)
    (if (< 1 (length dictionary))
        (if (equal value (second dictionary))
            (first dictionary)
            (my-get-key value (cddr dictionary))
        )
    )
)

(defun my-remove-with-key (key dictionary)
    (if (< 1 (length dictionary))
        (if (equal key (first dictionary))
            (my-remove-with-key key (cddr dictionary))
            (my-add (first dictionary)
                    (second dictionary)
                    (my-remove-with-key key (cddr dictionary))
            )
        )
        dictionary
    )
)

(defun my-optimize (dictionary)
    (if (< 1 (length dictionary))
        (my-add (first dictionary)
                (second dictionary)
                (my-optimize 
                    (my-remove-with-key (first dictionary) (cddr dictionary))
                )
        )
    )
)


;;; The flattest of the maps

(defun my-increment-each-by10 (values)
    (mapcar add10 values)
)

(defun my-map (fn values)
    (if values
        (cons
            (funcall fn (first values))
            (my-map fn (rest values))
        )
    )
)

(defun my-flatten (values)
    (cond
        ((null values) nil)
        ((atom values) (list values))
        (T
            (append (my-flatten (first values))
                    (my-flatten (rest values))
            )
        )
    )
)

(defun my-flatmap (fn values)
    (my-map fn (my-flatten values))
)

(defun my-deepmap (fn values)
    (cond
        ((null values) nil)
        ((atom values) (funcall fn values))
        (T  (cons
                (my-deepmap fn (first values))
                (my-deepmap fn (rest values))
            )
        )
    )
)


;;; Wolfgang Amadeus Mozart

(defun my-call-two (value fn-a fn-b)
    (funcall fn-b (funcall fn-a value))
)

(defun my-call-list (value fns)
    (if fns
        (my-call-list (funcall (first fns) value) (rest fns))
        value
    )
)

(defun my-compose-two (fn-a fn-b)
    (lambda (value) (my-call-two value fn-a fn-b))
)

(defun my-compose-list (fns)
    (lambda (value) (my-call-list value fns))
)

(defun my-compose (&rest fns)
    (my-compose-list fns)
)
