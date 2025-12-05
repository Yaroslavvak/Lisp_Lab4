(defun func-bubble-sort-hof (list &key (key #'identity) (test #'<))
  (let* ((keyed-list (mapcar (lambda (x) (cons (funcall key x) x)) list)))
    (labels 
        ((bubble-pass (lst)
           (cond
             ((null (cdr lst)) lst)
             ((funcall test (caadr lst) (caar lst)) 
              (cons (cadr lst) 
                    (bubble-pass (cons (car lst) (cddr lst)))))
             (t 
              (cons (car lst) 
                    (bubble-pass (cdr lst))))))
         (sort-rec (lst n)
           (if (zerop n)
               lst
               (sort-rec (bubble-pass lst) (1- n)))))
      (mapcar #'cdr (sort-rec keyed-list (length list))))))

(defun add-prev-reducer (&key (transform #'identity))
  (let ((prev nil)) 
    (lambda (acc elem)
      (let* ((curr-val (funcall transform elem)) 
             (pair (cons curr-val prev)))        
        (setf prev curr-val)                      
        (cons pair acc)))))

(defun check-test (name input expected actual)
  (let ((*print-pretty* nil))
    (if (equal expected actual)
        (format t "passed... ~24a Input: ~24a Result: ~a~%" name input actual)
        (format t "FAILED... ~24a Input: ~24a~% Expected: ~a~% Got: ~a~%" name input expected actual))))

(defun test-part-1 ()
  (format t "~%Testing Part 1: Bubble Sort HOF~%")
  (check-test "Test 1 (Normal)"    '(3 1 4 1 5 9 2 6)   '(1 1 2 3 4 5 6 9)    (func-bubble-sort-hof '(3 1 4 1 5 9 2 6)))
  (check-test "Test 2 (Desc)"      '(1 2 3 4 5)         '(5 4 3 2 1)          (func-bubble-sort-hof '(1 2 3 4 5) :test #'>))
  (check-test "Test 3 (Key Len)"   '((1 2 3) (1) (1 2)) '((1) (1 2) (1 2 3))  (func-bubble-sort-hof '((1 2 3) (1) (1 2)) :key #'length))
  (check-test "Test 4 (Empty)"     '()  '() (func-bubble-sort-hof '())))

(defun test-part-2 ()
  (format t "~%Testing Part 2: add-prev-reducer~%")
  
  (check-test "Test 1 (Simple)"    '(1 2 3)    '((1 . NIL) (2 . 1) (3 . 2))  (reverse (reduce (add-prev-reducer) '(1 2 3) :initial-value nil)))  
  (check-test "Test 2 (Transform)" '(1 2 3)    '((2 . NIL) (3 . 2) (4 . 3))  (reverse (reduce (add-prev-reducer :transform #'1+) '(1 2 3) :initial-value nil)))
  (check-test "Test 3 (Wrap List)" '(a b c)    '(((A) . NIL) ((B) . (A)) ((C) . (B))) (reverse (reduce (add-prev-reducer :transform #'list) '(a b c) :initial-value nil)))
  (check-test "Test 4 (Empty)"     '()  '()  (reverse (reduce (add-prev-reducer) '() :initial-value nil))))

(test-part-1)
(test-part-2)
