<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Вакульчук Ярослав Віталійович КВ-22</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
використати функції вищого порядку для роботи з послідовностями та додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями, при цьому key має виконатись мінімальну кількість разів.

2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом. Використання псевдофункцій не забороняється, але, за
можливості, має бути зменшене до необхідного мінімуму.

## Варіант першої частини №2
Алгоритм сортування обміном No1 (без оптимізацій) за незменшенням.

## Лістинг реалізації першої частини завдання
```lisp
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
```
### Тестові набори та утиліти першої частини
```lisp
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
```

### Тестування першої частини
```lisp
Testing Part 1: Bubble Sort HOF
passed... Test 1 (Normal)          Input: (3 1 4 1 5 9 2 6)        Result: (1 1 2 3 4 5 6 9)
passed... Test 2 (Desc)            Input: (1 2 3 4 5)              Result: (5 4 3 2 1)
passed... Test 3 (Key Len)         Input: ((1 2 3) (1) (1 2))      Result: ((1) (1 2) (1 2 3))
passed... Test 4 (Empty)           Input: NIL                      Result: NIL
```

## Варіант другої частини №2
Написати функцію add-prev-reducer , яка має один ключовий параметр — функцію
transform . add-prev-reducer має повернути функцію, яка при застосуванні в якості
першого аргументу reduce робить наступне: кожен елемент списку-аргументу reduce
перетворюється на точкову пару, де в комірці CAR знаходиться значення поточного
елемента, а в комірці CDR знаходиться значення попереднього елемента списку. Якщо функція transform передана, тоді значення
поточного і попереднього елементів, що потраплять у результат, мають бути змінені
згідно transform . Обмеження, які накладаються на використання функції-результату
add-prev-reducer при передачі у reduce визначаються розробником (тобто,
наприклад, необхідно чітко визначити, якими мають бути значення ключових параметрів
функції reduce from-end та initial-value ). transform має виконатись мінімальну кількість разів.

## Лістинг реалізації другої частини завдання
```lisp
(defun add-prev-reducer (&key (transform #'identity))
  (let ((prev nil)) 
    (lambda (acc elem)
      (let* ((curr-val (funcall transform elem)) 
             (pair (cons curr-val prev)))        
        (setf prev curr-val)                      
        (cons pair acc)))))
```
### Тестові набори та утиліти другої частини
```lisp
(defun test-part-2 ()
  (format t "~%Testing Part 2: add-prev-reducer~%")
  
  (check-test "Test 1 (Simple)"    '(1 2 3)    '((1 . NIL) (2 . 1) (3 . 2))  (reverse (reduce (add-prev-reducer) '(1 2 3) :initial-value nil)))  
  (check-test "Test 2 (Transform)" '(1 2 3)    '((2 . NIL) (3 . 2) (4 . 3))  (reverse (reduce (add-prev-reducer :transform #'1+) '(1 2 3) :initial-value nil)))
  (check-test "Test 3 (Wrap List)" '(a b c)    '(((A) . NIL) ((B) . (A)) ((C) . (B))) (reverse (reduce (add-prev-reducer :transform #'list) '(a b c) :initial-value nil)))
  (check-test "Test 4 (Empty)"     '()  '()  (reverse (reduce (add-prev-reducer) '() :initial-value nil))))
```
### Тестування другої частини
```lisp
Testing Part 2: add-prev-reducer
passed... Test 1 (Simple)          Input: (1 2 3)                  Result: ((1) (2 . 1) (3 . 2))
passed... Test 2 (Transform)       Input: (1 2 3)                  Result: ((2) (3 . 2) (4 . 3))
passed... Test 3 (Wrap List)       Input: (A B C)                  Result: (((A)) ((B) A) ((C) B))
passed... Test 4 (Empty)           Input: NIL                      Result: NIL
```


