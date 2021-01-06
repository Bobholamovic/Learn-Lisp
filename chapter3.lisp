;;;; Chapter 3


;;; 2

(defun new-union (x y)
  (if (null y)
      x
      (if (member (car y) x)
          (new-union x (cdr y))
          (new-union (append x (list (car y))) (cdr y)))))

(new-union '(a b c) '(b a d))


;;; 3

(defun occurrences (lst)
  (let ((alist nil))
    (dolist (ele lst)
      (let ((pair (assoc ele alist)))
        (if (null pair)
            (push (cons ele 1) alist)
            (incf (cdr pair)))))
    (sort alist #'> :key #'cdr)))

(occurrences '(a b a d a c d c a))


;;; 5

;;; Iterative

(defun pos+-iter (lst)
  (let ((idx 0)
        (lst+ nil))
    (dolist (ele lst)
      (setf lst+ (append lst+ (list (+ idx ele))))
      (incf idx))
    lst+))

;;; Recursive

(defun pos+-recr (lst)
  (defun recr (lst idx)
    (if (null lst)
        nil
        (cons (+ (car lst) idx) (recr (cdr lst) (+ idx 1)))))
  (recr lst 0))

;;; mapcar

(defun pos+-mapcar (lst)
  (let ((idx -1))
    (mapcar #'(lambda (x) (+ x (incf idx))) lst)))

(pos+-iter '(7 5 1 4))
(pos+-recr '(7 5 1 4))
(pos+-mapcar '(7 5 1 4))


;;; 7

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql next elt)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (cons n elt)
      elt))

(compress '(1 1 1 0 1 0 0 0 0 1))


;;; 8

(defun showdots (lst)
  (if (atom lst)
      (format nil "~A" lst)
      (format nil "(~A . ~A)" (showdots (car lst)) (showdots (cdr lst)))))

(showdots '(a b c))


;;; 9

(defun longest-path (start end net)
  (bfs end (list (list start)) net nil))

(defun bfs (end queue net res)
  (if (null queue)
      (reverse res)
      (let ((path (car queue)))
        (let ((node (car path)))
          (bfs end
               (append (cdr queue)
                       (new-paths path node net))
               net
               (if (eq (car path) end) path res))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (remove-if #'(lambda (x) (member x path)) (cdr (assoc node net)))))

(longest-path 'a 'd '((a b c) (b c) (c d)))
