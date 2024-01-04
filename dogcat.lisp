(setf *costs* '((A 1) (B 2) (C 2) (D 2) (E 1) (F 2) (G 2) (H 2) (I 1) (J 4) (K 4) (L 1) (M 2) (N 1) (O 1) (P 2) (Q 4) (R 1) (S 1) (T 1) (U 1) (V 2) (W 2) (X 4) (Y 2) (Z 4)))


(defun successors (lst)
  ; set *legal-words* list to empty
  (setf *legal-words* '())
  ; call recursive function to fill *legal-words*
  (find-words lst *costs*)
  ; if *legal-words* is null return empty list else return list
  (cond ((null *legal-words*) '())
        (t (cons (car *legal-words*) (cdr *legal-words*)))))

(defun find-words (wd *cost*)
  (cond ((null  *cost*) nil)
        ; check to see if replacing first letter results in legal word
        ((legalword (implode (cons (car (car *cost*)) (cdr (explode wd))))) (legal-words (cons (implode (cons (car (car *cost*)) (cdr (explode wd)))) (cdr (car *cost*))) wd *cost*))
        ; check to see if replacing second letter results in legal word
        ((legalword (implode (append (cons (car (explode wd)) (list (caar *cost*))) (cddr (explode wd))))) (legal-words (cons (implode (append (cons (car (explode wd)) (list (caar *cost*))) (cddr (explode wd)))) (cdr (car *cost*))) wd *cost*))
        ; check to see if replacing third letter results in legal word
        ((legalword (implode (append (cons (car (explode wd))(list(car (cdr (explode wd))))) (list (caar *cost*))))) (legal-words (cons (implode (append (cons (car (explode wd))(list(car (cdr (explode wd))))) (list (caar *cost*)))) (cdr (car *cost*))) wd *cost*))
        ; no legal words found so move to next letter
        (t (find-words wd (cdr *cost*)))))

(defun goal-state (node)
  ; set goalstate word to CAT
  (equal node 'CAT))


(defun legal-words (wd wd1 *cost*)
  ; if legal word doesnt equal first word (DOG) then add word to list
  (cond ((equal wd1 (car wd)) nil)
        (t (setf *legal-words* (cons wd *legal-words*))))
  ; continue searching alphabet for legal words
  (cond ((null (cdr *cost*)) nil)
        (t (find-words wd1 (cdr *cost*)))))

(defun dogcat-heuristic (node)
  ; call recursive helper function
  (h-helper 'CAT node))

(defun h-helper (goal node)
        ; if goal is null return 0
  (cond ((null goal) 0)
        ; elif the first char in goal and node are equal we dont need to
        ; make a move so add 0 to the move cost
        ((equal (car (explode goal)) (car (explode node))) (+ 0 (h-helper (implode (cdr(explode goal))) (implode (cdr (explode node))))))
        ; else we need to make a move so add 1 to move cost
        (t (+ 1 (h-helper (implode (cdr(explode goal))) (implode (cdr (explode node))))))))

(defun dogcat-depth-first-search (path-cost dogcat-heuristic) 0)

(defun dogcat-uniform-cost-search (successors dogcat-heur) successors)

(defun dogcat-breadth-first-search(path-cost dogcat-heuristic) NodesInGraph)

(defun dogcat-greedy-best-first-search (successors dogcat-heuristic) dogcat-heuristic)

(defun dogcat-a-search (successors dogcat-heuristic) (+ successors dogcat-heuristic))
 
