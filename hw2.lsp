;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; TODO: comment code

;NAME: Giovanni Moya
;UID: 505129076

;Q1 write a func that does BFS that takes in FRINGE
; as an argument

;explore nodes level-by-level and return each level 
; which is made up of nodes in order 

(defun BFS (FRINGE)
  (cond ((null FRINGE) NIL) ;if root is null return empty list
       ((listp (car FRINGE)) (BFS (append (cdr FRINGE) (car FRINGE))))
       ;if root not null append car to cdr 
       (T (cons (car FRINGE) (BFS (cdr FRINGE))))
       ; recursive step to call BFS by appending cdr to car 
       )
)

;test cases provided in hw

;(print (BFS '(ROOT)))
;(print (BFS '((((L E) F) T))))
;(print (BFS '((R (I (G (H T)))))))
;(print (BFS '(((A (B)) C (D)))))
;(print (BFS '((T (H R E) E))))
;(print (BFS '((A ((C ((E) D)) B)))))

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
  (equal S '(T T T T));returns T if true, false otherwise
)
; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
   ;case homer is alone
  (cond ((equal A 'h)
	 ;case no solution: posion with baby or dog with baby
	 (cond ((and (equal (first S) (second S)) ;homer alone
		     (or (equal (second S) (third S))  
			 (equal (second S) (fourth S))
		     )
		) Nil )
	       (T (list (cons (not (car S)) (cdr S))))
	 )
	)

   ;case homer with dog
   ((equal A 'd)
    ;when homer is not with the dog
    (cond ((not (equal (car S) (third S))) 
	   NIL)
	  ;when baby with poision
	  ((equal (second S) (fourth S))
	   NIL)
	  ;right case
	  (T (list (list 
		    (not (car S))
		   (second S) 
		   (not (third S))
		   (fourth S)
		   )
	     )
	  )
   )
  )
   ; case Home with baby                                                                             
   ((equal 'b A)

  (cond ((equal (first S) (second S))
         (list (list
                (not (first S))
                (not (second S))
                (third S)
                (fourth S))))
        ;when homer is not with the baby                                                             
        (T NIL)
   )
  )
   ;case homer with poision
   ((equal A 'p)
    ;when homer is not with poison
    (cond ((not 
	    (equal (car S) (fourth S)))
	   NIL )
	  ;when dog is with baby
	  ((equal (second S) (third S)) 
	   NIL )
	  ;right case
	  (T (list (list
		    (not (car S))
		    (second S)
		    (third S)
		    (not (fourth S)))))))
   ;else
   (T NIL)
   )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S) ; expand and explore states
  (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES) ;no state is on stack
  (cond ((null STATES) NIL ) 
	(T (cond ((equal (car STATES) S) T) ; some state is on stack
	   (T (ON-PATH S (cdr STATES))) ; search through states
	   )
	)
  )
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
  (cond ((null STATES)
	 NIL ) ;store state
	(T (let ((res (DFS (car STATES) PATH))) ; recusive step 
		 (cond ((null res) (MULT-DFS (cdr STATES) PATH))
		       (T res) ; state match
		 )
	   )
	   )
	)
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
  ; goal state found
  (cond ((FINAL-STATE S) 
	 (append PATH (list S)))
	
	((ON-PATH S PATH) ;no op because already checked
	 NIL )
	
	(T (MULT-DFS 
	    (SUCC-FN S) (append PATH (list S))
	    )
	   )
	;still looking for goal state
  )
)
    
;(print (DFS '(() T () T) ())) 
;(print (DFS '(T T T T) ()))
;(print (DFS '(T () () ()) ())) 
;(print (DFS '(() () () ()) ()))
