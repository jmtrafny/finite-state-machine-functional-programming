#lang racket

; This grammer accepts strings in the form: a(a^b)*a
;
;(define initial_state 0)
;(define final_states (list 2))
;(define delta_fn (list
;                       (list 0 "a" 1)
;                       (list 0 "b" 3)
;                       (list 1 "a" 2)
;                       (list 1 "b" 1)
;                       (list 2 "a" 2)
;                       (list 2 "b" 1)
;                       (list 3 "a" 3)
;                       (list 3 "b" 3)))

; This grammer accepts strings in the form: 0*1*2*
;
;(define initial_state 0)
;(define final_states (list 0 1 2))
;(define delta_fn (list
;                       (list 0 "0" 0)
;                       (list 0 "1" 1)
;                       (list 0 "2" 2)
;                       (list 1 "0" 3)
;                       (list 1 "1" 1)
;                       (list 1 "2" 2)
;                       (list 2 "0" 3)
;                       (list 2 "1" 3)
;                       (list 2 "2" 2)
;                       (list 3 "0" 3)
;                       (list 3 "1" 3)
;                       (list 3 "2" 3)))

; This grammer accepts strings in the form: a+b+a*
;
(define initial_state 0)
(define final_states (list 2 3))
(define delta_fn (list
                       (list 0 "a" 1)
                       (list 0 "b" 4)
                       (list 1 "a" 1)
                       (list 1 "b" 2)
                       (list 2 "a" 3)
                       (list 2 "b" 2)
                       (list 3 "a" 3)
                       (list 3 "b" 4)
                       (list 4 "a" 4)
                       (list 4 "b" 4)))

;;;
;;; UNCOMMENT ONE OF THE MACHINES ABOVE THIS LINE, OR CREATE YOUR OWN
;;;


; Get test string from user
(writeln "Enter a string to test (space deliniated):")
(define in_string (car (list (string-split (read-line)))))

; Set up for initial state
(define current_state initial_state)
(define next_state current_state)
(define filtered_delta_fn '()) 

; fsm will return True if in_string is accepted
(define (fsm current_state in_string delta_fn)
  (cond
    [(empty? in_string) (cond
                          [(member current_state final_states) 'string-ACCEPTED]
                          [else 'string-REJECTED])] 
    [else (let ([input (car in_string)])
            (let ([filtered_delta_fn (filter (lambda (element) (match element
                                                                 [(list (== current_state) (== input) _) #t]
                                                                 [_ #f]))
                                             delta_fn)])
              (let ([next_state (car (cdr (cdr (car filtered_delta_fn))))])
                (fsm next_state (cdr in_string) delta_fn))))]
  )
)

; Apply fsm
(fsm current_state in_string delta_fn)






