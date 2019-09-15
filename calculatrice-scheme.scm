#! /usr/bin/env gsi -:dR

;; Equipe : Paul Chaffanet
;;          Samuel Guigui

(define res cons) ; constructeur 
(define res-ASA car) ; partie ASA 
(define res-stream cdr) ; partie stream

(define char-lowerAlpha?
  (lambda (c)
    (and (char>=? c #\a)
	 (char<=? c #\z))))

(define char 
  (lambda (test) 
    (lambda (stream) 
      (and (not (null? stream)) 
	   (test (car stream)) 
	   (res (car stream) (cdr stream))))))

(define seq (lambda (p1 p2) 
	      (lambda (stream) 
		(let ((r1 (p1 stream))) 
		  (and r1
		       (let ((r2 (p2 (res-stream r1))))
			 (and r2
			      (res (cons (res-ASA r1) 
					 (res-ASA r2)) 
				   (res-stream r2)))))))))


(define choix 
  (lambda (p1 p2) 
    (lambda (stream) 
      (or (p1 stream) (p2 stream)))))

(define char= 
  (lambda (c) 
    (char (lambda (x) (char=? x c))))) 

(define iter+ ; iteration non-nulle
  (lambda (p) 
    (seq p (iter* p))))

(define iter* ; iteration possiblement nulle 
  (lambda (p) 
    (lambda (stream) 
      ((choix (iter+ p) vide) 
       stream))))

(define vide (lambda (stream) (res '() stream)))

(define fin 
  (lambda (stream) 
    (and (null? stream) 
	 (res '() stream))))

(define space (iter+ (char= #\ )))

(define delim (choix space fin))	     

(define opbin (seq (choix (char= #\+) 
			  (choix (char= #\-) (char= #\*)))
		   delim))

(define variable (seq (char char-lowerAlpha?)
		      delim))

(define opun (seq (seq (char char-lowerAlpha?)
			(char= #\=))
		  delim))

(define nombre (seq (iter+ (char char-numeric?))
		    delim))

(define L (choix opun
		 (choix opbin
			(choix variable 
			       (choix nombre space)))))

(define opbin? 
  (lambda (c)
    (or (char=? #\+ c)
	(char=? #\- c)
	(char=? #\* c))))

(define char->op 
  (lambda (c)
    (cond ((char=? c #\+) +)
	  ((char=? c #\-) -)
	  (else *))))

(define number->list
  (lambda (lst)
    (string->list (number->string lst))))

(define list->number 
  (lambda (lst)
    (string->number (list->string lst))))
	
(define del-assoc
  (lambda (key dict cont)
    (if (not (char=? key (caar dict)))
	(del-assoc key (cdr dict) (lambda (r) (cont (cons (car dict) r))))
	(cont (cdr dict)))))

(define operation
  (lambda (stack)
    (if (< (length (car stack)) 2)
	(string-append "Error syntax : Ill-formed expression, operator '"
		       (list->string (list (cadr stack))) 
		       "' has less than two arguments. Expressions must be in postfix form\n"
		       "Example : 1 + 2 -> 1 2 +\nFix your expression and retry.\n\n")
	(let ((resultat (number->list ((char->op (cadr stack))
				       (list->number (reverse (cadar stack)))
				       (list->number (reverse (caar stack)))))))
	  (cons (cons (reverse resultat)
		      (cddar stack))
		(cddr stack))))))

(define affectation
  (lambda (c value dict)
    (if (< (length dict) 1)
	(cons (cons c  value) '())
	(let ((var (assoc c dict)))     
	  (if var
	      (cons (cons c value)
		    (del-assoc (car var) dict (lambda (r) r)))
	      (cons (cons c value)
		    dict))))))

(define slice-expr 
  (lambda (expr stack)
    (if (null? expr)
	(cons '() stack)
	(let ((r (L expr)))
	  (if (not r)
	      "Error syntax : Ill-formed word. Fix your expression and retry.\n\n"
	      (if (and (char? (caar r))
		       (char=? #\ (caar r)))
		  (slice-expr (cdr r) stack)
		  (slice-expr (cdr r) (cons (caar r)
					    stack))))))))

(define evaluate-stack
  (lambda (stack dict)
    (if (string? stack)
	stack
	(if (null? (cdr stack))
	    (if (= 1 (length (car stack)))
		(cons (caar stack) dict)
		"Error Posfix : At least one expression doesn't have an operator. Fix your expression and retry.\n\n")
	    (cond ((list? (cadr stack))
		   (evaluate-stack (cons (cons (cadr stack) 
					       (car stack))
					 (cddr stack))
				   dict))
		  ((not (char? (cadr stack)))
		   (if (null? (car stack))
		       "Error assignation : Missing value for assignment. Fix your expression and retry.\n\n" 
		       (evaluate-stack (cons (car stack)
					     (cddr stack))
				       (affectation (caadr stack) 
						    (list->number (reverse (caar stack)))
						    dict))))
		  ((opbin? (cadr stack))
		   (evaluate-stack (operation stack) dict))
		  (else		   
		   (let ((var (assoc (cadr stack) dict)))
		     (if (not var)
			 (string-append "Error 404 variable '"
					(list->string (list (cadr stack)))
					"' not found. Fix your expression and retry.\n\n")
			 (evaluate-stack (cons (cons (reverse (number->list (cdr var)))
						     (car stack))
					       (cddr stack))
					 dict)))))))))

(define calculate-expr
  (lambda (expr dict)
    (let ((stack (slice-expr (reverse expr) '())))
      (if (string? stack)
	  stack
	  (evaluate-stack stack dict)))))
     
(define traiter
  (lambda (expr dict)
    (if (null? expr)
	(cons '(#\newline) dict)
	(let ((resultat (calculate-expr expr dict)))
	  (if (string? resultat)
	      (cons (string->list resultat)
		    dict)
	      (cons (append (reverse (car resultat))
			    '(#\newline))
		    (cdr resultat)))))))
;;;----------------------------------------------------------------------------

(define repl
  (lambda (dict)
    (print "# ")
    (let ((ligne (read-line)))
      (if (string? ligne)
          (let ((r (traiter-ligne ligne dict)))
            (for-each write-char (car r))
            (repl (cdr r)))))))

(define traiter-ligne
  (lambda (ligne dict)
    (traiter (string->list ligne) dict)))

(define main
  (lambda ()
    (repl '()))) ;; dictionnaire initial est vide
    
;;;---------------------------------------------------------------------------
