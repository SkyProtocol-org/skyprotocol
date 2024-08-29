(import :gerbil/gambit)

(def make-stack
  (lambda ()
          (let ((ls '())) 
            (lambda (msg . args)
                    (cond
                      ((eqv? msg 'empty?) (null? ls))
                      ((eqv? msg 'push!) (set! ls (cons (car args) ls)))
                      ((eqv? msg 'top) (car ls))
                      ((eqv? msg 'pop!) (set! ls (cdr ls)))
                      (else "oops"))))))

(def stack (make-stack))

(displayln (stack 'empty?))
(stack 'push! 'a)
(displayln (stack 'empty?))
(stack 'push! 'b)
(displayln (stack 'top))

(define factorial
  (lambda (n)
          (do ((i n (- i 1)) (a 1 (* a i))) 
              ((zero? i) a))))

(displayln (factorial 3))


(define (stream-car s) (car (force s)))

(define (stream-cdr s) (cdr (force s)))

(define (stream-map f s)
  (delay (cons (f (stream-car s)) (stream-map f (stream-cdr s)))))

(define (stream-add s1 s2)
  (delay (cons (+ (stream-car s1) (stream-car s2)) (stream-add (stream-cdr s1) (stream-cdr s2)))))

(define counters
  (let next ((n 1))
    (delay (cons n (next (+ n 1))))))

(displayln (stream-car counters))

(displayln (stream-car (stream-cdr counters)))

(define counters-squared
  (stream-map (lambda (e) (* e e)) counters))

(displayln (stream-car counters-squared))
(displayln (stream-car (stream-cdr counters-squared)))
(displayln (stream-car (stream-cdr (stream-cdr counters-squared))))


;; TCP server example

(define (start-server port)
  (let* ((server-socket (##tcp-listen port 5)) ;; create a listening TCP socket on specified port
        (client-socket (##tcp-accept server-socket))) ;; wait for a client to connect
    (let loop ((input (open-input-output-port client-socket)))
      (let ((line (read-line input)))
        (if line
            (begin
              (displayln line)
              (newline)
              (loop input))
            (close-input-port input))))
    (##tcp-close server-socket)))

(start-server 8000)
