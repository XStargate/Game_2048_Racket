#lang racket

(require 2htdp/image
           racket/cmdline
           (only-in racket/draw read-bitmap))

(require 2htdp/universe)

(define (make-board n)
  (make-list n (make-list n 0)))

(define PIECE_DIST '(2 2 2 2 2 2 2 2 2 4))
(define (choice l)
  (if (list? l) (list-ref l (random (length l)))
      (vector-ref l (random (vector-length l)))))
(define (get-a-piece)
  (choice PIECE_DIST))

(define (avail? lst)
  (if (list? lst)
      (ormap avail? lst)
      (zero? lst)))

(define (get-empty-refs lst zero-fun?)
  (for/list ([item lst]
             [i (range (length lst))]
             #:when (zero-fun? item))
    i))

(define (put-random-piece lst)
  (if (avail? lst)
      (if (list? lst)
          (let* ([i (choice (get-empty-refs lst avail?))]
                 [v (list-ref lst i)])
            (append (take lst i)
                    (cons (put-random-piece v) (drop lst (add1 i)))))
          (get-a-piece))
      lst))

(define (init-board n)
  (put-random-piece (put-random-piece (make-board n))))

(define (merge row)
  (cond [(<= (length row) 1) row]
        [(= (first row) (second row))
         (cons (* 2 (first row)) (merge (drop row 2)))]
        [else (cons (first row) (merge (rest row)))]))

(define (move-row row v left?)
  (let* ([n (length row)]
         [l (merge (filter (lambda (x) (not (zero? x))) row))]
         [padding (make-list (- n (length l)) v)])
    (if left?
        (append l padding)
        (append padding l))))

(define (move lst v left?)
  (map (lambda (x) (move-row x v left?)) lst))

(define (move-left lst)
  (put-random-piece (move lst 0 #t)))

(define (move-right lst)
  (put-random-piece (move lst 0 #f)))

(define (transpose lsts)
  (apply map list lsts))

(define (move-up lst)
  ((compose1 transpose move-left transpose) lst))

(define (move-down lst)
  ((compose1 transpose move-right transpose) lst))

(define ALL-OPS (list move-right move-down move-left move-up))

(define (finished? lst)
  (cons (andmap (lambda (op) (equal? lst (op lst))) ALL-OPS) "finished"))

(define (win? lst)
  (if (list? lst)
      (ormap win? lst)
      (equal? 2048 lst)))

(define (wingame? lst)
  (cons (win? lst) "wingame"))

(define (endgame? lst)
  (or (car (finished? lst)) (car (wingame? lst))))

(define (hex->rgb hex [alpha 255])
    (define r (regexp-match #px"^#(\\w{2})(\\w{2})(\\w{2})$" hex))
    (define (append-hex s) (string-append "#x" s))
    (define (color-alpha c) (apply color (append c (list alpha))))
    (if r
        (color-alpha (map (compose1 string->number append-hex) (cdr r)))
        #f))

(define ALPHA 184)
(define GRID-COLOR (hex->rgb "#bbada0"))
(define TILE-BG
  (make-hash (map (λ (item) (cons (first item) (hex->rgb (second item))))
                  '((0    "#ccc0b3") (2    "#eee4da") (4    "#ede0c8")
                                     (8    "#f2b179") (16   "#f59563") (32   "#f67c5f")
                                     (64   "#f65e3b") (128  "#edcf72") (256  "#edcc61")
                                     (512  "#edc850") (1024 "#edc53f") (2048 "#edc22e")))))
(define TILE-FG 'white)
(define TILE-SIZE 80)
(define TILE-TEXT-SIZE 50)
(define MAX-TEXT-SIZE 65)
(define TILE-SPACING 5)

(define (make-tile n)
  (define (text-content n)
    (if (zero? n) ""
        (number->string n)))

  (overlay (let* ([t (text (text-content n) TILE-TEXT-SIZE TILE-FG)]
                  [v (max (image-width t) (image-height t))]
                  [s (if (> v MAX-TEXT-SIZE) (/ MAX-TEXT-SIZE v) 1)])
             (scale s t))
           (square TILE-SIZE 'solid (hash-ref TILE-BG n))
           (square (+ TILE-SIZE (* 2 TILE-SPACING)) 'solid GRID-COLOR)))

(define (image-append images get-pos overlap)
  (if (<= (length images) 1)
      (car images)
      (let* ([a (first images)]
             [b (second images)]
             [img (apply overlay/xy (append (list a) (get-pos a overlap) (list b)))])
        (image-append (cons img (drop images 2)) get-pos overlap))))

(define (hc-append images [overlap 0])
    (image-append images
                  (lambda (img o) (list (- (image-width img) o) 0))
                  overlap))
(define (vc-append images [overlap 0])
  (image-append images
                (lambda (img o) (list 0 (- (image-height img) o)))
                overlap))

(define (show-board b)
  (let ([images ( for/list ([row b])
                           (hc-append (map make-tile row) TILE-SPACING))])
    (vc-append images TILE-SPACING)))

(define (key->ops a-key)
  (cond
    [(key=? a-key "left") move-left]
    [(key=? a-key "right") move-right]
    [(key=? a-key "up") move-up]
    [(key=? a-key "down") move-down]
    [else ( lambda (x) x)]))

(define (show-board-over b)
    (let* ([board (show-board b)]
           [layer (square (image-width board) 'solid (color 0 0 0 90))])
      (overlay (text "Game over!" 40 TILE-FG)
               layer board)))

(define (show-board-win b)
  (let* ([board (show-board b)]
         [layer (square (image-width board) 'solid (color 0 0 0 90))])
    (overlay (text "You win!" 40 TILE-FG)
             layer board)))

(define (show-board-end b)
  (if (car (finished? b))
      (show-board-over b)
      (show-board-win b)))

(define (change b key)
  ((key->ops key) b))

(define (start n)
  (big-bang (init-board n)
            (to-draw show-board)
            (on-key change)
            (stop-when endgame? show-board-end)
            ;;(stop-when win? show-board-win)
            (name "2048 - racket")))
