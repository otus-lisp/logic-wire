(define-library (src main)
   (import (otus lisp)
           (otus random!))
   (export
      load-xpm3
      load-layout

      xpm3->board
      board-texture

      board-energize
      board-simulate
   )

(begin
   (import (owl parse)
      (file xpm3)
      (lang sexp)
      (lib sha1))

   (define maybe-whitespaces (greedy* (byte-if (lambda (c) (has? '(#\tab #\newline #\space #\return) c)))))
   (define layout-parser
      (let-parse* (
            (colors (greedy+ (let-parse* (
                        (skip (imm #\:))
                        (name byte)
                        (value (sexp))
                        (skip maybe-whitespaces))
                     [name #\c (string->list (ref value 1))])))
            (skip (imm #\+)) (skip (greedy+ (imm #\-))) (skip (imm #\+)) (skip (imm #\newline))
            (bitmap (greedy+ (let-parse* (
                        (skip (imm #\|))
                        (chars (greedy+ (byte-if (lambda (x) (not (eq? x #\|))))))
                        (skip (imm #\|))  (skip (imm #\newline)))
                  chars))) )
            ;; (skip (imm #\+)) (skip (greedy+ (imm #\-))) (skip (imm #\+)) (skip (imm #\newline)))
         {
            'width (length (car bitmap))
            'height (length bitmap)
            'bitmap (make-vector (map make-vector bitmap))
            'colors (+ (length colors) 1)
            'color-table colors
         }))

   (define (rref board i j) (ref (ref board j) i))

   (define (load str parser)
      (define file (try-parse parser (str-iter str) #f))
      (if file
         (car file)))
   (define (load-xpm3 str) (load str xpm3-parser))
   (define (load-layout str) (load str layout-parser))

   (define (xpm3-width xpm3)
      (xpm3 'width))
   (define (xpm3-height xpm3)
      (xpm3 'height))

   (define (xpm3->board xpm3)
      ; step 1: find all closed circuits
      ; step 2: find all NOT gates and put into separate dictionary
      ; step 3: connect circuits to the games as "in" and "out" sockets

      (define not-a-wire #\space)
      ; очистим "черные" клеточки (впишем вместо них #f):
      (define board (vector-map (lambda (row)
            (vector-map (lambda (cell)
                  (if (not (eq? cell not-a-wire)) cell))
               row))
         (xpm3 'bitmap)))

      (define width (xpm3 'width))
      (define height (xpm3 'height))

      (define color-table (fold (lambda (ff array)
            (define index (ref array 1))
            (define color (list->string (ref array 3)))
            (put ff index (cond
               ((string-eq? color "red")    [[195  20  20] [255   0   0]])
               ((string-eq? color "green")  [[ 20 195  20] [  0 255   0]])
               ((string-eq? color "blue")   [[ 20  20 195] [  0   0 255]])

               ((string-eq? color "yellow") [[195 195  20] [255 255   0]])
               ((string-eq? color "magenta")[[195  20 195] [255   0 255]])
               ((string-eq? color "cyan")   [[  0 195 195] [  0 255 255]])

               ((string-eq? color "white")  [[195 195 195] [255 255 255]])
               ;; ((string-eq? color "gray")   [[105 105 105] [128 128 128]])
               ((string-eq? color "orange") [[255 140   0] [255 215   0]]) ; DarkOrange / Orange

               ((string-eq? color "black")  [[  0   0   0] [ 10  10  10]])
               ((string-eq? color "None")   [[ 17  17  17] [ 97  97  97]]) ; None
               (else [[229 229 229] [255 255 255]])))) ; неизвестный цвет
         { #f [[  0   0   0] [ 32  32  32]] } ; фоновый цвет (с индикатором ошибки)
         (xpm3 'color-table)))
      (print (xpm3 'color-table))
      ;; (print "color-table: " color-table)

      ;; ; find power color key (if exist)
      ;; (define power-color (fold (lambda (id color)
      ;;       (if id id
      ;;       else
      ;;          (define index (ref color 1))
      ;;          (if (string-eq? (list->string (ref color 3)) "power") (ref color 1))))
      ;;    #f
      ;;    (xpm3 'color-table)))
      ;; ;; (print "power-color: " power-color)

      ; 1. let's find all wires,
      ;    check the cross-wire connections
      (define wires (vector-map (lambda (row)
            (make-vector (size row))) board))

      (display "Loading wires... ")
      (define wires-count
         (fold (lambda (n j)
                  (fold (lambda (n i)   ; if already assigned or not a wire
                           (if (or (rref wires i j) (not (rref board i j)))
                              n
                           else
                              ; well, we have found a new unassigned wire
                              (define N (++ n))
                              (let loop ((i i) (j j))
                                 (if (or (rref wires i j) (not (rref board i j)))
                                    ; check possible cross-wire connection (first LogicWire pattern)
                                    (and (not (rref board i j))  ; empty center
                                       ; empty corners
                                       (not (rref board (- i 1) (- j 1)))
                                       (not (rref board (+ i 1) (- j 1)))
                                       (not (rref board (+ i 1) (+ j 1)))
                                       (not (rref board (- i 1) (+ j 1)))
                                       ; wired sides
                                       (rref board (- i 1) j)
                                       (rref board (+ i 1) j)
                                       (rref board i (- j 1))
                                       (rref board i (+ j 1)))
                                 else
                                    ; just directly connected wires
                                    (set-ref! (ref wires j) i N)
                                    (for-each (lambda (di dj)
                                          (if (loop (+ i di) (+ j dj))
                                             ; if there are cross-wire connection found
                                             (loop (+ i (* di 2)) (+ j (* dj 2)))))
                                       '(-1  0 +1  0)
                                       '( 0 -1  0 +1)))) ; #false
                              N))
                     n (iota width)))
            0 (iota height)))
      (print "loaded " wires-count)
      ;; (print "wires: " wires)

      ; step 2: find all NOT gates
      ;  -> list of [from to]
      ;  we don't need to store gate coordinates, just a way
      (display "Loading gates... ")
      (define gates
         (define (wired-cells i j)
            (fold (lambda (s di dj)
                     (+ s (if (rref board (+ i di) (+ j dj)) 1 0)))
               0
               '(-1 -1 -1  0   0 +1 +1 +1)
               '(-1  0 +1 -1  +1 -1  0 +1)))

         (fold (lambda (out j)
               (fold (lambda (out i)
                     (or
                        ; totally 4 gates with different rotation
                        (if (not (rref board i j)) ; speedup, (if central not empty)
                           (if (eq? (wired-cells i j) 6)
                              (cond
                                 ; down->up -> [from to]
                                 ((and (not (rref board (- i 1) (- j 1)))
                                       (not (rref board (+ i 1) (- j 1))))
                                    (cons [(rref wires i (+ j 1))
                                          (rref wires i (- j 1))] out))
                                 ; up->down
                                 ((and (not (rref board (- i 1) (+ j 1)))
                                       (not (rref board (+ i 1) (+ j 1))))
                                    (cons [(rref wires i (- j 1))
                                          (rref wires i (+ j 1))] out))
                                 ; left->right
                                 ((and (not (rref board (+ i 1) (- j 1)))
                                       (not (rref board (+ i 1) (+ j 1))))
                                    (cons [(rref wires (- i 1) j)
                                          (rref wires (+ i 1) j)] out))
                                 ; right->left
                                 ((and (not (rref board (- i 1) (- j 1)))
                                       (not (rref board (- i 1) (+ j 1))))
                                    (cons [(rref wires (+ i 1) j)
                                          (rref wires (- i 1) j)] out)))))
                              out))
                  out
                  (iota width)))
               '()
            (iota height)))
      (print "loaded " (length gates))
      ;; (print "gates: " gates)

      ; current wires state: powered (true) or not (false)
      (define wire-states (fold (lambda (ff i) ; array with unpowered wires
               (put ff i #f))
            {}
            (iota wires-count 1)))
      (print "wire-states: " wire-states)

      (define padding (+ (* width 3) (mod width 4))) ; align to the 4 bytes (for BITMAP)

      {
         'wires wires ; битовая карта с номерами проводов
         'wires-count wires-count ; и их количество
         'wire-states wire-states ; состояние проводов (под напряжением или нет)
         'gates gates ; вентили (от какого провода к какому)
         'color-table color-table ; таблица цветов

         'bitmap board ; оригинальные цвета
         'width width 'height height 'padding padding
         'texture (make-bytevector (* padding height) 0) ; заливка черным
      })

   (define (board-texture board)
      (define wires (board 'wires)) ; дорожки
      (define wire-states (board 'wire-states))
      (define bitmap (board 'bitmap)) ; цвета
      (define color-table (board 'color-table))

      (define width (board 'width))
      (define padding (board 'padding))
      (define height (board 'height))
      (define texture (board 'texture))
      (for-each (lambda (j)
            (for-each (lambda (i p)
                  (define wire (rref wires i j))
                  (when wire
                     (define power (wire-states wire))
                     (define color (color-table (rref bitmap i j)))
                     (define rgb (if power (ref color 2) (ref color 1)))
                     (set-ref! texture (+ p 2) (ref rgb 1))
                     (set-ref! texture (+ p 1) (ref rgb 2))
                     (set-ref! texture (+ p 0) (ref rgb 3))))
               (iota width 1)
               (iota width (* (- height j) padding) 3))) ; перевернем для BITMAP
         (iota height 1))
      
      (define image-size (+ (size texture) 54)) ; 54 is a BMP headres size

      ; create BMP image (todo: move to xpm3->board
      (bytevector-append (list->bytevector (list
         ; Bitmap File Header
         #\B #\M ; magic
         (band #xFF (>> image-size 0)) (band #xFF (>> image-size 8)) (band #xFF (>> image-size 16)) (band #xFF (>> image-size 24))
         0 0 0 0 ; application
         #x36 #x00 #x00 #x00 ; image start (54)
         ; DIB Header
         #x28 #x00 #x00 #x00 ; BITMAPINFOHEADER
         (band #xFF (>> width 0)) (band #xFF (>> width 8)) (band #xFF (>> width 16)) (band #xFF (>> width 24))
         (band #xFF (>> height 0)) (band #xFF (>> height 8)) (band #xFF (>> height 16)) (band #xFF (>> height 24))
         #x01 #x00 ; color planes
         #x18 #x00 ; bits per pixel
         #x00 #x00 #x00 #x00 ; compression method
         #x00 #x00 #x00 #x00 ; image size (not required for the RGB)
         #x00 #x00 #x00 #x00 ; horizontal ppm
         #x00 #x00 #x00 #x00 ; vertical ppm
         #x00 #x00 #x00 #x00 ; number of colors (0 means default)
         #x00 #x00 #x00 #x00 ; number of important colors (0 means all)
      )) texture))

   (define (board-energize board i j ttl) ; starting from 1, ttl 0 means forever
      (define wires (board 'wires)) ; дорожки
      (define wire-states (board 'wire-states))
      (define wire (rref wires i j))
      (if wire
         (put! wire-states wire ttl)))

      ;; (define wire-stateS (fold (lambda (ff i)
      ;;       (put ff i #f))
      ;;    {}
      ;;    (iota wires-count 1)))
      ;; (define TTL 99) ; 0 for "forever"
      ;; (define wire-states (fold (lambda (ff j)
      ;;       (fold (lambda (ff i)
      ;;             (if (eq? (rref board i j) power-color)
      ;;                (put ff (rref wires i j) TTL) ; power wire with "power" cell
      ;;             else
      ;;                ff))
      ;;          ff (iota width 1)))
      ;;    (fold (lambda (ff i) ; array with unpowered wires
      ;;          (put ff i #f))
      ;;       {}
      ;;       (iota wires-count 1))
      ;;    (iota height 1)))

   (define (board-simulate board)
      (define gates (board 'gates))
      (define wire-states (board 'wire-states))
      ; find signals
      (define signals
         (fold (lambda (ff gate)
                  (define from (ref gate 1))
                  (define to (ref gate 2))
                  (if (not (wire-states from #f))
                     (put ff to #true)
                  else
                     ff))
            {}
            gates))
      ; update wires
      (for-each (lambda (wire)
            (define state (wire-states wire #f))
            (if (natural? state) ; wire manually powered
               (put! wire-states wire (- state 1))
            else
               (put! wire-states wire (signals wire #false))))
         (keys wire-states)))

))