(define-library (src main)
   (import (otus lisp)
           (otus random!))
   (export
      load-xpm3
      xpm3-width
      xpm3-height

      xpm3->board
      board-texture

      load-layout
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
            'color-table (cons [32 99 '(98 108 97 99 107)] colors)
         }))

   (define (rref board i j) (ref (ref board j) i))
   (define (ref0 board i j) (ref (ref board (++ j)) (++ i)))

   (define (load-xpm3 source)
      (define file (try-parse xpm3-parser (str-iter source) #f))
      (if file
         (car file)))
   (define (load-layout source)
      (define file (try-parse layout-parser (str-iter source) #f))
      ;; (print file)
      (if file
         (car file)))

   (define (xpm3-width xpm3)
      (xpm3 'width))
   (define (xpm3-height xpm3)
      (xpm3 'height))

   (define (xpm3->board xpm3)
      ; step 1: find all closed circuits
      ; step 2: find all NOT gates and put into separate dictionary
      ; step 3: connect circuits to the games as "in" and "out" sockets

      (define not-a-wire (ref (car (xpm3 'color-table)) 1))
      ; очистим "черные" клеточки (впишем вместо них #f):
      (define board (vector-map (lambda (row)
            (vector-map (lambda (cell)
                  (if (not (eq? cell not-a-wire)) cell))
               row))
         (xpm3 'bitmap)))
      ;; (print "board: " board)

      (define width (xpm3 'width))
      (define height (xpm3 'height))

      (define color-table (fold (lambda (ff array)
            (define index (ref array 1))
            (define color (list->string (ref array 3)))
            (put ff index (cond
               ((string-eq? color "black")  '(  0   0   0))
               ((string-eq? color "red")    '(205  49  49))
               ((string-eq? color "green")  '( 13 188 121))
               ((string-eq? color "yellow") '(229 229  16))
               ((string-eq? color "blue")   '( 36 114 200))
               ((string-eq? color "magenta")'(188  63 188))
               ((string-eq? color "cyan")   '( 17 168 205))
               ((string-eq? color "white")  '(229 229 229))
               ((string-eq? color "gray")   '(189 189 189))
               ((string-eq? color "orange") '(255 160   0))
               ((string-eq? color "None")   '( 17  17  17))
               (else '(255 255 255)))))
         {}
         (xpm3 'color-table)))
      ;; (print "color-table: " color-table)

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

      ; current wires state: powered (true) or not (false)
      (define wire-states (fold (lambda (f i)
            (put f i #f)) ; 
         {}
         (iota wires-count 1)))

      ;; ; power all red wires
      ;; (define power-wire (ref (lref (file 'color-table) 1) 1))
      ;; (for-each (lambda (j)
      ;;       (for-each (lambda (i)
      ;;             (if (eq? (rref board i j) power-wire)
      ;;                (put! wire-states (rref wires i j) TTL)))
      ;;          (iota WIDTH 1)))
      ;;    (iota HEIGHT 1))

      {
         'bitmap board
         'wires wires
         'wires-count wires-count
         'gates gates
         'color-table color-table

         'width width 'height height
      })

   (define (board-texture board)
      (define wires (board 'wires))
      (define color-table (board 'color-table))
      (define bitmap (board 'bitmap))

      (define width (board 'width))  (define height (board 'height))
      (define data (make-bytevector (* width height 3)))
      (for-each (lambda (j)
            (for-each (lambda (i p)
                  (define cell (ref0 bitmap i (- height j 1)))
                  ;; (define wire (rref wires i j))

                  (define color (color-table cell '(0 0 0)))
                  (set-ref! data (+ p 2) (car color))
                  (set-ref! data (+ p 1) (cadr color))
                  (set-ref! data (+ p 0) (caddr color)) )
               (iota width 0)
               (iota width (* j width 3) 3)))
         (iota height 0))
      
      (define image-size (+ (size data) 54))
      (print image-size) (print width) (print height)

      ;; ; TEMP
      ;; (print (base64:encode (append (list
      ;;    ; Bitmap File Header
      ;;    #\B #\M ; magic
      ;;    (band #xFF (>> image-size 0)) (band #xFF (>> image-size 8)) (band #xFF (>> image-size 16)) (band #xFF (>> image-size 24))
      ;;    0 0 0 0 ; application
      ;;    #x36 #x00 #x00 #x00 ; image start (54)
      ;;    ; DIB Header
      ;;    #x28 #x00 #x00 #x00 ; BITMAPINFOHEADER
      ;;    (band #xFF (>> width 0)) (band #xFF (>> width 8)) (band #xFF (>> width 16)) (band #xFF (>> width 24))
      ;;    (band #xFF (>> height 0)) (band #xFF (>> height 8)) (band #xFF (>> height 16)) (band #xFF (>> height 24))
      ;;    #x01 #x00 ; color planes
      ;;    #x18 #x00 ; bits per pixel
      ;;    #x00 #x00 #x00 #x00 ; compression method
      ;;    #x00 #x00 #x00 #x00 ; image size (not required for the RGB)
      ;;    #x00 #x00 #x00 #x00 ; horizontal ppm
      ;;    #x00 #x00 #x00 #x00 ; vertical ppm
      ;;    #x00 #x00 #x00 #x00 ; number of colors (0 means default)
      ;;    #x00 #x00 #x00 #x00 ; number of important colors (0 means all)
      ;; ) (bytevector->list data))))

      ; return BMP image
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
      )) data))
      ;(string-append "data:image/bmp;base64," (base64:encode image)))
      ;; (string-append "data:image/bmp;base64," "xxxxxxx"))

))