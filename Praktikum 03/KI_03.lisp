(defconstant all-directions '(-10 -9 -8 -1 10 9 8 1))

(defconstant empty 0 "An empty square")
(defconstant black 1 "A black piece")
(defconstant white 2 "A white piece")
(defconstant outer 3 "Marks squares outside the 6x7 board")

(deftype piece () `(integer ,empty ,outer))

(defun my-name-of (piece) (char ".@O?" piece))

(defun opponent (player) (if (eql player black) white black))

(deftype board () '(simple-array piece (72)))

(defun bref (board square) (aref board square))
(defsetf bref (board square) (val) 
  `(setf (aref ,board , square) ,val))

(defun copy-board (board)
  (copy-seq board))

(defconstant all-squares
  (loop for i from 10 to 63 when (<= 2 (mod i 9) 8) collect i))

;;;ALL-SQUARES

(defconstant all-columns
  (loop for i from 1 to 9 collect i))

;;;ALL-COLUMNS

(defun initial-board ()
  "Return a board, empty except for four pieces in the middle."
  ;; Boards are 72-element vectors, with elements 8-49 used,
  ;; and the others marked with the sentinel OUTER. Initially
  ;; all squares are empty.
  (let ((board (make-array 72 :element-type 'piece
                           :initial-element outer)))
    (dolist (square all-squares)
      (setf (bref board square) empty))
      board))

;;;(print-board (initial-board))
;;;(initial-board)

(defun count-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (- (count player board)
     (count (opponent player) board)))

(defun valid-p (col)
  "Valid moves are numbers in the range 2-8."
  (and (integerp col) (<= 2 col 8)))

;;;(valid-p 1)
;;;(valid-p 2)
;;;(valid-p 8)
;;;(valid-p 9)

(defun legal-p (col board)
  "A Legal move must be into an empty square."
  (eql (bref board (+ col 9)) empty))

;;; (legal-p 1 (initial-board))
;;; (legal-p 2 (initial-board))
;;; (legal-p 8 (initial-board))
;;; (legal-p 9 (initial-board))

(defvar *last-move* 0 "The square of the last move that was played")

(defun make-move (col player board)
  "Update board to reflect move by player"
  ;; First make the move, then make any flips
  (let ((square (top-empty-square board col)))
    (setf (bref board square) player)
    (setf *last-move* square)
    board))

;;;(make-move 2 black (initial-board))
;;;(make-move 2 white (make-move 2 black (initial-board)))
;;;(make-move 4 white (make-move 3 black (make-move 4 white (make-move 4 black (initial-board)))))
;;;(print-board (make-move 4 white (make-move 3 black (make-move 4 white (make-move 4 black (initial-board))))))

(defun top-empty-square (board square)
  (if (eql (bref board (+ square 9)) empty)
      (top-empty-square board (+ square 9))
      square))

;;;(top-empty-square (initial-board) 2)  

(defun next-to-play (board previous-player)
  "Compute the player to move next, or NIL if nobody can move."
  (let ((opp (opponent previous-player)))
    (cond ((any-legal-move? board) opp)
          (t nil))))

;;;(next-to-play (initial-board) black)

(defun any-legal-move? (board)
  "Does player have any legal moves in this position?"
  (some #'(lambda (col) (legal-p col board))
        all-columns))

;;;(any-legal-move? (initial-board))

(defun random-strategy (player board)
  "Make any legal move."
  (if (null player) nil (random-elt (legal-moves board))))

;;;(random-strategy black (initial-board))

(defun legal-moves (board)
  "Returns a list of legal moves for player"
  ;;*** fix, segre, 3/30/93.  Was remove-if, which can share with all-squares.
  (loop for col in all-columns
     when (legal-p col board) collect (1- col)))

;;; (legal-moves (initial-board))

(defun maximize-difference (player board)
  "A strategy that maximizes the difference in pieces."
  (funcall (maximizer #'count-difference) player board))

;;(maximize-difference black (initial-board))

(defun maximizer (eval-fn)
  "Return a strategy that will consider every legal move,
  apply EVAL-FN to each resulting board, and choose 
  the move for which EVAL-FN returns the best score.
  FN takes two arguments: the player-to-move and board"
  #'(lambda (player board)
      (let* ((moves (legal-moves board))
             (scores (mapcar #'(lambda (move)
				 (funcall
				  eval-fn
				  player
				  (make-move move player
					     (copy-board board))))
                             moves))
             (best  (apply #'max scores)))
        (elt moves (position best scores)))))

(defun maximize-single-row-length (player board)
  "A strategy that maximizes the difference in pieces."
  (funcall (maximizer #'single-row-length) player board))

;;; (maximize-single-row-length black (initial-board))

(defun maximize-multiple-row-length (player board)
  "A strategy that maximizes the difference in pieces."
  (funcall (maximizer #'multiple-row-length) player board))

;;; (maximize-multiple-row-length black (initial-board))

(defconstant winning-value 4)
(defconstant losing-value -4)

(defun largest-row-of-player (player board)
	(let ((largest-row 0))
		(dolist (square all-squares largest-row)
			(if (and (has-player-square? player board square) (> (largest-row-of-square board square) largest-row))
				(setf largest-row (largest-row-of-square board square))))))

(defun 4-in-a-row? (board square)
  (>= (largest-row-of-square board square) 4))

;;;(4-in-a-row? (make-move 2 black (make-move 1 black (initial-board))) 56)
;;;(4-in-a-row? (make-move 4 black (make-move 3 black (make-move 2 black (make-move 1 black (initial-board))))) 56)
;;;(4-in-a-row? (make-move 4 black (make-move 3 black (make-move 2 black (make-move 1 black (initial-board))))) 58)

(defun largest-row-of-square (board square)
  (let ((player (bref board square))
	(current-row 1)
	(largest-row 1))
    (loop for direction from 0 to 3 do
	 (setq current-row (+ 1 (neighbors-count player board square (elt all-directions direction))
			    (neighbors-count player board square (elt all-directions (+ direction 4)))))
	 (if (> current-row largest-row)
	     (setq largest-row current-row)))
    largest-row))

;;;(largest-row-of-square (make-move 1 black (initial-board)) 56)
;;;(largest-row-of-square (make-move 4 black (make-move 3 black (make-move 2 black (make-move 1 black (initial-board))))) 58)

(defun neighbors-count (player board square direction)
  (if (null (member (+ square direction) all-squares)) 0
  (cond
    ((has-player-square? player board (+ square direction))
     (1+ (neighbors-count player board (+ square direction) direction)))
    (t 0))))

;;;(neighbors-count black (make-move 3 black (make-move 2 black (initial-board))) 57 1)
;;;(neighbors-count black (make-move 4 black (make-move 3 black (make-move 2 black (initial-board)))) 57 1)
;;;(neighbors-count black (initial-board) 62 10)

(defun has-player-square? (player board square)
  (eql (bref board square) player))

(defun get-row-length-weight (length)
	(cond
		((eql length 1) 1)
		((eql length 2) 5)
		((eql length 3) 20)
		((> 0 length 4) most-positive-fixnum)
		(t 0)))

(defun single-row-length (player board)
	(get-row-length-weight (largest-row-of-player player board)))

;;; (single-row-length black (make-move 3 black (initial-board)))

(defun multiple-row-length (player board)
  (let ((sum 0))
    (dolist (square all-squares)
      (if (has-player-square? player board square) (incf sum (get-row-length-weight (largest-row-of-square board square)))))
    sum))

;;; (multiple-row-length black (make-move 3 black (initial-board)))
;;; (multiple-row-length black (make-move 3 black (make-move 3 black (initial-board))))

(defun final-value (player board)
  "Is this a win, loss, or draw for player?"
  (cond
  	((>= (largest-row-of-player player board) 4 winning-value))
    ((>= (largest-row-of-player (opponent player) board) 4 losing-value))
    (t 0)))

(defun minimax (player board ply eval-fn)
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching PLY levels deep and backing up values."
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (legal-moves board)))
        (if (null moves)
                (final-value player board)
            (let ((best-move nil)
                  (best-val nil))
              (dolist (move moves)
                (let* ((board2 (make-move move player
                                          (copy-board board)))
                       (val (- (minimax
                                 (opponent player) board2
                                 (- ply 1) eval-fn))))
                  (when (or (null best-val)
                            (> val best-val))
                    (setf best-val val)
                    (setf best-move move))))
              (values best-val best-move))))))

(defun minimax-searcher (ply eval-fn)
  "A strategy that searches PLY levels and then uses EVAL-FN."
  #'(lambda (player board)
      (multiple-value-bind (value move)
          (minimax player board ply eval-fn) 
        (declare (ignore value))
        move)))

(defun alpha-beta (player board achievable cutoff ply eval-fn)
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching PLY levels deep and backing up values,
  using cutoffs whenever possible."
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (legal-moves board)))
        (if (null moves)
	    (final-value player board)
            (let ((best-move (first moves)))
              (loop for move in moves do
                (let* ((board2 (make-move move player
                                          (copy-board board)))
                       (val (- (alpha-beta
                                 (opponent player) board2
                                 (- cutoff) (- achievable)
                                 (- ply 1) eval-fn))))
                  (when (> val achievable)
                    (setf achievable val)
                    (setf best-move move)))
                until (>= achievable cutoff))
              (values achievable best-move))))))

(defun alpha-beta-searcher (depth eval-fn)
  "A strategy that searches to DEPTH and then uses EVAL-FN."
  #'(lambda (player board)
      (multiple-value-bind (value move)
          (alpha-beta player board losing-value winning-value
                      depth eval-fn) 
        (declare (ignore value))
        move)))

(defun human (player board)
  "A human player for the game"
  (format t "~&~c to move ~a: " (my-name-of player)
          (legal-moves board))
  (read))

;;;(human black (initial-board))

(defvar *move-number* 1 "The number of the move to be played")

(defun play-game (bl-strategy wh-strategy &optional (print t))
  "Play a game.  Return the score, where a positive
  difference means black, the first player, wins."
  (let ((board (initial-board))
	(last-player black))
    (catch 'game-over
      (loop for *move-number* from 1
            for player = black then (next-to-play board player)
            for strategy = (if (eql player black) 
                               bl-strategy
                               wh-strategy)
            until (null player)
	 do
	   (setq last-player player)
	   (get-move strategy player board print)))
      (when print
        (format t "~&~&The game is over and player '~c' has won :)~&Final result: " (my-name-of last-player))
        (print-board board))
      (count-difference black board)))

;;;(play-game #'random-strategy #'random-strategy)
;;;(play-game #'human #'random-strategy)
;;;(play-game #'random-strategy #'human)

(defvar *board* (initial-board) "A copy of the game board")

(defun get-move (strategy player board print)
  "Call the player's strategy function to get a move.
  Keep calling until a legal move is made."
  ;; Note we don't pass the strategy function the REAL board.
  ;; If we did, it could cheat by changing the pieces on the board.
  (when print (print-board board))
  (let* ((col (1+ (funcall strategy player (replace *board* board)))))
    (cond
      ((eq col 'resign)
       (THROW 'game-over (if (eql player black) losing-value winning-value)))
      ((and (valid-p col) (legal-p col board))
       (let ((square (top-empty-square board col)))
       (when print
         (format t "~&~c moves to ~a." 
                 (my-name-of player) square))
       (make-move col player board)
       (if (4-in-a-row? board square)
	   (THROW 'game-over winning-value))))
      (t (warn "Illegal move: ~a" col)
         (get-move strategy player board print)))))

(defun print-board (&optional (board *board*))
  "Print a board, along with some statistics."
  ;; First print the header and the current score
  (format t "~2&    1 2 3 4 5 6 7")
  ;; Print the board itself
  (loop for row from 1 to 6 do
        (format t "~&  ~d " row)
        (loop for col from 1 to 7
              for piece = (bref board (+ 1 col (* 9 row)))
              do (format t "~c " (my-name-of piece)))))
            
;;;(print-board (initial-board))

(defun random-game-series (strategy1 strategy2 n-pairs &optional (n-random 10))
  "Play a series of 2*n games, starting from a random position."
  (game-series
    (switch-strategies #'random-strategy n-random strategy1)
    (switch-strategies #'random-strategy n-random strategy2)
    n-pairs))

;;;(random-game-series #'random-strategy #'random-strategy 10)

(defun switch-strategies (strategy1 m strategy2)
  "Make a new strategy that plays strategy1 for m moves,
  then plays according to strategy2."
  #'(lambda (player board)
      (funcall (if (<= *move-number* m) strategy1 strategy2)
               player board)))

(defun game-series (strategy1 strategy2 n-pairs)
  "Play a series of 2*n-pairs games, swapping sides."
  (let ((scores
          (loop repeat n-pairs
             for random-state = (make-random-state)
             collect (play-game strategy1 strategy2 nil)
             do (setf *random-state* random-state)
             collect (- (play-game strategy2 strategy1 nil)))))
    ;; Return the number of wins (1/2 for a tie),
    ;; the total of the point differences, and the
    ;; scores themselves, all from strategy1's point of view.
    (values (+ (count-if #'plusp scores)
               (/ (count-if #'zerop scores) 2))
            (apply #'+ scores)
            scores)))

;;;(game-series #'random-strategy #'random-strategy 2)

(defun round-robin (strategies n-pairs &optional (n-random 10) (names strategies))
  "Play a tournament among the strategies.
  N-PAIRS = games each strategy plays as each color against
  each opponent.  So with N strategies, a total of
  N*(N-1)*N-PAIRS games are played."
  (let* ((N (length strategies))
         (totals (make-array N :initial-element 0))
         (scores (make-array (list N N)
                             :initial-element 0)))
    ;; Play the games
    (dotimes (i N)
      (loop for j from (+ i 1) to (- N 1) do 
          (let* ((wins (random-game-series
                         (elt strategies i)
                         (elt strategies j)
                         n-pairs n-random))
                 (losses (- (* 2 n-pairs) wins)))
            (incf (aref scores i j) wins)
            (incf (aref scores j i) losses)
            (incf (aref totals i) wins)
            (incf (aref totals j) losses))))
    ;; Print the results
    (dotimes (i N)
      (format t "~&~a~20T ~4f: " (elt names i) (elt totals i))
      (dotimes (j N)
        (format t "~4f " (if (= i j) '---
                             (aref scores i j)))))))

;;;(round-robin (list #'random-strategy #'random-strategy) 10)

(defun mobility (board)
  "The number of moves a player has."
  (length (legal-moves board)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; auxfns.lisp ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-elt (seq) 
  "Pick a random element out of a sequence."
  (elt seq (random (length seq))))
