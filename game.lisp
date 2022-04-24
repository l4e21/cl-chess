(in-package #:cl-chess)

(defmethod make-move (sq1 sq2 (cb chessboard))
  (let* ((sym (find-piece-on-square sq1 (slot-value cb 'bitboards)))
         (attack-squares-list (lookup-attack-squares sq1 sym cb)))
    (if (member sq2 attack-squares-list)
        (let* ((sq2-occupancy (find-piece-on-square sq2 (slot-value cb 'bitboards)))
               (same-piece (player-piece-p cb sq2-occupancy)))
          (cond ((equal sq2-occupancy 0)
                 (execute-move cb sq1 sq2 sym nil))
                (same-piece
                 (error "Can't move on top of your own piece"))
                ((not same-piece)
                 (execute-move cb sq1 sq2 sym sq2-occupancy))
                (t
                 (error "Unhandled case"))))
        (error "Not a possible move that you can make"))))

(defmethod execute-move ((cb chessboard) sq1 sq2 sym occupant)
  "Next we need to make the move and then determine if it's valid according to checks and captures. Pawn diagonals are still an edge case at this point right now?"
  (print (slot-value cb 'history))
  (let* ((sq1-bb (first (remove-if-not (lambda (bb) (equal sym (slot-value bb 'symbol))) (slot-value cb 'bitboards))))
         (sq2-bb (if occupant
                     (first (remove-if-not (lambda (bb) (equal occupant (slot-value bb 'symbol)))
                                           (slot-value cb 'bitboards)))
                     nil))
         (untouched-bitboards (remove-if (lambda (bb)
                                           (member (slot-value bb 'symbol)
                                                   (list occupant sym)))
                                         (slot-value cb 'bitboards)))
         (new-sq2-bb (if sq2-bb
                         (make-instance 'bitboard :data (empty-square sq2
                                                                      (slot-value sq2-bb 'data))
                                                  :symbol (slot-value sq2-bb 'symbol)
                                                  :player (slot-value sq2-bb 'player))
                         nil))
         (new-sq1-bb (make-instance 'bitboard
                                    :data (empty-square sq1
                                                        (fill-square sq2
                                                                     (slot-value sq1-bb 'data)))
                                    :symbol (slot-value sq1-bb 'symbol)
                                    :player (slot-value sq1-bb 'player)))
         (new-bitboards (if new-sq2-bb
                            (append (append untouched-bitboards (list new-sq2-bb))
                                    (list new-sq1-bb))
                            (append untouched-bitboards (list new-sq1-bb))))
         (new-cb (make-instance 'chessboard
                                :bitboards new-bitboards
                                :turn (if (equal (slot-value cb 'turn) 'white)
                                          'black
                                          'white)
                                :move-no (+ 1 (slot-value cb 'move-no))
                                :history (append (slot-value cb 'history)
                                                 (list (list
                                                        (cons :data new-bitboards)
                                                        (cons :move-no (+ 1 (slot-value cb 'move-no)))
                                                        (cons :turn (if (equal (slot-value cb 'turn)
                                                                               'white)
                                                                        'black
                                                                        'white))))))))
    (if (check-p new-cb (slot-value cb 'turn))
        (error "Can't move there as you would be in check!")
        new-cb)))

(defun other-player (player)
  (if (equal player 'white)
      'black
      'white))

(defmethod find-king-square ((cb chessboard) player)
  (let ((king-board
          (remove-if-not (lambda (board)
                           (and (equal (slot-value board 'player) player)
                                (or (equal (slot-value board 'symbol) 'WK)
                                    (equal (slot-value board 'symbol) 'BK))))
                         (slot-value cb 'bitboards))))
    (reduce (lambda (acc square)
              (if (not (equal (find-piece-on-square square king-board) '0))
                  square
                  acc))
            (range 63) :initial-value nil)))

(defmethod find-opponent-attack-squares ((cb chessboard) player)
  (let* ((other-player-boards
           (remove-if (lambda (board)
                        (equal (slot-value board 'player) player)) (slot-value cb 'bitboards))))
    (reduce (lambda (acc square)
              (let ((symsquare (find-piece-on-square square other-player-boards)))
                (if (not (equal symsquare 0))
                    (append acc (lookup-attack-squares square symsquare cb))
                    acc))) (range 64) :initial-value '())))
         
                  
(defmethod check-p ((cb chessboard) player)
  "Determine whether the piece is in check by getting the squares under the attack of all the pieces of the other player and the king position"
  ;; We should use lookup-attack-squares to determine the checks 
  (let* ((opponent-attack-squares (find-opponent-attack-squares cb player))
         (king-square (find-king-square cb player)))
    (if (member king-square opponent-attack-squares)
        t
        nil)))
