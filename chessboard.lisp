;;; chessboard.lisp

(in-package #:cl-chess)

(defparameter +WHITE-SYMS+ '(WP WK WB WR WK WQ))
(defparameter +BLACK-SYMS+ '(BP BK BB BR BK BQ))
(defparameter +MODES+ '(analysis engine normal three-check))

(defclass bitboard ()
  ;; A bitboard is a 64 bit integer (8 by 8! Perfect to represent squares)
  ((
    data
    :initarg :data
    :type (signed-byte 64)
    :accessor data
    :initform (error "Bitboard must have a 64bit integer"))
   (
    symbol
    :initarg :symbol
    :accessor symbol
    :initform (error "Bitboard must have a symbol"))
   (
    player
    :initarg :player
    :accessor player
    :initform nil)))

(defclass chessboard ()
  ((
    bitboards
    :initarg :bitboards
    :accessor bboards
    :initform (error "Bitboards must be initialized on board")
    )
   (turn
    :initarg :turn
    :accessor turn
    :initform 'white)
   (move-no
    :initarg :move-no
    :accessor move-no
    :initform 1)
   (history
    :initarg :history
    :accessor history
    :initform nil)))


(defmethod initialize-instance :after ((board chessboard) &key)
  ;; Lisp type system sucks so we're using CLOS to enforce type safety
  (assert (= (length (slot-value board 'bitboards)) 12))
  (assert (every (lambda (bitboard) (typep bitboard 'bitboard))
                 (slot-value board 'bitboards))))


(defun range (first &optional (second nil) (step 1))
  ;; Still not in standard library by the way
  (macrolet ((for (second word first)
               `(loop :for x :from ,second ,word ,first :by step
                      :collect x)))
    (cond ((and second (> second first)) (for first to second))
          (second (for first downto second))
          (t (for 0 to first)))))

(defun bb-map (body)
  (mapcar (lambda (x)
            (mapcar (lambda (y)
                      (let ((square (+ x y)))
                         (apply body (list square))))
                    (range 0 7)))
          (range 56 0 8)))

(defun make-int64-from-xs (xs &optional (initdata 0))
  "Fill a board with data in the following squares..."
  (if (car xs)
      (make-int64-from-xs (cdr xs) (fill-square (car xs) initdata))
      initdata))                        

(defun fill-square (x &optional (initdata 0))
  "Adds data to a square on a bitboard"
  (logior (+ (ash 1 x) initdata) initdata))

(defun empty-square (x initdata)
  (- initdata (ash 1 x)))

(defun make-bitboards ()
  (list (make-instance 'bitboard :data (make-int64-from-xs
                                        '(8 9 10 11 12 13 14 15)) :symbol 'WP :player 'white)
        (make-instance 'bitboard :data (make-int64-from-xs
                                        '(1 6))
                                 :symbol 'WN :player 'white)
        (make-instance 'bitboard :data (make-int64-from-xs
                                        '(2 5)) :symbol 'WB :player 'white)
        (make-instance 'bitboard :data (make-int64-from-xs
                                        '(3)) :symbol 'WQ :player 'white)
        (make-instance 'bitboard :data (make-int64-from-xs
                                        '(4)) :symbol 'WK :player 'white)
        (make-instance 'bitboard :data (make-int64-from-xs
                                        '(0 7)) :symbol 'WR :player 'white)
        (make-instance 'bitboard :data (make-int64-from-xs
                                        '(48 49 50 51 52 53 54 55)) :symbol 'BP :player 'black)
        (make-instance 'bitboard :data (make-int64-from-xs
                                        '(57 62)) :symbol 'BN :player 'black)
        (make-instance 'bitboard :data (make-int64-from-xs
                                        '(58 61)) :symbol 'BB :player 'black)
        (make-instance 'bitboard :data (make-int64-from-xs
                                        '(59)) :symbol 'BQ :player 'black)
        (make-instance 'bitboard :data (make-int64-from-xs
                                        '(60)) :symbol 'BK :player 'black)
        (make-instance 'bitboard :data (make-int64-from-xs
                                        '(56 63)) :symbol 'BR :player 'black))
  )

(defun make-chessboard ()
  (make-instance 'chessboard :bitboards (make-bitboards)
                             :history
                             (list (list
                                    (cons :data (make-bitboards))
                                    (cons :move-no 1)
                                    (cons :turn 'white)))
))

(defmethod format-bitboard ((bb bitboard))
  (bb-map (lambda (x)
            (let ((filled (if (< 0 (logand (slot-value bb 'data) (expt 2 x))) 1 0)))
              (if (= 0 (mod (+ x 1) 8))
                  (format nil "~A ~C" filled #\linefeed)
                  (format nil "~A" filled))))))

(defmethod print-object ((bb bitboard) stream)
  (print-unreadable-object (bb stream :type t)
    (with-slots (symbol player data) bb
      (format stream "player: ~A;~C symbol: ~A;~C data: ~A"
              player #\linefeed symbol #\linefeed (format-bitboard bb)))))

(defun find-piece-on-square (square bitboardxs)
  "Finds a piece in a list of bitboards and returns symbol for piece"
  (if (null bitboardxs)
      '0
      (if (not (= 0 (logand (slot-value (car bitboardxs) 'data) (ash 1 square))))
          (slot-value (car bitboardxs) 'symbol)
          (find-piece-on-square square (cdr bitboardxs))))
  )

(defmethod format-chessboard ((cb chessboard))
  (bb-map (lambda (x)
            (let ((square-sym (find-piece-on-square x (slot-value cb 'bitboards))))
              (if (= 0 (mod (+ 1 x) 8))
                  (format nil "~A~C" square-sym #\linefeed)
                  (format nil "~A" square-sym))))))

(defmethod print-object ((cb chessboard) stream)
  "Not implemented"
  (print-unreadable-object (cb stream :type t)
    (format stream "State of board: ~C ~A" #\linefeed (format-chessboard cb))))

(defmethod player-piece-p ((cb chessboard) sym)
  (or (and (equal (slot-value cb 'turn) 'white)
           (member sym +WHITE-SYMS+))
      (and (equal (slot-value cb 'turn) 'black)
           (member sym +BLACK-SYMS+))))
