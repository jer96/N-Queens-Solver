;Jeremiah Bill
;CSC 173 
;Scheme 3-4
;11/30/2015

;Importing random library in order to help in the defining the minimum conflict algorithm
(#%require (only racket/base random))
;Defining counters to count the number of steps for each algorithm
(define bt 0);counter for backtracking
(define incbt (lambda () (set! bt (+ bt 1))))
(define mc 0);counter for minimum conflict
(define incmc (lambda () (set! mc (+ mc 1))))

;Defining the backtracking algorithm 
;Back tracking works by placing queens on the board one at a time and if placing a queen down results in an attack 
;it will take the preceeding queen and place in it in a new spot and continue on throughout the rest of the board
;takes in boolean and vector size
;makes a vector filled with -1 to signify empty columns the takes that vector and uses it within the backtracking method
;if the boolean is true the graphic representation of the board will be printed otherwise the vector representaiton will be printed
;in both cases the number of steps is shown
(define (nq-bt bool n)
  
  (let ((v (make-vector n -1)))
    (cond 
      ((eq? bool #f) (display (BackTrack v 0 0)));displays the vector representation
      ((eq? bool #t) (printBoard (BackTrack v 0 0))(display (BackTrack v 0 0)))));displays both the vector and the graphic representation
  (newLine)
  (display "Number of steps: " )(display bt)
  (set! bt 0))

;Defining backtracking conflict method which will return true if based on the input row column vector and arbitrary iter values placing a queen down will result in some type of attack
;The empty spaces in the vector is handle by just skipping over the -1's in the vector by making the conditon for conflicts dependent on searching the indices where there does not exist a -1
;Attacks are handle by the nature of the vector ensuring that each index has a different value, secondly the method begins by checking if the incrementer (iter) has incremented 
;secondly attacks are defined by queens placed in the same row therefore if the attempted value has the row value as a previous queen the function will return true stating that there does exist conflicts
;lastly diagonal conflicts are found by simply checking if the change in the x values(rows) is equal to the change in y values (columns)
;therefore the absolute value can be taken from the previous row minus the current row and same with previous column minus current column and if the absolute values are equivalent then there exists attacks and then function returns true
;otherwise the function moves the next columns
(define (BackTrackCon? row col iter vec)
      (cond 
      ((> iter col) #f);if counter (iter) is greater than the current column return false ie no conflicts
      ((= row (vector-ref vec iter)) #t);if the current row is equal to a row in another column the return false
      ((and (not (= (vector-ref vec iter) -1)) (= (abs (- row (vector-ref vec iter))) (abs (- col iter))) )#t );skip over -1's ie skip over un filled columns and check if the diagonals casuse attacks if so return true
      (else (BackTrackCon? row col (+ iter 1) vec))));otherwise continue through the vector 

;Recursive helper method to actually solve via backtracking
;Method takes in the vector and the initial row and col values
;base case returns the vector once the columns have been recursed through to the end of the board/vector
;Defines a save variable which basically saves the index of the preceeding queen when the next queen placed results in an attack
;otherwise if there exists no conflicts when placing down the current queen, the value at that column can be set to the current row value and the function can move on to the next value on the vector/board
;if neither one of these conditions is satisfied then the funcition will move on to the next potential row value 
(define (Backtrack vec col row)
  (define saveVar 0);defining variable to change if the I need to backtrack
  (if (= col (vector-length vec)) vec ;if the function has reached the end of the vector return the vector
  (cond 
    ((= row (vector-length vec))  (incbt)(set! saveVar (vector-ref vec (- col 1))) (vector-set! vec (- col 1) -1) (BackTrack vec (- col 1) (+ saveVar 1)));backtracking step changes saveVar to the value of the previous columns row then set the column to -1 and recursively call the method in order to try to place the queen again 
    ((not (BackTrackCon? row col 0 vec)) (incbt)  (vector-set! vec col row) (Backtrack vec (+ col 1) 0) ); if there are no conflicts then set the value of the row at the current column then continue to the next column
    (else (BackTrack vec  col (+ row 1))))));otherwise just continue to the next row 

;Defining minimum conflicts algorithm
;Minimum conflicts basically generates a board then proceeds to take that given board and moves the queens accordingly such that one the algorithm has finished results in
;a board which satsifies the nqueens problem
;So my method begins with recursively generating a random board of size n using the random number generator library function to generate random numbers within the bounds of n for each index
(define (nq-mc bool n)
  (let ((randVec (randBoard 0 (make-vector n))) (randCol (random n)))
    (cond 
      ((eqv? bool #f) (display (minimumConflictRec randVec 0 randCol)))
      ((eqv? bool #t) (printBoard (minimumConflictRec randVec 0 randCol)) (display (minimumConflictRec randVec 0 randCol)))
      )
    (newLine)
    (display "Number of steps: ")(display mc)
    (set! mc 0)))

;Helper method which actually executes the minimum conflict algorithm
;checks if the board is in a goal state and if so returns the board
;checks if the board is in a local minimum if the count (number of executions within the program has exceed a large number as shown i use 15000 and if so generates another random board and trys to solve that board
;otherwise checks if there exist conflicts if so finds the minimum row location with selecting a random if there exist a tie between the minimum row locations and changes the location of the queen
;then continues to another random column until the board has been solved
(define (minimumConflictRec vec count col)
  (cond
    ( (goalState? vec 0)  vec);base case
    ( (> count 15000)  (minimumConflictRec (randBoard 0 vec) 0 col));local minimum case
    ((> (minConConflict (vector-ref vec col) col 0 0 vec) 0 ) (incmc) (vector-set! vec col (breaktie vec col (findMinRow 0 0 0 0 col vec))) (minimumConflictRec vec (+ count 1) (random (vector-length vec)))) ; minimum conflict step where the queen is moved to a place where there are the least conflicts and if there exist a tie chooses a minimum row location at random and moves on to the next random column
    (else (minimumConflictRec vec (+ count 1) (random (vector-length vec))))));other wise move on to another column


;Returns the number of conflicts per row and col in the vector 
; method which finds conflicts is an adaptation to the same method used for finding conflicts in the backtracking approach 
;the exception is that instead of returning a boolean value the program returns a number of conflicts for each row col and vector input into the function
;to implement this one more parameter was added to represent the count wich is the return value of the function which when called is always 0
;So the function uses the same logic as the previous implemented version except the base case is that once the value of iter has reached the end of the vector count is returned
;to continue count is only incremented when there exists an attack between the current queen and the previous placed queen
;therefore count is incremented when the function finds an attack either horizontally or diagonally 
;by result the total number of conflicts per queen is calculated which is used further in the minimum conflict algorithm
(define (minConConflict row col iter count vec)
      (cond 
      ((= iter (vector-length vec)) count);base case
      ((and (not (= col iter)) (= row (vector-ref vec iter))) (minConConflict row col (+ iter 1) (+ count 1) vec));ensures the current column does not equal column i am currently checking and if the row is equal to a row in anotherr column then increment the count and continue through the rest of the vector
      ((and (not (= col iter))(and (not (= (vector-ref vec iter) -1)) (= (abs (- row (vector-ref vec iter))) (abs (- col iter))))) (minConConflict row col (+ iter 1)(+ count 1) vec));same as previous step except is checking for diagonals
      (else  (minConConflict row col (+ iter 1) count vec))));else just move throughout the vector column by column
   
;the solved method is used to figure out if the state of the current bored is a goal state or "solved"
;this method only takes in the vector and a column value
;therefore the base case is if the column we are checking is the end of the vector then the board is solved
;secondly if the number of conflicts of  queens in the current column is greater than zero ie if any attacks are found the board has NOT been solved
;otherwise just increment through the rest of the vector
(define (goalState? vec col)
  (cond
    ((= col (vector-length vec)) #t);base case returns true if it has reached the end of the vector
    ((> (minConConflict (vector-ref vec col) col 0 0 vec) 0) #f);checks if the there are conflicts(attacks in each column and if so the board is not in its goal state
    (else (goalState? vec (+ col 1)))));else just move through the vector (columns of the board)

;Random board generator
;helper method for minimum conflict algorithm
;Generates random board given an iterator and a vector 
;Base case checks if the iterator is equal to the length of the vector 
;then proceeds to recurse through setting the index of the vector at a the iterator to a random number within the bounds of the length of the vector 
;then moves on to the next index in the vector, therefore this method creates takes a vector creates a random board layout with (vector-length) queens placed on the board
(define (randBoard iter v)
    (cond  
      ((= iter (vector-length v)) v)
      (else (vector-set! v iter (random (vector-length v)))(randBoard (+ iter 1) v))))

;Minimum row finder
;This method will take in values for the minimum row, current row , number of conflicts, a flag , column and vector 
;The logic of this method is to find where the minimum row of a given column is so therefore if the row has made it to the end of the row return minrow
;otherwise if the number of conflicts exceeds the number of conflicts on the current row set the new number of conflicts to the to that rows conflicts and change minrow to the current row and search through the rest of the rows in the column
(define(findMinRow minRow row numConflicts flag col vec )
    (cond
      ((= row (vector-length vec)) minRow);base case
      ((= flag 0) (set! minRow row) (set! numConflicts (minConConflict row col 0 0 vec)) (findMinRow minRow (+ row 1) numConflicts (+ flag 1) col vec));sets initial value for the minimum value to be compared to all other values
      ((> numConflicts (minConConflict row col 0 0 vec)) (set! minRow row) (set! numConflicts (minConConflict row col 0 0 vec))(findMinRow minRow (+ row 1) numConflicts flag col vec));if a new minimum is found do the following changes
      (else (findMinRow minRow (+ row 1) numConflicts flag col vec))));otherwise move on to the next row

;break tie method
;used to break ties when there a multiple rows that have the minimum number of conflicts
;randomly breaks tie between given value
(define (breakTie vec col minrow)
  (let ( (rand (random (vector-length vec))) ); defines random number within the bounds of the vector length
  (cond
    ((= (minConConflict minrow col 0 0 vec) (minConConflict rand col 0 0 vec)) rand);if the number of conflicts of the minimum row is equivalent to the randomly generated value use the randomly generated value
    (else breakTie vec col minrow))));else continue to attempt to break the tie 

;Board printer used to print out the board representation of any vector of any length
;defines two nested iterative loops then proceeds to go until each loop has reached the end of the vector this ensures the board is printed correctly such that it is a N X N board where n is the vector length and number of queens
;checks if the position at j in the vector is equal to the iterator i which will in turn print out every instance of a queen in a given column
;otherwise just print out 0s for empty squares
;and print a new line after every iteration of the first loop which just allows the board to be alot more readable
(define (printBoard vector)
  (do ((i 0 (+ i 1)))
        ((= i (vector-length vector) ))
      (do ((j 0 (+ j 1)))
        ((= j (vector-length vector) ))
        (if (eq? (vector-ref vector j) i)
            (display "1 ")
            (display "0 ")))
    (newLine)))
    
    
    





    
  
