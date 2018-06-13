;     ************************************************************
;     * Name:  Kyle Calabro                                      *
;     * Project:  Longana - Lisp Implementation					         *
;     * Class:  CMPS 366 - Organization of Programming Langauges *
;     * Date:  October 24, 2017                                  *
;     ************************************************************

; -----------------Game-Driver Functions-------------------

;***************************************************************************
; Function Name: longana
; Purpose: To start up and initialize the game (the program itself)
; Parameters: none
; Return value: T
; Local variables: None.
; Algorithm:
    ; 1. Print several messages that indicates what the program is and what 
    ;    number to input for their corresponding actions.
	  ;	2. If the user answers 1, start a new game and jump to startNewRound.
 	  ;	3. If the user answers 2, load a save game by jumping to resumeGame.
 	  ;	4. Otherwise, the user wants to quit. Print a goodbye message and exit.
; Assistance received: None.
;***************************************************************************

(defun longana ()
  (terpri)
  (printMessage "Welcome to Longana, a two-player Cuban domino game.")
  (printMessage "In this version you will be playing against a computer.")
  (printMessage "Please enter your choice from the options below:")
  (printMessage "1). Start a new game.")
  (printMessage "2). Load a saved game.")
  (printMessage "3). Quit.")
  (terpri)

  (let ((answer (askForInput "Enter your choice:" 1 3)))
  (terpri)

  (cond ((= answer 1)
          (let ((tournamentScore (getTournamentScore)))
         	(startNewRound 0 0 tournamentScore 1)))
        ((= answer 2)
         	(resumeGame(loadFile)))
        (t
          (terpri)
         	(printMessage "Hope to see you again soon... goodbye!")))))

;***************************************************************************
; Function Name: startNewRound
; Purpose: To help start a new round of Longana.
; Parameters: 
    ; computerScore and humanScore, the scores of each player.
    ; tournamentScore, the score for the tournament as given by the user.
    ; engine, the engine for the current round of Longana.
; Return value: T
; Local variables: 
    ; firstPlayerResult, the first player determined by who holds the engine.
    ; humanHand, the human player's hand.
    ; computerHand, the computer player's hand.
    ; boneyard, the boneyard for Longana.
    ; stock, the initial, shuffled, double six-set for Longana.
    ; engine, the engine for the current round of Longana.
; Algorithm:
    ; 1. Check if the human player has won the tournament.
    ; 2. Check if the computer player has won the tournament.
    ; 3. If neither player has won the tournament:
        ; a. Generate the initial stock.
        ; b. Draw both player's hands.
        ; c. Create the boneyard.
        ; d. Determine the engine for the round.
        ; e. Determine the first player.
        ; f. Create the gameboard.
        ; g. Go ahead and set up the round to play.
; Assistance received: None.
;***************************************************************************

(defun startNewRound (humanScore computerScore tournamentScore roundNum)
  (terpri)
  (princ "Tournament Score: ")
  (princ tournamentScore)
  (terpri)

  ; Check that the tournament score has not been reached.
  (cond ((> humanScore tournamentScore)
            (printMessage "The human player won the tournament!")
            (princ "Human player's score: ")
            (princ humanScore)
            (terpri)
            (princ "Computer player's score: ")
            (princ computerScore)
            (terpri))
        ((> computerScore tournamentScore)
            (printMessage "The computer player won the tournament!")
            (princ "Human player's score: ")
            (princ humanScore)
            (terpri)
            (princ "Computer player's score: ")
            (princ computerScore)
            (terpri))
        
        ; If the tournament has not ended, start a new round.
        (t 
            (terpri)
            (princ "Entering round: ")
            (princ roundNum)
            (terpri)
            (terpri)

            ; Generate the stock, deal the hands, form the boneyard.
            ; Determine the engine for the round.
            ; Determine who the first player will be.
            (let* 
              ((stock (initializeStock 0 0))
              (humanHand (drawHand stock 0 0))
              (computerHand (drawHand stock 0 1))
              (boneyard (removeDominoes 16 stock))
              (engine (determineEngine roundNum))
              (firstPlayerResult (determineFirstPlayer humanHand computerHand engine))
              (gameboard ()))
              (printMessage "Would you like to save the game now?")
              (printMessage "Both player's hands have been drawn and the boneyard has been generated.")
              (printMessage "To save now enter Save, to play the round enter Play.")
              (terpri)
              (printMessage "Your choice: ")
              (let ((input (read)))
                (terpri)

                ; Save the game.
                (cond ((equal input `Save)
                        (saveGame tournamentScore roundNum computerHand computerScore humanHand humanScore gameboard boneyard `N firstPlayerResult))

                      ; Play the new round.
                      ((equal input `Play)
                        (setUpRound gameboard humanScore computerScore tournamentScore humanHand computerHand boneyard firstPlayerResult engine roundNum))
                      (t 
                        (printMessage "Sorry that input was incorrect, try again!")
                        (startNewRound humanScore computerScore tournamentScore roundNum))))))))

;***************************************************************************
; Function Name: setUpRound
; Purpose: Intermediary step to help start a new round of Longana.
; Parameters: 
    ; computerScore and humanScore, the scores of each player.
    ; tournamentScore, the score of the tournament.
    ; humanHand, the human player's hand.
    ; computerHand, the computer player's hand.
    ; boneyard, the boneyard for longana.
    ; firstPlayer, the first player of the round (engine holder).
    ; engine, the engine for the round.
; Return value: T
; Local variables: 
    ; humanHand, the human player's hand.
    ; computerHand, the computer player's hand.
    ; boneyard, the boneyard for longana.
    ; firstPlayer, the first player of the round (engine holder).
; Algorithm:
    ; 1. If either the human or the computer player holds the engine, 
    ;    remove it from the respective player's hand, add it to
    ;    the gameboard and play the round.
    ; 2. Otherwise, have both players draw from the boneyard.
    ; 3. Remove the dominoes from the boneyard appropriately.
    ; 4. Recursively call setUpRound until one of the player's has the engine.
; Assistance received: None.
;***************************************************************************

(defun setUpRound (gameboard humanScore computerScore tournamentScore humanHand computerHand boneyard firstPlayer engine roundNum)
 
 ; Begin the round with the human as the first player.
 (cond ((equal firstPlayer `H)
          (printMessage "The human player had the engine!")
          (printMessage "The engine has been added to the gameboard.")
          (let* 
            ((humanHand (removeUniqueDomino (list engine engine) humanHand))
             (gameboard (modifyGameboard (list engine engine) gameboard `R)))
            (playRound gameboard humanScore computerScore tournamentScore humanHand `N computerHand `N boneyard `C roundNum)))

      ; Begin the round with the computer as the first player.
      ((equal firstPlayer `C)
          (printMessage "The computer player had the engine!")
          (printMessage "The engine has been added to the gameboard.")
          (let* 
            ((computerHand (removeUniqueDomino (list engine engine) computerHand))
             (gameboard (modifyGameboard (list engine engine) gameboard `L)))
            (playRound gameboard humanScore computerScore tournamentScore humanHand `N computerHand `N boneyard `H roundNum)))
      
      ; Neither player had the engine, deal two dominoes, and remove them from the boneyard.
      (t 
          (printMessage "Neither player had the engine, dealing both players one new domino!")
          (let*
            ((humanHand (drawFromBoneyard boneyard humanHand))
            (boneyard (removeDominoes 1 boneyard))
            (computerHand (drawFromBoneyard boneyard computerHand))
            (boneyard (removeDominoes 1 boneyard))
            (firstPlayer (determineFirstPlayer humanHand computerHand engine)))
            (printBoneyardLength boneyard)
            (terpri)
            (setUpRound gameboard humanScore computerScore tournamentScore humanHand computerHand boneyard firstPlayer engine roundNum)))))

;***************************************************************************
; Function Name: playRound
; Purpose: To play a round of Longana.
; Parameters: 
    ; computerScore and humanScore, the scores of each player.
    ; tournamentScore, the score of the tournament.
    ; humanHand, the human player's hand.
    ; computerHand, the computer player's hand.
    ; boneyard, the boneyard for longana.
    ; firstPlayer, the first player of the round (engine holder).
    ; roundNum, the current round number.
; Return value: T
; Local variables: None.
; Algorithm:
    ; 1. If the boneyard has emptied, end the round.
    ; 2. If the human has emptied their hand, end the round.
    ; 3. If the computer has emptied its hand, end the round.
    ; 4. If both player's passed, end the round.
    ; 5. Otherwise:
        ; a. If it's the human player's turn to play:
            ; 1. Draw a domino from the boneyard if necessary.
            ; 2. If the human player passed their play, allow the computer to play.
            ; 3. If the human placed a domino, validate it, and place it.
            ; 4. If the human wishes to save the game, do so.
        ; b. If it's the computer's turn to play:
            ; 1. Draw a domino if necessary.
            ; 2. Call computerInputPlay.
            ; 3. Announce the computer's move.
; Assistance received: None.
;***************************************************************************

(defun playRound (gameboard humanScore computerScore tournamentScore humanHand humanPassed computerHand computerPassed boneyard nextPlayer roundNum)
  (terpri)
  (cond ((null boneyard)
        (printMessage "The boneyard has been emptied, ending the round...")
        (determineWinner computerScore humanScore `bothPassed humanHand computerHand tournamentScore roundNum))
    ((null humanHand)
        (printMessage "The human player emptied their hand, ending the round...")
        (determineWinner computerScore humanScore `humanEmptied humanHand computerHand tournamentScore roundNum))
    ((null computerHand)
        (printMessage "The computer player emptied its hand, ending the round...")
        (determineWinner computerScore humanScore `computerEmptied humanHand computerHand tournamentScore roundNum))
    (t 
      ; If the next player is the human.
      (cond ((equal nextPlayer `H)
                (let* ((humanPlay (humanInputPlay humanHand gameboard boneyard)))
                  
                  ; If necessary, draw a domino from the boneyard.
                  (cond ((and (equal humanPlay `P) (equal humanPassed `N))
                          (let*
                            ((humanHand (drawFromBoneyard boneyard humanHand))
                            (boneyard (removeDominoes 1 boneyard)))
                            (printMessage "A domino has been drawn from the boneyard for you!")
                            (playRound gameboard humanScore computerScore tournamentScore humanHand `Y computerHand computerPassed boneyard `H roundNum)))
                        
                        ; If the human player has picked up a domino from the boneyard, and passed their play.
                        ((and (equal humanPlay `P) (equal humanPassed `Y))
                          
                          ; If both player's passed, end the round.
                          (cond ((equal computerPassed `Y)
                                    (printMessage "The human player passed their play.")
                                    (terpri)
                                    (printMessage "Both players passed their plays, ending the round!")
                                    (determineWinner computerScore humanScore `bothPassed humanHand computerHand tournamentScore roundNum))
                                
                                ; The human player passed.
                                (t 
                                    (printMessage "The human player passed their play!")
                                    (playRound gameboard humanScore computerScore tournamentScore humanHand `Y computerHand `N boneyard `C roundNum))))
                        
                        ; If the human player wishes to save.
                        ((equal humanPlay `S)
                          (saveGame tournamentScore roundNum computerHand computerScore humanHand humanScore gameboard boneyard computerPassed nextPlayer)
                          (printMessage "The file has been successfully saved, goodbye!"))
                        
                        ; If the human player requests help.
                        ((equal humanPlay `H)
                          (helpMode gameboard humanHand computerPassed)
                          (playRound gameboard humanScore computerScore tournamentScore humanHand humanPassed computerHand computerPassed boneyard `H roundNum))
                        
                        ; Otherwise, the human player gave a play, validate it, and place the domino.
                        (t
                          (let ((side (first humanPlay))
                                (domino (rest humanPlay))) 
                            (let*((result (validateDominoPlacement gameboard side domino computerPassed humanHand)))
                              (cond ((equal result `V)
                                        (let*((humanHand (removeUniqueDomino domino humanHand))
                                              (gameboard (modifyGameboard domino gameboard side)))
                                        (playRound gameboard humanScore computerScore tournamentScore humanHand `N computerHand `N boneyard `C roundNum)))
                                    (t 
                                      (playRound gameboard humanScore computerScore tournamentScore humanHand humanPassed computerHand `N boneyard `H roundNum)))))))))
           
            ; If the next player is the computer.
            ((equal nextPlayer `C)
                (printBoneyardLength boneyard)
                (let* ((computerPlay (computerInputPlay gameboard computerHand humanPassed)))
                  
                  ; If the computer passes, and has not already picked up a domino from the boneyard, do so.
                  (cond ((and (equal computerPlay `P) (equal computerPassed `N))
                          (let* ((computerHand (drawFromBoneyard boneyard computerHand))
                                (boneyard (removeDominoes 1 boneyard)))
                                (printMessage "The computer drew a tile from the boneyard.")
                                (playRound gameboard humanScore computerScore tournamentScore humanHand humanPassed computerHand `Y boneyard `C roundNum)))
                        
                        ; If the computer has picked up a domino, and passes.
                        ((and (equal computerPlay `P) (equal computerPassed `Y))
                          
                          ; Both player's have passed, end the round.
                          (cond ((equal humanPassed `Y)
                                    (printMessage "The computer player passed their play.")
                                    (terpri)
                                    (printMessage "Both players passed their plays, ending the round!")
                                    (determineWinner computerScore humanScore `bothPassed humanHand computerHand tournamentScore roundNum))
                                
                                ; Let the human player play.
                                (t 
                                    (printMessage "The computer player passed their play!")
                                    (playRound gameboard humanScore computerScore tournamentScore humanHand `N computerHand `Y boneyard `H roundNum))))
                        
                        ; The computer made a play, place the domino on the given side.
                        (t 
                          (let ((side (first computerPlay))
                                (domino (rest computerPlay)))
                                (princ "The computer placed ")
                                (printDomino domino)
                                (cond ((equal side `L)
                                          (princ " to the left."))
                                (t 
                                  (princ " to the right.")))
                                (let* ((computerHand (removeUniqueDomino domino computerHand))
                                      (gameboard (modifyGameboard domino gameboard side)))
                                (playRound gameboard humanScore computerScore tournamentScore humanHand `N computerHand `N boneyard `H roundNum)))))))
            (t 
              ())))))

;***************************************************************************
; Function Name: determineFirstPlayer
; Purpose: To determine who holds the engine for the first play of Longana.
; Parameters: 
    ; computerHand the hand for the computer player.
    ; humanHand the hand for the human player.
    ; engine, the engine for the current round.
; Return value: 
    ; D for both player's needing to draw.
    ; H if the human's hand holds the engine.
    ; C if the computer's hand holds the engine.
; Local variables: None.
; Algorithm:
    ; 1. If both player's hands are empty, neither hold the engine and must
    ;    both draw.
    ; 2. Otherwise, go through each domino in the player's hands, determine
    ;    if the domino is a double.
    ; 3. If the domino is a double, call determineIfEngine to see if the 
    ;    domino is the engine.
    ; 4. Call determineFirstPlayer recursively until a condition is met.
; Assistance received: None.
;***************************************************************************

(defun determineFirstPlayer (humanHand computerHand engine)
    (cond((and(null humanHand) (null computerHand)) 
          `D) 
        ((and (determineIfDouble (first humanHand)) (determineIfEngine (first humanHand) engine))
          `H)
        ((and (determineIfDouble (first computerHand)) (determineIfEngine (first computerHand) engine))
          `C)
        (t 
          (determineFirstPlayer (rest humanHand) (rest computerHand) engine))))

;***************************************************************************
; Function Name: determineEngine
; Purpose: To calculate the engine for the current round.
; Parameters:
    ; roundNum, the current round number in the Longana tournament.
; Return value: 
    ; An integer value representing the engine for the round.
; Local variables: 
    ; num, value used to represent the engine.
; Algorithm:
    ; 1. Mod the given roundNum by 7.
    ; 2. Subtract the result of the modulus from 6.
; Assistance received: None.
;***************************************************************************

(defun determineEngine (roundNum)
  (let ((num (mod (- roundNum 1) 7)))
    (- 6 num)))

; -----------------Human Player Input and Validation-------------------

;***************************************************************************
; Function Name: humanInputPlay
; Purpose: To get play input from the human player.
; Parameters: 
    ; humanHand the hand for the human player.
    ; gameboard, the current gameboard.
    ; boneyard, the boneyard for longana.
; Return value: 
    ; P if the player passed.
    ; Q if the player quit without saving.
    ; S if the player wishes to save the game.
    ; A list containing a domino and side to place the domino on.
; Local variables: 
    ; input, the human player's input.
; Algorithm:
; Assistance received: None.
;***************************************************************************

(defun humanInputPlay (humanHand gameboard boneyard)
  (displayGameboard gameboard)
  (printPlayerHand humanHand)
  (terpri)
  (printBoneyardLength boneyard)
  (printInputInstructions)
  (printMessage "Your play:")
  (let
      ((input (read)))
      (cond ((equal input `Pass)
                `P)
            ((equal input `Quit)
                (printMessage "Hope to see you again soon... goodbye!"))
            ((equal input `Save)
                `S)
            ((equal input `Help)
                `H)
            ;If the human's input is a list.
            ((cond ((listp input)
                      ; Check that its length is 3 (side pip pip).
                      (cond ((equal (length input) 3)
                                input)
                      ; Otherwise, it was improper format.
                      (t 
                          (terpri)
                          (printMessage "Sorry that format was incorrect, try again!")
                          (humanInputPlay humanHand gameboard boneyard))))))
            (t 
                (terpri)
                (printMessage "Sorry that format was incorrect, try again!")
                (humanInputPlay humanHand gameboard boneyard)))))

;***************************************************************************
; Function Name: validateDominoPlacement
; Purpose: To validate play input from the human player.
; Parameters: 
    ; humanHand the hand for the human player.
    ; gameboard, the current gameboard.
    ; side, the side of the gameboard for which the domino is to be placed.
    ; domino, the domino as entered by the user.
    ; computerPassed, whether or not the computer passed its play.
; Return value: 
    ; `V for valid play.
    ; `N for non-valid play.
; Local variables: 
    ; rightMostPip, the right-most pip on the gameboard.
    ; leftMostPip, the left-most pip on the gameboard.
; Algorithm:
    ; 1. Check that the side matches L or R.
    ; 2. If side matches `R:
        ; a. Ensure that the given domino is either a double or the computer
        ;    passed its turn.
        ; b. Ensure that the domino entered can be placed on the given side.
    ; 3. If side matches `L:
        ; a. Ensure that the domino entered can be placed on the given side.
    ; 4. If a given domino cannot be placed, let the user know, return `N.
    ; 5. Otherwise, if the domino can be placed, return `V for valid.
; Assistance received: None.
;***************************************************************************

(defun validateDominoPlacement (gameboard side domino computerPassed humanHand)

  ; Ensure the domino exists in the player's hand.
  (cond ((checkDominoExists humanHand domino)

      ; If the user wants to place right, the domino must be a double, or
      ; the computer must have passed its play.
      (cond ((equal side `R)
          (cond ((or (equal computerPassed `Y) (determineIfDouble domino))
                    (let ((rightMostPip (first(rest(first(last gameboard))))))
                        (cond ((or (= (first domino) rightMostPip) (= (first (rest domino)) rightMostPip))
                                  `V)
                              (t 
                                  (printMessage "That domino could not be placed there.")
                                  (printMessage "Try again.")
                                  `N))))))

        ; If the user wants to place left, the domino must match the left-most domino
        ; on the gameboard.
        ((equal side `L)
            (let ((leftMostPip (first(first gameboard))))
                    (cond ((or (= (first domino) leftMostPip) (= (first (rest domino)) leftMostPip))
                              `V)
                          (t 
                              (printMessage "That domino could not be placed there.")
                              (printMessage "Try again.")
                              `N))))
        (t 
            (printMessage "Side must be either R or L!")
            `N)))
  (t 
      (printMessage "That domino is not in your hand, try again.")
      `N)))
  

;***************************************************************************
; Function Name: checkDominoExists
; Purpose: To check if a given domino exists in a player's hand.
; Parameters:
    ; humanHand, the human player's hand.
    ; domino, the domino to search for in the player's hand.
; Return value: T.
; Local variables: None.
; Assistance received: None.
;***************************************************************************

(defun checkDominoExists (humanHand domino)
  (find domino humanHand :test #'equal))


; -----------------Computer Play/Strategy Functions-------------------

;***************************************************************************
; Function Name: computerInputPlay
; Purpose: To have the computer place a valid domino.
; Parameters:
    ; computerHand, the computer player's hand.
    ; humanPassed, whether or not the human player passed their play.
    ; gameboard, the current gameboard for Longana.
; Return value: T.
; Local variables: 
    ; gameboardPip, the leftMostPip on the gameboard.
; Assistance received: None.
;***************************************************************************

(defun computerInputPlay (gameboard computerHand humanPassed)

  ; If the human passed, the computer can place on either side.
  (cond ((equal humanPassed `Y)
          (cond ((equal (canMakePlay (first(rest(first(last gameboard)))) computerHand) `Y)
                  (findBestPlay gameboard computerHand `R))
                ((equal (canMakePlay (first (first gameboard)) computerHand) `Y)
                  (findBestPlay gameboard computerHand `L))
                (t 
                  `P)))

        ; If the human did not pass, the computer can only place on the right.
        ((equal humanPassed `N)
          (let ((gameboardPip (first(rest(first(last gameboard))))))
          (cond ((equal (canMakePlay gameboardPip computerHand) `Y)
                  (findBestPlay gameboard computerHand `R))
                (t 
                  `P))))
        (t
          `P)))

;***************************************************************************
; Function Name: canMakePlay
; Purpose: To determine if a player can place a domino.
; Parameters:
    ; playerHand, a player's hand.
    ; gameboardPip, the left-most or right-most pip on the gameboard.
; Return value: 
    ; `N if the player cannot place a domino.
    ; `Y if the player can place a domino.
; Local variables: None.
; Algorithm:
    ; 1. Go through the given player's hand recursively, checking if a domino can be placed.
    ; 2. If the player has a domino that can be placed return `Y.
    ; 3. If there are no dominoes remaining in the player's hand, none could
    ;    be placed, return `N.
; Assistance received: None.
;***************************************************************************

(defun canMakePlay (gameboardPip playerHand)
  (cond 
        ((null playerHand)
            `N)
        ((or (= (first(first playerHand)) gameboardPip) (= (first(rest(first playerHand))) gameboardPip))
            `Y)
        (t 
            (canMakePlay gameboardPip (rest playerHand)))))

;***************************************************************************
; Function Name: findBestPlay
; Purpose: To find the best possible play for a player.
; Parameters:
    ; playerHand, a player's hand.
    ; gameboard, the current gameboard for Longana.
    ; side, the side of the board for which the domino is to be placed.
; Return value: 
    ; A play in the form of (<side> <left pip> <right pip>)
; Local variables: 
    ; domino, the result of the findHighestSumDomino function (a domino).
; Assistance received: None.
;***************************************************************************

(defun findBestPlay (gameboard playerHand side)
  (cond ((equal side `L)
          (let* ((domino (findHighestSumDomino (first(first gameboard)) playerHand 0 ())))
            (cons `L domino)))
        ((equal side `R)
          (let* ((domino (findHighestSumDomino (first(rest(first(last gameboard)))) playerHand 0 ())))
            (cons `R domino)))))

;***************************************************************************
; Function Name: findHighestSumDomino
; Purpose: To find the domino with the highest sum.
; Parameters:
    ; gameboardPip, the right-most or left-most pip on the gameboard.
    ; playerHand, a player's hand.
    ; sum, the lowest sum thus far in the player's hand.
; Return value: 
    ; A domino in the form of (<left pip> <right pip>).
; Local variables: 
    ; result, the result of the findHighestSumDomino function (a domino).
; Algorithm:
    ; 1. Go through the player's hand recursively.
    ; 2. Find the domino with the highest sum of pips in the hand.
    ; 3. Return the domino with the highest sum. 
; Assistance received: None.
;***************************************************************************

(defun findHighestSumDomino (gameboardPip playerHand sum greatestDomino)
  (cond ((null playerHand)
            greatestDomino)
        (t 
          (let ((domino (first playerHand)))
            ; Ensure the domino can be placed on the correct side of the gameboard.
            (cond ((or (= (first domino) gameboardPip) (= (first (rest domino)) gameboardPip))
                      (let* ((dominoSum (calculateDominoSum domino)))
                        (cond ((> dominoSum sum)
                                  (findHighestSumDomino gameboardPip (rest playerHand) dominoSum domino))
                              (t 
                                (findHighestSumDomino gameboardPip (rest playerHand) dominoSum greatestDomino)))))
                  (t 
                    (findHighestSumDomino gameboardPip (rest playerHand) sum greatestDomino)))))))
  
; -----------------Help Mode Functions-------------------

;***************************************************************************
; Function Name: helpMode
; Purpose: To recommend a play to the human player.
; Parameters:
    ; gameboard, the current gameboard for longana.
    ; humanHand, the human player's hand.
    ; computerPassed, whether or not the computer placed their turn.
; Return value: T, output based.
; Local variables: 
    ; bestPlay, the best play for the human player to make.
    ; gameboardPip, the left-most pip on the gameboard.
; Algorithm:
    ; 1. First determine whether or not the player has any dominoes
    ;    that can be placed on either side depending on whether or not
    ;    the computer passed its turn.
    ; 2. If the player can place a domino: 
        ; a. Go through the player's hand and find the domino with the lowest
        ;    sum that can be placed.
        ; b. Output that domino in the proper format.
    ; 3. Otherwise, if the player can't place a domino, recommend to the player
    ;    that they pass their play. 
; Assistance received: None.
;***************************************************************************

(defun helpMode (gameboard humanHand computerPassed)
  (terpri)
  (printMessage "Welcome to the help mode for Longana.")
  (printMessage "The computer will recommend a play for you!")
  (terpri)

  ; If the computer passed, the human can place on either side.
  (cond ((equal computerPassed `Y)

          ; Check if the human has a domino that can be placed to the right.
          (cond ((equal (canMakePlay (first(rest(first(last gameboard)))) humanHand) `Y)
                    (let ((bestPlay (findBestPlay gameboard humanHand `R)))
                        (princ "The computer recommends placing ")
                        (printDomino (rest bestPlay))
                        (princ " to the right.")
                        (terpri)
                        (printMessage "As that domino has the highest sum in your hand (that can be placed to the right).")))

                ; Check if the human has a domino that can be placed to the left.
                ((equal (canMakePlay (first (first gameboard)) humanHand) `Y)
                    (let ((bestPlay (findBestPlay gameboard humanHand `L)))
                        (princ "The computer recommends placing ")
                        (printDomino (rest bestPlay))
                        (princ " to the left.")
                        (terpri)
                        (printMessage "As that domino has the highest sum in your hand (that can be placed to the left).")))

                ; Otherwise, the human does not have a domino that can be placed, they must pass.
                (t 
                  (printMessage "You do not have any dominoes in your hand that can be placed.")
                  (printMessage "Therefore, the computer recommends passing your play!"))))

        ; If the computer did not pass, the human can only place to the left.
        ((equal computerPassed `N)
            (let ((gameboardPip (first(first gameboard))))

            ; Check that the human can place to the left.
            (cond ((equal (canMakePlay gameboardPip humanHand) `Y)
                    (let ((bestPlay (findBestPlay gameboard humanHand `L)))
                        (princ "The computer recommends placing ")
                        (printDomino (rest bestPlay))
                        (princ " to the left.")
                        (terpri)
                        (printMessage "As that domino has the highest sum in your hand (that can be placed).")))
                  (t 
                      (printMessage "You do not have any dominoes in your hand that can be placed.")
                      (printMessage "Therefore, the computer recommends passing your play!")))))
       
        (t
            (printMessage "You do not have any dominoes in your hand that can be placed.")
            (printMessage "Therefore, the computer recommends passing your play!"))))

; -----------------Domino Manipulation Functions-------------------

;***************************************************************************
; Function Name: removeDomnioes
; Purpose: To remove a given set of dominoes from the beginning of a list.
; Parameters:
		; length, the length of the given list.
		; vector, a list containing domnioes.
; Return value: A list containing dominoes with the desired dominoes removed.
; Local variables: None.
; Assistance received: None.
;***************************************************************************

(defun removeDominoes(length vector)
    (cond ((= length 0)
              vector)
          (t 
              (removeDominoes (- length 1) (rest vector)))))

;***************************************************************************
; Function Name: determineIfDouble
; Purpose: To determine whether or not a given domino is a double.
; Parameters:
		; domino, a list containing a single domino.
; Return value: 
    ; A boolean value.
; Local variables: None.
; Algorithm:
		; 1. Check if the first and second values in the list are equal.
; Assistance received: None.
;***************************************************************************

(defun determineIfDouble (domino)
    (equal (first domino) (first(rest domino))))

;***************************************************************************
; Function Name: removeUniqueDomino
; Purpose: To remove a given domino from a player's hand.
; Parameters:
    ; domino, a list containing a single domino.
    ; vector, a list containing dominoes, mainly a player's hand.
; Return value: 
    ; A list containing the player's hand with the given domino removed.
; Local variables: None.
; Algorithm:
    ; 1. Utilize remove to remove the given domino from the vector.
; Assistance received: None.
;***************************************************************************

(defun removeUniqueDomino (domino vector)
  (remove domino vector :test #'equal))

; -----------------Stock Functions-------------------

;***************************************************************************
; Function Name: initializeStock
; Purpose: To initialize the shuffled double-six set for longana.
; Parameters: 
    ; rightPip, the pips on the right side of the domino.
    ; leftPip, the pips on the left side of the domino.
; Return value: A list containing the shuffled initial stock for longana.
; Local variables: None.
; Algorithm:
    ; 1. If both the left and right pip equal 7, return an empty list.
    ; 2. If the right pip equals 6 recursively call initializeStock.
    ; 3. Otherwise, call shuffleStock to return a shuffled stock.
; Assistance received: None.
;***************************************************************************

(defun initializeStock (leftPip rightPip)
	(cond ((and (= leftPip 7) (= rightPip 7))
            ())
        ((= rightPip 6)
           	(append (initializeStock (+ leftPip 1) (+ leftPip 1)) (list(list leftPip rightPip))))
        (t
            (shuffleStock (append (initializeStock leftPip (+ rightPip 1)) (list(list leftPip rightPip)))))))

;***************************************************************************
; Function Name: shuffleStock
; Purpose: To shuffle the stock for longana.
; Parameters: None.
; Return value: A list containing the initial, shuffled stock for longana.
; Local variables: 
    ; rightPip, the pips on the right side of the domino.
    ; leftPip, the pips on the left side of the domino.
; Assistance received: None.
;***************************************************************************

(defun shuffleStock (stock)
	(cond ((= (random 200 (make-random-state t)) 56)
            (append stock nil))
          (t 
            (let*
                ((rightPip(random 7))
					      (leftPip(random (+ rightPip 1))))  
                (shuffleStock (cons (list leftPip rightPip)
                (remove (list leftPip rightPip) stock :test #'equal)))))))

;***************************************************************************
; Function Name: drawHand
; Purpose: To draw a player's hand from the double-six set of domnioes.
; Parameters: 
    ; stock, the initial, shuffled double-six set for Longana.
    ; dominoCount, the number of dominoes to be drawn from the stock.
    ; playerId, identifies the player, 0 for computer, 1 for human.
; Return value: A list containing a player's initial drawn hand (8 dominoes).
; Local variables: None.
; Algorithm:
    ; 1. If dominoCount equals 8 return an empty list.
    ; 2. Otherwise, draw a domino from the stock for the respective playerId.
    ; 3. Continue to recursively call drawHand until dominoCount reaches 8.
; Assistance received: None.
;***************************************************************************

(defun drawHand(stock dominoCount playerId)
    (cond ((= dominoCount 8)
            ())
          ((= playerId 0)
             (cons (first stock) (drawHand (rest(rest stock)) (+ dominoCount 1) 0)))
          ((= playerId 1)
             (cons (first (rest stock)) (drawHand (rest(rest stock)) (+ dominoCount 1) 1)))
          (t
          stock)))

;***************************************************************************
; Function Name: drawFromBoneyard
; Purpose: To draw a single domino from the boneyard.
; Parameters: 
    ; boneyard, boneyard for Longana.
    ; hand, a player's current hand.
; Return value: 
    ; A list containing a player's hand with a domino drawn from the boneyard.
; Local variables: None.
; Algorithm:
    ; 1. Append the first domino in the boneyard to the player's hand.
; Assistance received: None.
;***************************************************************************

(defun drawFromBoneyard (boneyard hand)
  (append hand (list(first boneyard))))

;***************************************************************************
; Function Name: determineIfEngine
; Purpose: To determine whether or not a particular domino is the engine.
; Parameters: 
    ; domino, a list representing a domino.
    ; engine, the engine for the current round.
; Return value: T.
; Local variables: None.
; Algorithm:
    ; 1. Check if the left pip of the domino equals the engine.
; Assistance received: None.
;***************************************************************************

(defun determineIfEngine (domino engine)
  (= (first domino) engine))

; -----------------Score Maintenance Functions-------------------

;***************************************************************************
; Function Name: calculateDominoSum
; Purpose: To calculate the sum of pips on a single domino.
; Parameters: 
		; domino, a list containing a single domino.
; Return value: A value representing the summation of the left and right pips of a domino.
; Local variables: None.
; Algorithm:
		; 1. Add the left and right pips of the domino.
; Assistance received: None.
;***************************************************************************

(defun calculateDominoSum (domino)
  (+ (first domino) (first(rest domino))))

;***************************************************************************
; Function Name: calculateHandSum
; Purpose: To calculate the sum of all dominoes in a player's hand.
; Parameters: 
    ; hand, a list of lists containing a player's hand.
    ; sum, a player's current tournament score.
; Return value: A value representing the summation of the dominoes in a player's hand.
; Local variables: None.
; Algorithm:
    ; 1. If the hand vector is empty, return the sum.
    ; 2. Otherwise, call calculateDominoSum on the first domino in the hand.
    ; 3. Recursively call calculateHandSum, incrementing the sum accordingly.
; Assistance received: None.
;***************************************************************************

(defun calculateHandSum (hand sum)
  (cond (( null hand)
          sum)
        (t
          (calculateHandSum (rest hand) (+ sum (calculateDominoSum(first hand)))))))

;***************************************************************************
; Function Name: getTournamentScore
; Purpose: To ask the user for the tournament score.
; Parameters: None.
; Return value: A value representing the tournament score.
; Local variables: None.
; Assistance received: None.
;***************************************************************************
(defun getTournamentScore ()
  (askForInput "Please enter the maximum score for the tournament: " 1 500))

;***************************************************************************
; Function Name: determineWinner
; Purpose: To determine which player won the round, and add to the player's 
;          score appropriately.
; Parameters: 
    ; computerScore, the computer player's score.
    ; humanScore, the human player's score.
    ; winningCase, the case that ended the round:
        ; `bothPassed if either both player's passed or the boneyard is empty.
        ; `humanEmptied if the human's hand is empty.
        ; `computerEmptied if the computer's hand is empty.
    ; humanHand, the human player's hand.
    ; computerHand, the computer player's hand.
    ; tournamentScore, the score of the tournament.
    ; roundNum, the current round number.
; Return value: T.
; Local variables: None.
; Algorithm:
    ; 1. If both player's passed:
        ; a. The player with the smaller sum hand wins, and gets the sum of
        ;    their opponent's hand added to their score.
        ; b. If the player's hands have the same sum, it is a draw.
    ; 2. If a player emptied their hand, they get the sum of their opponent's
    ;    hand added to their score.
; Assistance received: None.
;***************************************************************************

(defun determineWinner (computerScore humanScore winningCase humanHand computerHand tournamentScore roundNum)

  ; If both players passed, the player with the lower sum of domino pips earns the 
  ; sum of pips in their opponents hand as their score.
  (cond ((equal winningCase `bothPassed)
            (let* ((computerHandSum (calculateHandSum computerHand 0))
                   (humanHandSum (calculateHandSum humanHand 0)))

            ; If the human has a lower sum, the player gets the computer's sum
            ; as their score.
            (cond ((< humanHandSum computerHandSum)
                    (terpri)
                    (printMessage "The human player won the round!")
                    (princ "Human player's score: ")
                    (princ (+ humanScore computerHandSum))
                    (terpri)
                    (princ "Computer player's score: ")
                    (princ computerScore)
                    (terpri)
                    (startNewRound (+ humanScore computerHandSum) computerScore tournamentScore (+ roundNum 1)))

                  ; If the computer has a lower sum than the human, the computer gets
                  ; the human's sum as its score.
                  ((< computerHandSum humanHandSum)
                    (terpri)
                    (printMessage "The computer player won the round!")
                    (princ "Computer player's score: ")
                    (princ (+ computerScore humanHandSum))
                    (terpri)
                    (princ "Human player's score: ")
                    (princ humanScore)
                    (terpri)
                    (startNewRound humanScore (+ computerScore humanHandSum) tournamentScore (+ roundNum 1)))

                  ; If the sum's for both player's are equal, the round is a draw, and neither player
                  ; is awarded any points.
                  (t 
                    (terpri)
                    (printMessage "Both player's hands had the same sum of pips.")
                    (printMessage "Therefore, the round is a draw!")
                    (princ "Human player's score: ")
                    (princ humanScore)
                    (terpri)
                    (princ "Computer player's score: ")
                    (princ computerScore)
                    (terpri)
                    (startNewRound humanScore computerScore tournamentScore (+ roundNum 1))))))

        ; If the human player emptied their hand, they win the round and earn
        ; the computer's hand sum of pips as their score.
        ((equal winningCase `humanEmptied)
          (let* ((humanScore (calculateHandSum computerHand humanScore)))
              (terpri)
              (printMessage "The human player won the round!")
              (princ "Human player's score: ")
              (princ humanScore)
              (terpri)
              (princ "Computer player's score: ")
              (princ computerScore)
              (terpri)
              (startNewRound humanScore computerScore tournamentScore (+ roundNum 1))))

        ; If the computer emptied its hand, it is awarded the human's hand sum
        ; as its score.
        ((equal winningCase `computerEmptied)
            (let* ((computerScore (calculateHandSum humanHand computerScore)))
              (terpri)
              (printMessage "The computer player won the round!")
              (princ "Human player's score: ")
              (princ humanScore)
              (terpri)
              (princ "Computer player's score: ")
              (princ computerScore)
              (terpri)
              (startNewRound humanScore computerScore tournamentScore (+ roundNum 1))))))

; -----------------Output/Display Functions-------------------

;***************************************************************************
; Function Name: printDomino
; Purpose: To print a single domino in the proper format.
; Parameters: 
		; domino, a list containing a single domino.
; Return value: None.
; Local variables: None.
; Assistance received: None.
;***************************************************************************

(defun printDomino (domino)
    (princ (first domino))
    (princ "-")
    (princ (first(rest domino))))

;***************************************************************************
; Function Name: printDominoes
; Purpose: To print a list of domnioes.
; Parameters: 
		; vector, a list of lists holding dominoes.
; Return value: None.
; Local variables: None.
; Algorithm:
		; 1. If vector is null, return nothing.
		; 2. Otherwise, call print domino on each list found within the vector,
		; 	 by calling printDomino and recursively calling printDominoes.
; Assistance received: None.
;***************************************************************************

(defun printDominoes (vector)
    (cond ((null vector))
          (t
            (printDomino (first vector))
            (princ " ")
            (printDominoes (rest vector)))))

;***************************************************************************
; Function Name: printBoneyardLength
; Purpose: To print the number of remaining dominoes in the boneyard.
; Parameters: 
    ; boneyard, a list of lists holding the boneyard.
; Return value: None.
; Local variables: None.
; Assistance received: None.
;***************************************************************************

(defun printBoneyardLength (boneyard)
  (princ "Dominoes remaining in the boneyard: ")
  (princ (length boneyard))
  (terpri))

;***************************************************************************
; Function Name: printPlayerHand
; Purpose: To print the a player's current hand.
; Parameters: 
    ; hand, a list of lists holding the player's current hand.
; Return value: None.
; Local variables: None.
; Assistance received: None.
;***************************************************************************

(defun printPlayerHand (hand)
  (princ "Your current hand: ")
  (printDominoes hand))

;***************************************************************************
; Function Name: printInputInstructions
; Purpose: To print directions for how the user should input plays.
; Parameters: None.
; Return value: T.
; Local variables: None.
; Assistance received: None.
;***************************************************************************
(defun printInputInstructions ()
  (terpri)
  (printMessage "How to submit a play:")
  (printMessage "To pass, type Pass.")
  (printMessage "To quit without saving, type Quit.")
  (printMessage "To save the game, type Save.")
  (printMessage "To get help, type Help.")
  (printMessage "To place a domino, type (<Side> <left pip> <right pip>).")
  (printMessage "Side may equal L or R, left pip and right pip must each be 1-6.")
  (terpri))

; -----------------Gameboard functions-------------------

;***************************************************************************
; Function Name: modifyGameboard
; Purpose: To modify the gameboard.
; Parameters: 
    ; domino, a list representing a single domino.
    ; vector, a list of lists holding the gameboard.
    ; direction, the side of the board in which the domino is to be placed.
; Return value: 
    ; A list of domnioes representing the gameboard with the given domino
    ; added to the given direction (side).
; Local variables: None.
; Algorithm:
    ; 1. If vector is null, simply add nothing to the gameboard.
    ; 2. If the direction is set to left, modify the gameboard accordingly.
    ; 3. If the direction is set to right, modify the gameboard accordingly.
; Assistance received: None.
;***************************************************************************

(defun modifyGameboard(domino vector direction)

    ; If the side to play on is left, or it is the first domino to be placed.
    (cond ((or (equal direction `L) (null vector))

            ; If it is not the first domino on the gameboard,
            ; check for need to transpose tile.
            (cond ((equal (first domino) (first(first vector)))
                      (let ((leftPip (first domino))
                            (rightPip (first (rest domino))))
                      (cons (list rightPip leftPip) vector)))
            (t 
              (cons domino vector))))

          ; If the side to play on is right, check for need to transpose tile.
          ((equal direction `R)
            (cond ((equal (first (rest domino)) (first(rest(first(last vector)))))
                      (let ((leftPip (first domino))
                            (rightPip (first (rest domino))))
                      (append vector (list (list rightPip leftPip)))))
            (t 
              (append vector (list domino)))))
          (t)))

;***************************************************************************
; Function Name: displayGameboard
; Purpose: To print the gameboard.
; Parameters: 
    ; vector, a list of lists holding the gameboard.
; Return value: None.
; Local variables: None.
; Assistance received: None.
;***************************************************************************
(defun displayGameboard (vector)
  (terpri)
  (terpri)
  (printMessage "The current gameboard:")
  (terpri)
  (princ "         ")
  (printDoubles (getPositionList vector () 1) (getDoublesList vector ()) ())
  (terpri)
  (princ "          ")
  (princ "L ")
  (printGameboardDomnioes vector)
  (princ "R")
  (terpri)
  (princ "         ")
  (printDoubles (getPositionList vector () 1) (getDoublesList vector ()) ())
  (terpri)
  (terpri))

;***************************************************************************
; Function Name: printGameboardDominoes
; Purpose: To print dominoes of the gameboard in the proper format.
; Parameters: 
    ; gameboard, a list of lists holding the gameboard.
; Return value: None.
; Local variables: None.
; Algorithm:
    ; 1. If the gameboard is empty, return nothing.
    ; 2. If the domino to be printed is a double:
        ; a. Print | instead of the domino.
    ; 3. Otherwise, print the domino.
    ; 4. Call printGameboardDominoes recursively.
; Assistance received: None.
;***************************************************************************

(defun printGameboardDomnioes (gameboard)
  (cond ((null gameboard))
        ((determineIfDouble (first gameboard))
            (princ "| ")
            (printGameboardDomnioes (rest gameboard)))
        (t 
          (printDomino (first gameboard))
          (princ " ")
          (printGameboardDomnioes (rest gameboard)))))

;***************************************************************************
; Function Name: getPositionList
; Purpose: To create a list of the positions of doubles on the gameboard.
; Parameters: 
    ; gameboard, a list of lists holding the gameboard.
    ; positionList, a list holding the positions of doubles.
    ; position, the current position on the gameboard.
; Return value: None.
; Local variables: None.
; Algorithm:
    ; 1. If the gameboard is null, return the positionList.
    ; 2. Otherwise, recursively traverse the gameboard.
    ; 3. Check if the domino on the gameboard is a double.
    ; 4. If so, add the position to the list.
    ; 5. Otherwise, call getPositionList recursively.
; Assistance received: None.
;***************************************************************************

(defun getPositionList (gameboard positionList position)
  (cond ((null gameboard)
            positionList)
        ((determineIfDouble (first gameboard))
            (getPositionList (rest gameboard) (append positionList (list position)) (+ position 1)))
        (t 
            (getPositionList (rest gameboard) positionList (+ position 1)))))

;***************************************************************************
; Function Name: getDoublesList
; Purpose: To create a list of the doubles on the gameboard.
; Parameters: 
    ; gameboard, a list of lists holding the gameboard.
    ; doublesList, a list holding the doubles of the gameboard.
; Return value: None.
; Local variables: None.
; Algorithm:
    ; 1. If the gameboard is null, return the doublesList.
    ; 2. Otherwise, recursively traverse the gameboard.
    ; 3. Check if the domino on the gameboard is a double.
    ; 4. If so, add the double to the list.
    ; 5. Otherwise, call getPositionList recursively.
; Assistance received: None.
;***************************************************************************

(defun getDoublesList (gameboard doublesList)
  (cond ((null gameboard)
            doublesList)
        ((determineIfDouble (first gameboard))
            (getDoublesList (rest gameboard) (append doublesList (list (first (first gameboard))))))
        (t 
            (getDoublesList (rest gameboard) doublesList))))

;***************************************************************************
; Function Name: printDoubles
; Purpose: To print the doubles of the gameboard in the proper format.
; Parameters: 
    ; positionList, a list holding the positions of doubles.
    ; doublesList, a list holding the doubles of the gameboard.
    ; previousPosition, the position of the previous double.
; Return value: None.
; Local variables: None.
; Algorithm:
    ; 1. If the positionList is null, return nothing.
    ; 2. Otherwise, if previousPosition is null:
        ; a. It is the first double to be printed.
        ; b. Calculate the amount of spaces to print.
        ; c. Print the spaces and the double value.
    ; 3. If neither of the above cases:
        ; a. Calculate the amount of spaces to print.
        ; b. Print the spaces and the double value.
    ; 4. Call printDoubles recursively.
; Assistance received: None.
;***************************************************************************

(defun printDoubles (positionList doublesList previousPosition)
  (cond ((null positionList))
        ((null previousPosition)
            (let* ((position (first positionList))
                  (double (first doublesList))
                  (formattedPosition (+ (* (- position 1) 4) 3)))
            (printSpaces 0 formattedPosition)
            (princ double)
            (printDoubles (rest positionList) (rest doublesList) position)))
        (t 
            (let* ((position (first positionList))
                    (double (first doublesList))
                    (width (* (- position 1) 4))
                    (previousWidth (+ (* (- previousPosition 1) 4) 3))
                    (formattedPosition (- width previousWidth)))             
              (printSpaces 0 formattedPosition)
              (princ double)
              (printDoubles (rest positionList) (rest doublesList) position)))))

;***************************************************************************
; Function Name: printSpaces
; Purpose: To print a given number of spaces.
; Parameters: 
    ; min, minimum number to print.
    ; max, maximum number to print.
; Return value: None.
; Local variables: None.
; Assistance received: None.
;***************************************************************************

(defun printSpaces (min max)
  (cond ((< min max)
            (princ " ")
            (printSpaces (+ min 1) max))
        (t )))

;***************************************************************************
; Function Name: displayGameComponents
; Purpose: To display the various components of Longana.
; Parameters: 
    ; tournamentScore, the score of the tournament.
    ; computerScore, the computer's current score.
    ; humanScore, the human player's current score.
    ; gameboard, the current layout/gameboard.
    ; nextPlayer, the next player to play.
; Return value: None.
; Local variables: None.
; Assistance received: None.
;***************************************************************************

(defun displayGameComponents (gameboard tournamentScore computerScore computerHand humanScore humanHand nextPlayer boneyard playerPassed)
  (printMessage "--------------------------------------------------------")
  (displayGameboard gameboard)
  (terpri)
  (princ "Tournament Score: ")
  (printMessage tournamentScore)
  (princ "Human Score: ")
  (printMessage humanScore)
  (princ "Human Hand: ")
  (printDominoes humanHand)
  (terpri)
  (princ "Computer Score: ")
  (printMessage computerScore)
  (princ "Computer Hand: ")
  (printDominoes computerHand)
  (terpri)
  (princ "Boneyard: ")
  (printDominoes boneyard)
  (terpri)
  (princ "Previous Player Passed: ")
  (cond ((null playerPassed)
            (princ "No"))
        (t 
            (princ "Yes")))
  (terpri)
  (princ "Next Player: ")
  (cond 
    ((equal nextPlayer `Human)
      (printMessage "Human"))
    ((equal nextPlayer `Computer)
      (printMessage "Computer"))
    (t 
      (printMessage "To be decided...")))
  (terpri))

; -----------------Serialization Functions-------------------

;***************************************************************************
; Function Name: saveGame
; Purpose: To save the current game to a text file in the proper format.
; Parameters: 
    ; tournamentScore, the score of the tournament.
    ; computerHand, the computer's current hand.
    ; computerScore, the computer's current score.
    ; humanHand, the human player's current hand.
    ; humanScore, the human player's current score.
    ; gameboard, the current layout/gameboard.
    ; playerPassed, whether or not the previous player passed their turn.
    ; nextPlayer, the next player to play.
; Return value: None.
; Local variables: None.
; Assistance received: None.
;***************************************************************************

(defun saveGame (tournamentScore roundNum computerHand computerScore humanHand humanScore gameboard boneyard playerPassed nextPlayer)
  (terpri)
  (printMessage "Saving the file as savedGame.txt")
  (with-open-file(filepath "/Users/KyleCalabro/documents/college/senior/opl/savedGame.txt" :direction :output :if-exists :supersede :if-does-not-exist :create)
  (format filepath "(~%")
  (format filepath "   ")
  (format filepath "~a" tournamentScore)
  (format filepath "~&")
  (format filepath "~%")
  (format filepath "   ")
  (format filepath "~a" roundNum)
  (format filepath "~&")
  (format filepath "~%")
  (format filepath "   (~%")
  (format filepath "   ")
  (format filepath "   ")
  (format filepath "~a" computerHand)
  (format filepath "~&")
  (format filepath "~%")
  (format filepath "   ")
  (format filepath "   ")
  (format filepath "~a" computerScore)
  (format filepath "~&")
  (format filepath "   )~%")
  (format filepath "~%")
  (format filepath "   (~%")
  (format filepath "   ")
  (format filepath "   ")
  (format filepath "~a" humanHand)
  (format filepath "~&")
  (format filepath "~%")
  (format filepath "   ")
  (format filepath "   ")
  (format filepath "~a" humanScore)
  (format filepath "~&")
  (format filepath "   )~%")
  (format filepath "~%")
  (format filepath "   ")
  (convertBoardToSave filepath gameboard)
  (format filepath "~%")
  (format filepath "~%")
  (format filepath "   ")
  (format filepath "~a" boneyard)
  (format filepath "~%")
  (format filepath "~%")
  (format filepath "   ")
  (cond ((equal playerPassed `Y)
          (format filepath "(t)"))
        (t 
          (format filepath "()")))
  (format filepath "~%")
  (format filepath "~%")
  (format filepath "   ")
  (cond ((equal nextPlayer `C)
          (format filepath "Computer~%"))
        (t 
          (format filepath "Human~%")))
  (format filepath ")")))

;***************************************************************************
; Function Name: convertBoardToSave
; Purpose: Converts the gameboard to the proper format for serialization.
; Parameters: 
    ; filepath, the pathname of the file to write to.
    ; gameboard, the current gameboard for Longana.
; Return value: 
    ; Prints to the given file the gameboard in the proper format.
; Local variables: 
    ; gameboard, the gameboard for longana with `L added to the front.
    ; modifyGameboard, the gameboard with `R added to the end.
; Assistance received: none
;***************************************************************************

(defun convertBoardToSave (filepath gameboard)
  (let ((gameboard (cons `L gameboard)))
      (let ((modifyGameboard (append gameboard (list `R))))
            (format filepath "~a" modifyGameboard))))
  
;***************************************************************************
; Function Name: restoreBoard
; Purpose: Converts the gameboard to the proper format for playing Longana
           ; from serialization.
; Parameters: 
    ; vector, a list representing the gameboard from the serialized file.
; Return value: 
    ; A list representing the gameboard for Longana.
; Local variables: 
    ; gameboard, the gameboard for longana with `R removed.
    ; gameboardOG, the gameboard with `L and `R removed.
; Assistance received: none
;***************************************************************************

(defun restoreBoard (vector)
  (let* ((gameboard (remove `R vector :test #'equal))
        (gameboardOG (remove `L gameboard :test #'equal)))
  gameboardOG))

;***************************************************************************
; Function Name: loadFile
; Purpose: Intermediate step in resuming a game, loads a file and collects data.
; Parameters: None.
; Return value: T
; Local variables: 
    ; data, the block of data read in from the file.
; Assistance received: none
;***************************************************************************

(defun loadFile ()
  (printMessage "Select an option below:")
  (printMessage "1). /Users/KyleCalabro/documents/college/senior/opl/case1.txt")
  (printMessage "2). /Users/KyleCalabro/documents/college/senior/opl/case2.txt")
  (printMessage "3). /Users/KyleCalabro/documents/college/senior/opl/case3.txt")
  (printMessage "4). /Users/KyleCalabro/documents/college/senior/opl/savedGame.txt")
  (printMessage "5). Enter your own filepath.")
  (terpri)

  ; Give the user a variety of options to save themselves some time if possible.
  (let* ((choice (askForInput "Your choice:" 1 5)))
    (cond ((equal choice 1)
              (let* ((filePath (open "/Users/KyleCalabro/documents/college/senior/opl/case1.txt"))
                    (data (read filePath)))
                      (close filePath)
                      data))
          ((equal choice 2)
              (let* ((filePath (open "/Users/KyleCalabro/documents/college/senior/opl/case2.txt"))
                    (data (read filePath)))
                      (close filePath)
                      data))
          ((equal choice 3)
              (let* ((filePath (open "/Users/KyleCalabro/documents/college/senior/opl/case3.txt"))
                    (data (read filePath)))
                      (close filePath)
                      data))
          ((equal choice 4)
              (let* ((filePath (open "/Users/KyleCalabro/documents/college/senior/opl/savedGame.txt"))
                    (data (read filePath)))
                      (close filePath)
                      data))
          (t 
            (printMessage "Enter your pathname in double quotations: ")
              (let ((pathname (read)))
                (let* ((filePath (open pathname))
                        (data (read filePath)))
                          (close filePath)
                          data))))))

;***************************************************************************
; Function Name: loadFile
; Purpose: Intermediate step in resuming a game, loads a file and collects data.
; Parameters: 
    ; dataBlock, the block of data read in from the file.
; Return value: T
; Local variables: 
    ; computerScore and humanScore, the scores of each player.
    ; tournamentScore, the score of the tournament.
    ; humanHand, the human player's hand.
    ; computerHand, the computer player's hand.
    ; boneyard, the boneyard for longana.
    ; firstPlayer, the first player of the round (engine holder).
    ; roundNum, the current round number.
    ; nextPlayer, the next player in the round.
    ; engine, the engine for the current round.
    ; humanData, block of data for the human player.
    ; computerData, block of data for the computer player.
    ; gameboardSerialized, the gameboard as loaded in from the file.
    ; playerPassed, whether or not the previous player passed their turn.
; Assistance received: none
;***************************************************************************

(defun resumeGame (dataBlock)
  (cond ((not dataBlock)
          (printMessage "Sorry, there was an error reading the file, try again.")
          (longana))
        (t
          (printMessage "Loading the file!")

          ; Assign values from the data in the file.
          (let* ((tournamentScore (first dataBlock))
                  (roundNum (first (rest dataBlock)))
                  (computerData (first (rest (rest dataBlock))))
                  (humanData (first (rest (rest (rest dataBlock)))))
                  (gameboardSerialized (first (rest (rest (rest (rest dataBlock))))))
                  (boneyard (first (rest (rest (rest (rest (rest datablock)))))))
                  (playerPassed (first (rest (rest (rest (rest (rest (rest datablock))))))))
                  (nextPlayer (first (rest (rest (rest (rest (rest (rest (rest datablock)))))))))
                  (computerHand (first computerData))
                  (computerScore (first (rest computerData)))
                  (humanHand (first humanData))
                  (humanScore (first (rest humanData)))
                  (gameboard (restoreBoard gameboardSerialized))
                  (engine (determineEngine roundNum)))
                  (displayGameComponents gameboard tournamentScore computerScore computerHand humanScore humanHand nextPlayer boneyard playerPassed)

           ; Check that the tournament score has not already been reached.
           (cond ((> computerScore tournamentScore)
                    (printMessage "The computer has won the tournament!")
                    (princ "Tournament Score: ")
                    (princ tournamentScore)
                    (terpri)
                    (princ "Computer Score: ")
                    (princ computerScore)
                    (terpri)
                    (princ "Human Score: ")
                    (princ humanScore)
                    (terpri))
                 ((> humanScore tournamentScore)
                    (printMessage "The human player has won the tournament!")
                    (princ "Tournament Score: ")
                    (princ tournamentScore)
                    (terpri)
                    (princ "Computer Score: ")
                    (princ computerScore)
                    (terpri)
                    (princ "Human Score: ")
                    (princ humanScore)
                    (terpri))

                 ; If the gameboard is empty, the engine has not been dealt yet.
                 ((null gameboard)
                    (let* ((firstPlayer (determineFirstPlayer humanHand computerHand engine)))
                    (setUpRound gameboard humanScore computerScore tournamentScore humanHand computerHand boneyard firstPlayer engine roundNum)))

                 ; Determine who the next player is and the conditions for the round.
                 ((null playerPassed)
                    (cond ((equal nextPlayer `Human)
                      (playRound gameboard humanScore computerScore tournamentScore humanHand `N computerHand `N boneyard `H roundNum))
                    (t 
                      (playRound gameboard humanScore computerScore tournamentScore humanHand `N computerHand `N boneyard `C roundNum))))
                  (t 
                      (cond ((equal nextPlayer `Human)
                              (playRound gameboard humanScore computerScore tournamentScore humanHand `N computerHand `Y boneyard `H roundNum))
                            (t 
                              (playRound gameboard humanScore computerScore tournamentScore humanHand `Y computerHand `N boneyard `C roundNum)))))))))

; -----------------Misc. Usage Functions-------------------

;***************************************************************************
; Function Name: printMessage
; Purpose: To print a message to the window.
; Parameters: 
		; message, the message that will be printed.
; Return value: T
; Local variables: None.
; Algorithm: 
		; 1. Call princ on the message passed into the function.
; Assistance received: none
;***************************************************************************

(defun printMessage (message)
  (princ message)
  (terpri))

;***************************************************************************
; Function Name: askForInput
; Purpose: To ask the user for (valid) input.
; Parameters: 
		; message, the message to print before asking for input.
		; minNum, the minimum number for valid input.
		; maxNum, the maximum number for valid input.
; Return value: T
; Local variables: None.
; Algorithm:
		; 1. Print the message that goes with the input
		; 2. Call validateInput on the same parameters of the function but also 
    ;    include read to actually get the input.
; Assistance received: None.
;***************************************************************************

(defun askForInput (message minNum maxNum)
  (princ message)
  (terpri)
  (validateInput (read) message minNum maxNum))

;***************************************************************************
; Function Name: validateInput
; Purpose: To validate the input passed into the parameters.
; Parameters: 
		; message, the message to print before asking for input (if it needs to ask again). 
		; minNum, the minimum number for valid input.
		; maxNum, the maximum number for valid input, and input, the actual input to check.
    ; input, the user's input.
; Return value: 
		; The number that the user input into the function.
; Local variables: None.
; Algorithm:
		; 1. If input is less than minNum, ask for input again.
		; 2. If input is greater than maxNum, ask for input again.
		; 3. Otherwise, it's valid input. Return the number.
; Assistance received: None.
;***************************************************************************

(defun validateInput (input message minNum maxNum)
  (cond ((not (integerp input))
          (printMessage "Sorry that format was incorrect, try again.")
          (terpri)
          (longana))
        ((< input minNum)
         	(askForInput message minNum maxNum))
        ((> input maxNum)
         	(askForInput message minNum maxNum))
        (t
         	input)))
