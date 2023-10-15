{- 
A Board is a list (not tuple for comprhension reasons) of Columns from left to right (7 of them)
A column is a list off booleans representing if its our token. Bottom up. If empty, its empty (max 6)
-}


consecutiveBoolsInARow [] _ _ = 0

-- where (x:xs) is the list, i is the current count, and bool is the boolean its counting
consecutiveBoolsInARow (x:xs) i bool
    | xs == [] && x == bool = i + 1
    | xs == [] = i
    | x == bool = consecutiveBoolsInARow xs (i+1) bool
    | otherwise = max (consecutiveBoolsInARow xs 0 bool) i

has4InARow x bool = consecutiveBoolsInARow x 0 bool >= 4
evaluationFromList x
    | has4InARow x False = -1
    | has4InARow x True = 1
    | otherwise = 0

evaluateColumns board = sum [ evaluationFromList x | x <- board] -- we should never have two winners at once


--evaluateRows board = [[(col1, col2, col3, col4, col5, col6, col7)] | col1 <- board !! 0 | col2 <- board !! 1 | col3 <- board !! 2 | col4 <- board !! 3 | col5 <- board !! 4 | col6 <- board !! 5 | col7 <- board !! 6 ]
splitOneRow board row split
    | board == [] = if split /= [] && last split == [] then init split else split
    | length (head board) < row = splitOneRow (tail board) row (if split /= [] && last split /= [] then split ++ [[]] else split )
    | split == [] = splitOneRow (tail board) row [[head board!!(row-1)]]
    | otherwise = splitOneRow (tail board) row (init split ++ [last split ++ [head board!!(row-1)]])


-- myMaximum [] = 0
-- myMaximum (x : xs) = max (head x) (myMaximum xs)

-- longestColumn board = myMaximum [length x | x <- board]


splitRowsIntoMiniRows board = concat [splitOneRow board r [] | r <- [1 .. 6]] -- not longestColumn cause error idk how to fix
evaluateRows board = sum [evaluationFromList x | x <- splitRowsIntoMiniRows board]


splitDiagonal board startColumn startRow splits
    | startRow == 7 = splits
    | startColumn == 8 = splits
    | length (board!!(startColumn-1)) < startRow = splitDiagonal board (startColumn+1) (startRow+1) (splits ++ [[]])
    | splits == [] = splitDiagonal board (startColumn+1) (startRow+1) [[board!!(startColumn-1)!!(startRow-1)]]
    | otherwise = splitDiagonal board (startColumn+1) (startRow+1) (init splits ++ [last splits ++ [board!!(startColumn-1)!!(startRow-1)]])

splitBottomDiagonals board = concat [splitDiagonal board x 1 [] | x <- [1..7]]
splitSideDiagonals board = concat [splitDiagonal board 1 x [] | x <- [2 .. 6]]

-- ok btm right to top left in terms off starting point
splitDiagonalsIntoLists board = let revBoard = reverse board
                                in splitBottomDiagonals board ++ splitSideDiagonals board ++ splitBottomDiagonals revBoard ++ splitSideDiagonals revBoard

evaluateDiagonals board = sum [evaluationFromList x | x <- splitDiagonalsIntoLists board]

evaluateBoard board = evaluateDiagonals + evaluateColumns + evaluateRows

addMove board col = let (x,y:ys) = splitAt col board -- ik im inconsistent with if rows start from 0 or 1 its cause of the possibleNextBoards if statement
                    in x ++ [y ++ [True]] ++ ys
possibleNextBoards board = [addMove board x | x <- [0 .. 6], length (board !! x) < 6]


emptyBoard = [[], [], [], [], [], [], []]

t = True -- peak laziness
f = False
testBoard1 = [[True], [False, True], [True, False,False], [True, False, True, False], [False,True,True], [False,True,False,True,False], [True]]
testBoard2 = [[t,t,f], [t, f], [t,f,t], [t,f,f,t,f], [t,t,f,f], [t,f,f], [f,f]]