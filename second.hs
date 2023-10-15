{-# LANGUAGE ParallelListComp #-}
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

-- ok im using parallel generators, ik we haven't formally learnt it but idk what else to use
-- i cant use zip because what happens if the first column has no items y'know

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

possibleMoves x = ()

emptyBoard = [[], [], [], [], [], [], []]

testBoard1 = [[True], [False, True], [True, False,False], [True, False, True, False], [False,True,True], [False,True,False,True,False], [True]]
