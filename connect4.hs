{- 
A Board is a list (not tuple for comprhension reasons) of Columns from left to right (7 of them)
A column is a list off booleans representing if its our token. Bottom up. If empty, its empty (max 6)
-}
module Connect4 where

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

evaluateBoard board = evaluateDiagonals board + evaluateColumns board + evaluateRows board

addMove board col bool = let (x,y:ys) = splitAt col board -- ik im inconsistent with if rows start from 0 or 1 its cause of the possibleNextBoards if statement
                    in x ++ [y ++ [bool]] ++ ys
possibleNextBoards board bool = if evaluateBoard board /= 0 then [] else [addMove board x bool | x <- [0 .. 6], length (board !! x) < 6]


getBestMoveFromList (x:xs)
    | xs == [] = x
    | otherwise =   let
                        (move, cost) = x
                        (move2, cost2) = getBestMoveFromList xs
                    in if max cost cost2 == cost then (move, cost) else (move2, cost2)

getWorstMoveFromList (x : xs) --aka MIN
    | xs == [] = x
    | otherwise =
        let (move, cost) = x
            (move2, cost2) = getWorstMoveFromList xs
        in if min cost cost2 == cost then (move, cost) else (move2, cost2)

-- The above two functions are dumb and will never be used because MiniMax is depth first not breadth first. I spent time writing them out because im a moron
-- On second thoughts im about to implement them into the code below. I was stupid for thinking i wouldnt need them. I remain a moron

getWorstMove (move : xs) depth
    | cost == (-1) = (move, cost)
    | xs == [] = (move, cost)
    | otherwise =
        let (move2, cost2) = getWorstMove xs depth
        in if min cost cost2 == cost then (move, cost) else (move2, cost2)
    where cost = snd (searchForMove move True (depth + 1))

getBestMove (move : xs) depth
    | cost == (1) = (move, cost) -- le alpha pruning (i think)
    | xs == [] = (move, cost)
    | otherwise =
        let (move2, cost2) = getBestMove xs depth
        in if max cost cost2 == cost then (move, cost) else (move2, cost2)
    where cost = snd (searchForMove move False (depth+1))


searchForMove board isMax depth
    | possibleNextBoards board isMax == [] || depth >= 6 = (board, evaluateBoard board)
    | isMax = getBestMove (possibleNextBoards board True) depth
    | otherwise = getWorstMove (possibleNextBoards board False) depth

-- realised isMax is useless cause we can do MOD 2

play board = searchForMove board True 0


boardToString board = "-------\n" ++ [if col == 7 then '\n' else if length (board!!col) <= row then ' ' else if board!!col!!row then 'X' else '0' | row <- [5,4..0], col <- [0..7]] ++ "-------"
playerMove board column = addMove board (column-1) False

emptyBoard = [[], [], [], [], [], [], []]

t = True -- peak laziness
f = False
testBoard1 = [[True], [False, True], [True, False,False], [True, False, True, False], [False,True,True], [False,True,False,True,False], [True]]
testBoard2 = [[t,t,f], [t, f], [t,f,t], [t,f,f,t,f], [t,t,f,f], [t,f,f], [f,f]]

testBoard3 = [[t], [f, t, f, t], [f], [f], [t], [f, t], [t, f]] -- win in 3 https://sites.math.rutgers.edu/~zeilberg/C4/ch3/Problems.html
testBoard4 = [[], [f,t,f],[t,t,t], [f,t,f], [f], [t,f,f], [t,t,f]] -- easy win in 1 
testBoard5 = [[f,t,t], [t,f], [f,t], [t,f], [f,f], [], [t,f,t]] -- in 2

testBoard6 = [[t,t], [f], [t,t,f], [f], [f,t], [f], [t,f,t,f]] -- in 2

testBoard7 = [[t], [t], [f], [f, t, f, t], [f], [t], [f,f,t]] -- in 4 https://sites.math.rutgers.edu/~zeilberg/C4/ch3/P7.html
testBoard8 = [[t, f, t, t], [f], [f, t], [f, f, t], [t], [t, f, f], []] -- in 5 https://sites.math.rutgers.edu/~zeilberg/C4/ch5/P2.html