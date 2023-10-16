import Connect4
import System.Exit


main :: IO ()

win = do
    putStrLn "You win"
    die "Idk how to end it properly"

lose = do
    putStrLn "You Loose L"
    die "Idk how to end it properly"

move board column = do
    putStrLn ("You chose column " ++ show column)
    let (newBoard) = Connect4.playerMove board column
    putStrLn (Connect4.boardToString newBoard)
    let newnewBoard = fst (Connect4.play newBoard)
    let cost = Connect4.evaluateBoard newnewBoard
    putStrLn (Connect4.boardToString newnewBoard)
    if cost == 1 then lose else (if cost == (-1) then win else gameLoop newnewBoard)

gameLoop board = do
    putStrLn "Enter a column (1-7):"
    columnStr <- getLine
    case readMaybe columnStr of
        Just column ->  if column `elem` [1..7]
                        then move board column
                        else putStrLn "Invalid column. Please enter a number between 1 and 7."
        Nothing -> putStrLn "Invalid input. Please enter a valid number."

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

main = do
    print "Welcome to my shitty connect 4 thing"
    gameLoop Connect4.emptyBoard
