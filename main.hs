import Connect4

main :: IO()

gameLoop board = do
    print("Please enter a Column (1-7)")
    --col <- getLine
    --Connect4.playerMove board (read col:: Int)


main = do
    
    print("Please enter a Column (1-7)")
-- taking user input using getLine function
    line <- getLine
-- typecasting string to integer
    let number = (read line:: Int)
-- printing the number
    --Connect4.playerMove Connect4.emptyBoard number
    --Fuck it idk