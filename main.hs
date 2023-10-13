
{- 
Board is going to be bitboard
0x1 Available to place (or not)
0x2 Red/White  (if its available the second bit should not matter) (i think)
Ok fuck that idea icl
Lets just use arrays
Nah but even if we use like 3 bits per place still more efficient init
Ok bits are displayed as column by column
shut the fuck up ik this is ineffeicite
ok better idea

each row is a byte (actually 7 bits but stfu)
first bit is if its empty
nah nvm

uhhhhh

Each "place" 
0x1 empty
0x2 isUs
(the first idea)
columns first, then rows
-}
import Data.Bits

emptyBoard =  0

getOptions board = 0110

