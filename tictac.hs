--Original
--https://github.com/willdoescode/TicTacHs

module Main where

import Data.Foldable 
-- import Data.Foldable.null
import Data.Sequence hiding (replicate)
-- import Data.Set
import System.Exit (exitSuccess)

type Board = [Row]

type Row = [Move]

data Move = X | O | Blank | Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq)

data Play = Play
  { current :: Move,
    board :: Board
  }
  deriving (Show)

instance Show Move where
  show Blank = " "
  show X = "X"
  show O = "O"
  show Zero = "0"
  show One = "1"
  show Two = "2"
  show Three = "3" 
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"



-- showRow :: Row -> String
showRow row =
  show (head row)
    ++ " | "
    ++ show (row !! 1)
    ++ " | "
    ++ show (row !! 2)
    ++ "\n"
    ++ "-   -   -"
    ++ "\n"

showBoard = foldl (\s row -> s ++ showRow row) ""

myNewBoardV3 = [[Zero, One, Two], [Three, Four, Five], [Six, Seven, Eight]]

myNewBoardV4 = [[One, Two, Three], [Four, Five, Six], [Seven, Eight, Nine]]
 
checkRow :: Row -> Bool
checkRow row
  | all (== X) row = True
  | all (== O) row = True
  | otherwise = False

checkWin :: Board -> Bool
checkWin [[a, b, c], [d, e, f], [g, h, i]] =
  or
    [ checkRow [a, b, c],
      checkRow [d, e, f],
      checkRow [g, h, i],
      checkRow [a, d, g],
      checkRow [b, e, h],
      checkRow [c, f, i],
      checkRow [c, e, g],
      checkRow [a, e, i]
    ]

flatten :: [[a]] -> [a]
flatten arr = [y | x <- arr, y <- x]

-- checkTie :: Board -> Bool
checkTie = all (== False) . flatten . (map . map) (== Blank)

checkTieV2 :: Eq a => [a] -> [a] -> Bool
checkTieV2 x y = any id $ (==) <$> x <*> y

replace :: Int -> a -> [a] -> [a]
replace index el lst = toList $ update index el $ fromList lst

-- changeElem :: (Int, Int) -> Move -> Board -> Board
changeElem (x, y) move board =
  let row = board !! x
      newRow = replace y move row
   in replace x newRow board

alternatePlayer :: Move -> Move
alternatePlayer X = O
alternatePlayer O = X
alternatePlayer Blank = X

getCell :: Int -> (Int, Int)
getCell n = (x, y)
  where
    x = quot n 3
    y = n - (x * 3)

-- isacceptableAnswer? answer = 
  -- what toDo
  -- 1. cell = getCell ()

runGameV3 :: Play -> IO ()
runGameV3 Play {current = curr, board = b} = do
  putStrLn "\n"
  putStr $ showBoard b

  putStrLn $ "Current Player: " ++ show curr
  putStrLn "Enter cell to change> "
  cellNumber <- getLine
  let cell = getCell (read cellNumber :: Int)
  let newBoard = changeElem cell curr b


  if checkWin newBoard
    then do
      putStrLn $
        show curr
          ++ " Has won!"
      exitSuccess
    else
      if not $ checkTieV2 (flatten myNewBoardV3) $ flatten myNewBoardV3
        then do
          putStrLn "There was a tie :("
          exitSuccess
        else runGameV3 Play {current = alternatePlayer curr, board = newBoard}  

main :: IO ()
main = runGameV3 Play {current = X, board = myNewBoardV3}

-- reverseWords :: String -> String  
-- reverseWords = unwords . map reverse . words

-- getValidInput = do   
--     line <- getLine  
--     if null line  
--         then return ()  
--         else do  
--             putStrLn $ line  
--             getValidInput  


-- reverseWords :: String -> String  
-- reverseWords = unwords . map reverse . words              
