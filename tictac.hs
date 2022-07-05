--Original
--https://github.com/willdoescode/TicTacHs
-- module Main where
import Data.Foldable 
import Text.Read
import Data.Sequence hiding (replicate, null)
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

newBoard = [[Zero, One, Two], [Three, Four, Five], [Six, Seven, Eight]]

alternatePlayer :: Move -> Move
alternatePlayer X = O
alternatePlayer O = X
alternatePlayer Blank = X

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (Prelude.filter (\y -> x /= y) xs)

flatten :: [[a]] -> [a]
flatten arr = [y | x <- arr, y <- x]            

isTied :: Eq a => [[a]] -> Bool
isTied b =
   Data.Foldable.length (nub $ flatten b) <= 2

checkRow :: Row -> Bool
checkRow row
  | all (== X) row = True
  | all (== O) row = True
  | otherwise = False   

hasWinner :: Board -> Bool
hasWinner [[a, b, c], [d, e, f], [g, h, i]] =
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

--why cant i just do this? something about seq? confused :(
-- replace index el lst = update index el lst 
replace :: Int -> a -> [a] -> [a]
replace index el lst = toList $ update index el $ fromList lst  

changeElem :: (Int, Int) -> Move -> [[Move]] -> [[Move]]
changeElem (x, y) move board =
  let row = board !! x
      newRow = replace y move row
   in if row !! y == X || row !! y == O
      then board 
      else replace x newRow board

getCell :: Int -> (Int, Int)
getCell n = (x, y)
  where
    x = quot n 3
    y = n - (x * 3)
    
getLineInt :: IO Int
getLineInt = do
      putStrLn "enter cell to change"
      line <- getLine
      case readMaybe line of
            Just x -> do
               return x
            Nothing -> do
               putStrLn "Invalid number entered"
               x <- getLineInt
               return x

showRow :: Row -> String
showRow row =
  show (head row)
    ++ " | "
    ++ show (row !! 1)
    ++ " | "
    ++ show (row !! 2)
    ++ "\n"
    ++ "-   -   -"
    ++ "\n"              

--why does it give both of these? depening if i comment out type signature and :t showBoard
-- showBoard :: [Row] -> [Char]
-- showBoard :: Foldable t => t Row -> [Char]
showBoard board = foldl (\s row -> s ++ showRow row) "" board              

runGame :: Play -> IO ()
runGame Play {current = curr, board = b} = do
  putStrLn "\n"
  putStr $ showBoard b
  putStrLn $ "Current Player: " ++ show curr

  cellNumber <- getLineInt
  let cell = getCell cellNumber 
  let newBoard = changeElem cell curr b

  let action | cellNumber > 8 = runGame Play {current = curr, board = b } -- this feels wrong, becuase let newBoard returns an error. 
             | newBoard == b = runGame Play {current = curr, board = b } --if they choose a cell thats already taken 
             | hasWinner newBoard = do putStrLn $ show curr ++ " Has won!"
             | isTied newBoard = putStrLn "Their was a tie, run main to play again"
             | otherwise = runGame Play {current = alternatePlayer curr, board = newBoard}  
  action

main :: IO ()
main = runGame Play {current = X, board = newBoard}

-- "Yes"