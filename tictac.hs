--Original
--https://github.com/willdoescode/TicTacHs
module Main where
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

myNewBoard = [[Zero, One, Two], [Three, Four, Five], [Six, Seven, Eight]]

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

-- changeElemV2 (x, y) move board =  
--   let row = board !! x
--       if (row !! y) == X
--         then board
--         else do          
--           newRow = replace y move row
--           in replace x newRow board

isSymbol s = 
  if s == X || s == O   
    then True
    else False


-- main = do   
--    let var = 23 
--    if var `rem` 2 == 0 
--       then putStrLn "Number is Even" 
--    else putStrLn "Number is Odd"

-- if (row !! y) == X || O
--   then board
--   else newRow = replace y move row
--    in replace x newRow board  

-- changeElemV2 (x, y) move board =
--   let row = board !! x
--       cell = row !! y
--   if cell == X 
--     then board
--   else newRow = replace y move row
--    in replace x newRow board   

alternatePlayer :: Move -> Move
alternatePlayer X = O
alternatePlayer O = X
alternatePlayer Blank = X

getCell :: Int -> (Int, Int)
getCell n = (x, y)
  where
    x = quot n 3
    y = n - (x * 3)
    


getLineInt :: IO Int
getLineInt = do
      putStrLn "Please enter cell to change"
      line <- getLine
      case readMaybe line of
            Just x -> do
               return x
            Nothing -> do
               putStrLn "Invalid number entered"
               x <- getLineInt
               return x




runGameV3 :: Play -> IO ()
runGameV3 Play {current = curr, board = b} = do
  putStrLn "\n"
  putStr $ showBoard b

  putStrLn $ "Current Player: " ++ show curr

  cellNumber <- getLineInt
  let cell = getCell cellNumber 
  let newBoard = changeElem cell curr b

  if checkWin newBoard
    then do
      putStrLn $
        show curr
          ++ " Has won!"
      exitSuccess
    else
      if not $ checkTieV2 (flatten myNewBoard) $ flatten myNewBoard
        then do
          putStrLn "There was a tie :("
          exitSuccess
        else runGameV3 Play {current = alternatePlayer curr, board = newBoard}  

main :: IO ()
main = runGameV3 Play {current = X, board = myNewBoard}


--plan 
--if its either X or O return board
-- else return newBoard  


