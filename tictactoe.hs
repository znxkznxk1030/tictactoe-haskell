{-# LANGUAGE RecordWildCards #-}
import System.Environment
import Data.List

data Player = PlayerX | PlayerO
data State = Done | Progress
data Cell = X | O | Empty
type Row = [Cell]
type Board = [Row]

instance Show Player where
  show PlayerX = "player [X]"
  show PlayerO = "player [O]"

instance Eq Player where
  PlayerX == PlayerX = True
  PlayerO == PlayerO = True
  _ == _ = False

instance Show Cell where
  show Empty = "   "
  show X = " X "
  show O = " O "

instance Eq Cell where
  X == X = True
  O == O = True
  Empty == Empty = True
  _ == _ = False

data GameState = GameState { board :: Board, active :: Player}

initialGameState :: GameState
initialGameState = GameState (replicate 3 $ replicate 3 Empty) PlayerX

updateGameState :: Board -> Player -> GameState
updateGameState = GameState

getBoard :: GameState -> Board
getBoard GameState{..} = board

getPlayer :: GameState -> Player
getPlayer GameState{..} = active

showCell :: Row -> Int -> String
showCell row idx = show $ row !! idx

divideLine :: String
divideLine = "\n-------------\n"

showRows :: Board -> [String]
showRows = map (\row -> "|" ++ showCell row 0 ++ "|" ++ showCell row 1 ++ "|" ++ showCell row 2 ++ "|")

renderBoard :: Board -> String
renderBoard board = divideLine ++ intercalate divideLine (showRows board) ++ divideLine

toCell :: Player -> Cell
toCell player
  | player == PlayerO = O
  | player == PlayerX = X
  | otherwise = Empty

move :: GameState -> Int -> Int -> Board
move game row col = 
  left ++ newMid:right
  where 
    board = getBoard game
    turn = getPlayer game
    (left, mid:right) = splitAt row board
    (lc, mc:rc) = splitAt col mid
    newMid = lc ++ toCell turn :rc

isEmpty :: Board -> Int -> Int -> Bool 
isEmpty board ridx cidx = let row = board !! ridx
                              cell = row !! cidx
                          in cell == Empty

playerMove :: GameState -> IO(Int, Int)
playerMove game = do
  let player = getPlayer game
  let board = getBoard game
  -- putStr $ renderBoard board
  putStrLn $ "Enter coords (x y | 0 <= x, y <= 2) :"
  (col : row : _) <- fmap parseInts getLine
  if (row < 0) || (row > 2) || (col < 0) || (col > 2)
    then do
      putStrLn "Bad coords. Try again."
      playerMove game
    else do
      if not $ isEmpty board row col
        then do
          putStrLn "Spot taken. Try again."
          playerMove game
        else pure(row, col)

playGame :: GameState -> IO (Player, Board)
playGame currGameState = do
  (row, col) <- playerMove currGameState
  let currPlayer = getPlayer currGameState
  let nextPlayer = if currPlayer == PlayerX
                      then PlayerO
                      else PlayerX
  let nextBoard = move currGameState row col
  clearScreen
  putStrLn.renderBoard $ nextBoard
  if won nextBoard currPlayer
    then pure(currPlayer, nextBoard)
    else do
        let nextGameState = GameState nextBoard nextPlayer
        playGame nextGameState

won :: Board -> Player -> Bool
won board player = any full $ diagonals board ++ board ++ transpose board
            where symbol = playerToSymbol player
                  full [a,b,c] = a == b && b == c && a == symbol
                  diagonals [ [a1, _, a3],
                              [_ , b2, _],
                              [c1, _, c2]] = [[a1, b2, c2], [a3, b2, c1]]


main = do
  let initGameState = initialGameState
  putStrLn.renderBoard $ getBoard initGameState
  (winner, finalBoard) <- playGame initialGameState
  putStrLn $ "winner is " ++ (show.playerToSymbol) winner ++ "!!"



playerToSymbol :: Player -> Cell
playerToSymbol player = if player == PlayerO then O else X


parseInts :: String -> [Int]
parseInts str = map read (words str)

clearScreen :: IO() 
clearScreen = putStr "\ESC[2J"
