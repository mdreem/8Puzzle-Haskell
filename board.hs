type Tile = Maybe Integer
type Board = [[Tile]]
type Pos = (Int, Int)

testBoard::Board
testBoard = [[Just 1,Just 2,Just 3],[Just 4,Just 5,Just 6],[Just 7,Just 8, Nothing]]

-- returns the tile at the given position
getTile :: Board -> Pos -> Tile
getTile board (pos1, pos2) = board!!pos1!!pos2

-- checks if the positions match the size of the board
allowedPos :: Int -> Pos -> Bool
allowedPos size (pos1, pos2) = pos1 >= 0 && pos2 >= 0 && pos1 < size && pos2 < size

-- returns the movements in the four directions around a given position
possiblePositions :: Pos -> [Pos]
possiblePositions (pos1, pos2) = [ (pos1 - 1, pos2), (pos1, pos2-1), (pos1 + 1, pos2), (pos1, pos2 + 1) ]

-- returns possible movements of a tile
getPossiblePositions :: Pos -> Int -> [Pos]
getPossiblePositions pos size = filter (allowedPos size) (possiblePositions pos)

-- get a list of Boards with possible movements of a certain tile
neighbours :: Pos -> Board -> [Board]
neighbours pos board = map (flipTiles board pos) positions
	where 
		len = length board
		positions = getPossiblePositions pos len

flipTiles :: Board -> Pos -> Pos -> Board
flipTiles board pos1 pos2 = setMatrixValue pos1 (setMatrixValue pos2 board val1) val2
	where 
		val1 = getTile board pos1
		val2 = getTile board pos2
		
-- get manhattan distance of one tile to its final position
manhattanTile :: Tile -> Int -> Pos -> Int
manhattanTile Nothing _ _ = 0
manhattanTile tile size (oPosI, oPosJ) = abs(oPosI - posI) + abs(oPosJ - posJ) 
	where
		(posI, posJ) = tilePosition tile size

-- get manhattan distance of the whole board
manhattanBoard :: Board -> Int
manhattanBoard board = sum distBoard 
	where
		size = length board
		distBoard = [ manhattanTile (getTile board (i, j)) size (i, j) | i <- [0 .. size - 1], j <- [0 .. size - 1]]	
		
-- given the value of a tile, return its position
tilePosition :: Tile -> Int -> Pos
tilePosition Nothing _ = (-1, -1)
tilePosition (Just tile) size = (finPosI, finPosJ)
	where 
		intTile = toInteger tile
		intSize = toInteger size
		finPosI = fromIntegral(quot (intTile - 1) intSize)
		finPosJ = fromIntegral(mod (intTile - 1) intSize)
	
pos1 (posA, posB) = posA
		
-- Takes the position of the element to change, the list and a function applied to the element we
-- want to change
setRow :: Int -> [a] -> (a -> a) -> [a]
setRow _ [] _ = []
setRow 0 (x:xs) f = (f x) : xs
setRow pos (x:xs) f = x : setRow (pos - 1) xs f

setRowValue :: Int -> [a] -> a -> [a]
setRowValue pos xs val = setRow pos xs (const val)

setMatrix :: Pos -> [[a]] -> (a -> a) -> [[a]]
setMatrix (pos1, pos2) matrix f = setRow pos1 matrix (\row -> setRow pos2 row f)

setMatrixValue :: Pos -> [[a]] -> a -> [[a]]
setMatrixValue pos matrix val = setMatrix pos matrix (const val)

-- functions for printing the board to the screen

printBoardRow :: [Tile] -> String
printBoardRow []= ""
printBoardRow (x:xs) = printTile x ++ " " ++ printBoardRow xs

printBoard :: Board -> String
printBoard [] = ""
printBoard (x:xs) = (printBoardRow x) ++ "\n" ++ printBoard xs

printTile :: Tile -> String
printTile (Just a) = show a
printTile Nothing = "X"

printBoardList :: [Board] -> String
printBoardList [] = ""
printBoardList (x:xs) = (printBoard x) ++ "\n" ++ printBoardList xs
