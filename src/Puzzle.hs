import           Board
import           Search
import           System.Environment

main = do
    [filename] <- getArgs
    loadBoard filename
    
loadBoard filename = do
    file <- readFile filename
    let fileLines = lines file
    let num = read (head fileLines) :: Int
    let board = (take num (map lineToList (tail fileLines)))
    print (solve (toPuzzleBoard board))

    
lineToList line = map (\x -> (read x :: Int)) (words line)

toPuzzleBoard :: [[Int]] -> Board
toPuzzleBoard b = [[ toTile (b!!i!!j) | j <- [0..len-1]] |  i <- [0..len-1]]
    where len = length b

toTile :: Int -> Tile
toTile i
    | i == 0    = Nothing
    | otherwise = Just i
