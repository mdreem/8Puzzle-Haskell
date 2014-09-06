module Search where

import           Board
import qualified Data.Heap as Heap

-- Node steps priority board node
data SearchNode = Nil | Node Int Int Board SearchNode deriving (Eq)

-- compare SearchNodes by ...
instance Ord SearchNode where
    Nil `compare` Nil = EQ
    (Node _ i1 _ _ ) `compare` (Node _ i2 _ _) = i1 `compare` i2
    Nil `compare` (Node _ _ _ _ )              = LT
    (Node _ _ _ _) `compare` Nil               = GT

-- solver
solve::Board -> Maybe SearchNode
solve board
    | origBoard == board = Just (solve' heap twinHeap)
    | otherwise          = Nothing
    where
        node        = Node 0 (manhattanBoard board) board Nil
        twinNode    = Node 0 (manhattanBoard twinBoard) twinBoard Nil
        twinBoard   = twin board
        heap        = Heap.fromList [node]
        twinHeap    = Heap.fromList [twinNode]
        solution    = solve' heap twinHeap
        origBoard   = getOriginalBoard (nodeList solution)

solve' heap twinHeap
    | isFinal board     = node
    | isFinal twinBoard = twinNode
    | otherwise         = solve' (step heap) (step twinHeap)
    where
        Just node            = Heap.viewHead heap
        Just twinNode        = Heap.viewHead twinHeap
        Node _ _ board _     = node
        Node _ _ twinBoard _ = twinNode

-- takes the minimal element from the heap, takes all the neighbours and
-- puts the respective search nodes back on the heap.
step :: Heap.MinHeap SearchNode -> Heap.MinHeap SearchNode
step heap = Heap.drop 1 (toHeap (map (nextNode node) nb) heap)
    where
        Just node          = Heap.viewHead heap
        (Node s p board _) = node
        nb                 = filter (notPrevBoard node) (neighbours (findEmpty board) board)

       
-- extracts the board used one step back from a search node 
prevBoard :: SearchNode -> Board
prevBoard (Node _ _ _ Nil) = []
prevBoard (Node _ _ _ p)   = b
    where
        (Node _ _ b _) = p

        
notPrevBoard :: SearchNode -> Board -> Bool
notPrevBoard node board = board /= b
    where
        b = prevBoard node
        
-- takes a node and a board and constructs the next search node
nextNode node board = Node (s + 1) (s + 1 + manhattanBoard board) board node
    where
        (Node s p b n) = node

-- adds a list of elements to a heap
toHeap [] heap = heap
toHeap (x:xs) heap = toHeap xs (Heap.insert x heap)

-- generate a list from the search nodes
nodeList Nil = []
nodeList node =  node:nodeList next
    where
        Node _ _ _ next = node

-- returns the board in the first node
getOriginalBoard (x:[]) = b
    where
        Node _ _ b _ = x
getOriginalBoard (x:xs) = getOriginalBoard xs

-- search node to string
printSearchNode :: SearchNode -> String
printSearchNode Nil = ""
printSearchNode (Node i p b sn) = "Steps: " ++ (show i) ++ " Priority: " ++ (show p) ++ "\n" ++ (printBoard b)

printNodes :: SearchNode -> String
printNodes Nil = ""
printNodes (Node _ _ b sn) = (printBoard b) ++ "\n" ++ printNodes sn

printNodeList :: [SearchNode] -> String
printNodeList [] = ""
printNodeList (x:xs) = (printBoard b) ++ "\n" ++ printNodeList xs
        where
            (Node _ _ b _) = x

instance Show SearchNode where
    show sn = "Steps: " ++ show i ++ "\n\n" ++ printNodeList (reverse (nodeList sn))
        where
            (Node i _ _ _) = sn
