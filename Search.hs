import Board

data SearchNode = Nil | Node Int Board SearchNode deriving (Eq)

instance Ord SearchNode where
	Nil `compare` Nil = EQ
	(Node i1 _ _ ) `compare` (Node i2 _ _) = i1 `compare` i2
	Nil `compare` (Node _ _ _ )= LT
	(Node _ _ _ ) `compare` Nil = GT

	

testNode :: SearchNode
testNode = Node 2 testBoard Nil

testNode1 :: SearchNode
testNode1 = Node 1 testBoard Nil

testNode3 :: SearchNode
testNode3 = Node 3 testBoard Nil


printSearchNode :: SearchNode -> String
printSearchNode Nil = ""
printSearchNode (Node i b sn) = "Steps: " ++ (show i) ++ "\n" ++ (printBoard b)

instance Show SearchNode where
	show sn = printSearchNode sn
	