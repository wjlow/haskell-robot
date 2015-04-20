type Position = (Int, Int)
data Direction = North | South | East | West
	deriving (Eq, Show)

place :: Position -> Direction -> (Position, Direction)
place pos dir = (pos, dir)

move :: (Position, Direction) -> (Position, Direction)
move ((x, y), dir)
	| dir == North	= ((x, y + 1), dir)
	| dir == South 	= ((x, y - 1), dir)
	| dir == West 	= ((x - 1, y), dir)
	| dir == East 	= ((x + 1, y), dir)

left :: (Position, Direction) -> (Position, Direction)
left (pos, dir) 
	| dir == North 	= (pos, West)
	| dir == West 	= (pos, South)
	| dir == South 	= (pos, East)
	| dir == East	= (pos, North)

right :: (Position, Direction) -> (Position, Direction)
right (pos, dir) 
	| dir == North 	= (pos, East)
	| dir == East 	= (pos, South)
	| dir == South 	= (pos, West)
	| dir == West	= (pos, North)

report :: (Position, Direction) -> (Position, Direction)
report x = x

robot :: Position -> Direction -> [(Position, Direction) -> (Position, Direction)] -> (Position, Direction)
robot pos dir commands = compose commands (place pos dir)
	where compose fs v = foldl (flip (.)) id fs $ v