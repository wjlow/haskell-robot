type Position = (Int, Int)
data Direction = North | South | East | West
    deriving (Eq, Show)

place :: Position -> Direction -> (Position, Direction)
place pos dir = (pos, dir)

move :: (Position, Direction) -> (Position, Direction)
move ((x, y), dir)
    | dir == North = (newPos (x, y + 1), dir)
    | dir == South = (newPos (x, y - 1), dir)
    | dir == West = (newPos (x - 1, y), dir)
    | dir == East = (newPos (x + 1, y), dir)
    where newPos pos = if onGrid pos then pos else (x, y)

left :: (Position, Direction) -> (Position, Direction)
left (pos, dir) 
    | dir == North = (pos, West)
    | dir == West = (pos, South)
    | dir == South = (pos, East)
    | dir == East = (pos, North)

right :: (Position, Direction) -> (Position, Direction)
right (pos, dir) 
    | dir == North = (pos, East)
    | dir == East = (pos, South)
    | dir == South = (pos, West)
    | dir == West = (pos, North)

report :: (Position, Direction) -> (Position, Direction)
report x = x

onGrid :: Position -> Bool
onGrid (x, y)
    | x >= 0 && x <= 3 && y >= 0 && y <= 3 = True
    | otherwise = False


robot :: Position -> Direction -> [(Position, Direction) -> (Position, Direction)] -> (Position, Direction)
robot pos dir commands = reduce commands initial
    where
        initial = place pos dir
        reduce fs v = foldl (flip (.)) id fs $ v -- reduce [f, g, h] x = h(g(f(id(x))))