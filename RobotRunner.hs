import Data.Monoid (Monoid, (<>))
import qualified Data.Monoid as Monoid
import qualified Data.Monoid as Monoid

data Position = Position { x :: Int, y :: Int } deriving Show

data Direction = North | South | East | West deriving Show

data Robot = Robot { pos :: Position, dir :: Direction } deriving Show

type RobotRunner = Monoid.Endo (Maybe Robot)

move :: RobotRunner
move = Monoid.Endo (fmap (\robot ->
  case (pos robot, dir robot) of
      (Position x y, North) -> Robot (Position x (y + 1)) North
      (Position x y, South) -> Robot (Position x (y -1)) South
      (Position x y, East) -> Robot (Position (x + 1) y) East
      (Position x y, West) -> Robot (Position (x - 1) y) West
  ))

left :: RobotRunner
left = Monoid.Endo (fmap (\robot ->
  case (pos robot, dir robot) of
      (p, North) -> Robot p West
      (p, West) -> Robot p South
      (p, South) -> Robot p East
      (p, East) -> Robot p North
  ))

right :: RobotRunner
right = left <> left <> left

place :: Robot -> RobotRunner
place r = Monoid.Endo (
      \maybeRobot -> Just r
  )

