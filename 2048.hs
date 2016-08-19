{-main =
  whileM_ ()
-}

data Swipe = Up | Down | Right | Left deriving(Eq)

type Field = Maybe Int

type BoardPosition = (Int,Int)

type Board = [[Field]] -- top left is 0 0

data GameState = GameState Board Int deriving(Eq)

swipe :: Swipe -> GameState -> GameState
swipe d (GameState b s) = 
  


collapse :: Swipe -> GameState -> GameState
collapse d (GameState b score) =
  
  

moveToBorder :: Swipe -> Board -> Board
moveToBorder d b =
  

elemFromDirection :: Swipe -> BoardPosition -> Board -> Field


moveFromDirection :: Swipe -> BoardPosition -> BoardPosition
moveFromDirection Up (a,b)
