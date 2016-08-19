import Data.Maybe

type Field = Maybe Int
type Board = [[Field]]
data GameState = GameState Board Int Int
data Direction = DUp | DDown | DLeft | DRight

instance Show GameState where
  show (GameState b s _) = "asdf"

display :: Field -> String
display Nothing = " "
display (Just a) = show a

boardPrint :: Board -> String
boardPrint = unlines . map (unwords.(map display))

rotate :: [[a]] -> [[a]]
rotate ([]:xs) = []
rotate board = (reverse $ map head board) : (rotate $ map tail board)

colapseList :: [Field] -> ([Field],Int)
colapseList [] = ([],0)
colapseList (a:[]) = ([a],0)
colapseList (a:b:xs) =
  if a == b
  then
    let (rest, score) = colapseList xs
    in  ((fmap (*2) a) : rest ++ [Nothing], score + (maybe 0 (*2) a))
  else
    let (rest, score) = colapseList (b:xs)
    in (a : rest, score)
       
shiftList :: [Field] -> [Field]
shiftList a = (filter (/=Nothing) a) ++ (filter (==Nothing) a)

swipeLeft :: Board -> (Board,Int)
swipeLeft a = (map fst result, sum $ map snd result)
    where result = map (colapseList.shiftList) a

applyToFirst :: (a -> b) -> (a,c) -> (b,c)
applyToFirst f (a, b) = (f a, b)

swipe :: Direction -> Board -> (Board,Int)
swipe DLeft a = swipeLeft a
swipe DDown a = applyToFirst (rotate.rotate.rotate) (swipeLeft $ rotate a)
swipe DRight a = applyToFirst (rotate.rotate) (swipeLeft $ rotate $ rotate a)
swipe DUp a = applyToFirst (rotate) (swipeLeft $ rotate $ rotate $ rotate a)

rand :: Int -> Int
rand a = (1103515245*a+12345) `mod` 4294967296


isGameOver :: Board -> Bool
isGameOver b = (length $ filter (==Nothing) $ concat b) == 0

insertNew :: Int -> Board -> Board
insertNew seed board =
  let
    lList   = map length board
    rowLen  = length $ head board
    pList   = map (\a -> a-rowLen) $ tail $ scanl (+) 0 lList
    tList   = zip pList board
    nList   = map (\(n, l) -> zip [n..] l ) tList
    ntgList = filter ((==Nothing) . snd) $ concat nList
    emptyNr = length ntgList
    pos     = seed `mod` emptyNr
    newNr   = if seed `mod` 10 == 0 then 4 else 2
    pInList = fst $ ntgList!!pos
    replace = map (\(n, m) -> if n==pInList then (Just newNr) else m)
  in
    map replace nList


    

gameStep :: Direction -> GameState -> GameState
gameStep dir (GameState board score seed) = 
    let
      (nboard,delta) = swipe dir board
    in
      GameState (insertNew seed nboard) (score+delta) (rand seed)
    
        
