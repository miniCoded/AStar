{-#LANGUAGE TemplateHaskell#-}

module Main where
import Optics ( (&), (%~), (.~), (^.), makeLenses )
import Data.List ( elemIndex )
import Data.Maybe ( fromJust )
import qualified Data.Set as Set

-- A Node data represents a certain configuration of a 3x3 grid, the target state and the depth of the iteration
data Node = Node {
  _prev   :: Maybe Node,
  _nums   :: [Int],
  _len    :: Int,
  _target :: [Int]
} deriving (Show)

-- The Path is a linked list of the states which tracks the next Node and its score
data Path =
  End {
    _node  :: Node,
    _score :: Int
  }
  | Path {
    _next :: Path
  } deriving (Show)

makeLenses ''Node
makeLenses ''Path

-- The heuristic uses Manhattan distance to calculate then score of a Node, excluding 0
-- The grid of numbers is a flat list, so div and mod are required to pin the position of each number in the "grid"
targetDistance :: Node -> Int
targetDistance g = sum [distance (idx x (g ^. target)) (idx x (g ^. nums)) | x <- g ^. nums, x /= 0]
  where
    idx n t = (x,  y)
      where
        x = mod (fromJust $ elemIndex n t) 3
        y = div (fromJust $ elemIndex n t) 3
    distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- Two Nodes are said to be equal if their numbers are equal. This allows the algorithm to replace longer states if a shorter path to it is found
instance Eq Node where
  (==) :: Node -> Node -> Bool
  g1 == g2 = g1 ^. nums == g2 ^. nums

-- The complete score of a Node is definedas its heuristic and the depth of the iteration (the length of the path to it)
instance Ord Node where
  compare :: Node -> Node -> Ordering
  compare g1 g2 = compare (targetDistance g1 + g1 ^. len) (targetDistance g2 + g2 ^. len) <> compare (g1 ^. nums) (g2 ^. nums)

-- The grid of numbers is a flat list, so div and mod are required to pin the position of each number in the "grid"
zeroNeighbors :: Node -> [Int]
zeroNeighbors g =
  let
    -- mod3 is required for the horizontal nodes to detect the edge of the grid
    mod3 = mod idx 3
    -- div3 is required for the vertical nodes to detect te depth of the grid
    div3 = div idx 3
    hn = case mod3 of
      0 -> [(g ^. nums) !! (idx + 1)]
      1 -> [(g ^. nums) !! (idx - 1), (g ^. nums) !! (idx + 1)]
      _ -> [(g ^. nums) !! (idx - 1)]
    vn = case div3 of
      0 -> [(g ^. nums) !! (idx + 3)]
      1 -> [(g ^. nums) !! (idx - 3), (g ^. nums) !! (idx + 3)]
      _ -> [(g ^. nums) !! (idx - 3)]
  in
    vn ++ hn
  where
    idx = fromJust $ elemIndex 0 (g ^. nums)

-- To slide a node (assuming it's adjacent) you just swap positions with 0
slide :: Node -> Int -> Node
slide g n = g & nums .~ swap n (g ^. nums) & len %~ (+1)
  where
    swap x = map (\z -> if z == x then 0 else if z == 0 then x else z)

-- Not all boards are solvable, and thus a parity check is needed.
-- Parity check consists on finding all pairs of values (A,B) such that A>B && idx(A) < idx(B)
-- If the pair count is even, then the board is solvable. Otherwise not.
parity :: [Int] -> Bool
parity g = (`mod` 2) (length [() | x <- noZero, y <- noZero, x > y && idx x < idx y]) == 0
  where
    noZero = filter (/= 0) g
    idx n  = fromJust $ elemIndex n noZero

-- The algorithm used to solve the grid is A*
solve :: Node -> Maybe Path
solve g =
  let
    start = End {_node = g & prev .~ Nothing, _score = targetDistance g}
    (res, _, _) = aux start (Set.fromList [g]) g
  in
    res
  where
    -- The first argument is the current node
    -- Second is the open set (which in fact is a Set, a structure where no elements can be the same), where the to-visit states are stored
    -- Third is the previous node that was visited

    aux (Path p) open_set prev_node = aux p open_set prev_node
    aux (End gr s) open_set prev_node
      -- If the score is 0, then it means we are at distance 0 from the target state, which means we've reached it
      | s == 0 = (Just End { _node = gr, _score = s }, Set.empty, prev_node)
      -- If the open set for some reason is empty, then it means there is no path
      -- Due to the parity checks done previously, it is guaranteed that a path is always there
      | Set.null open_set = (Nothing, Set.empty, prev_node)
      | otherwise =
        let
          -- Get all the possible slides that are not the previous node, plus make the current node their previous
          neighbor_grids = [x & prev .~ Just gr | x <- map (slide gr) (zeroNeighbors gr), x /= prev_node]
          -- If a repeated grid is inserted, the Set structure will not insert them
          -- The new open set is the union between the old one and the new states, minus the current state of the grid
          new_set = Set.delete gr $ Set.union open_set (Set.fromList neighbor_grids)
          new_best = Set.findMin new_set
          -- This performs a botton recursion down to the end, sending the new minimum, the new set and the previous grid of the new best
          (new_next_path, new_next_set, new_next_prev) = aux (End {_node = new_best, _score = targetDistance new_best}) new_set (fromJust $ new_best ^. prev)
        in
          -- If the current node is the new node's previous, then we return a new link of the path
          if gr == new_next_prev  then
            (Just Path {_next = fromJust new_next_path}, new_next_set, prev_node)
          else
          -- Otherwise we just pass through the current node and path
            (new_next_path, new_next_set, new_next_prev)

printPath :: Path -> IO ()
printPath p =
  let
    bottomNode = bottom p
  in
    printPrevious (Just bottomNode) >> putStrLn ("Longitud: " ++ show (bottomNode ^. len))
  where
    bottom (Path n) = bottom n
    bottom (End n _) = n
    printPrevious (Just (Node p2 n _ _)) = do
      printPrevious p2
      putStrLn   "┌─────────┐"
      putStrLn $ "│ " ++ show (n !! 0) ++ "  " ++ show (n !! 1) ++ "  " ++ show (n !! 2) ++ " │"
      putStrLn $ "│ " ++ show (n !! 3) ++ "  " ++ show (n !! 4) ++ "  " ++ show (n !! 5) ++ " │"
      putStrLn $ "│ " ++ show (n !! 6) ++ "  " ++ show (n !! 7) ++ "  " ++ show (n !! 8) ++ " │"
      putStrLn   "└─────────┘"
    printPrevious Nothing = return ()

main :: IO ()
main = do
  putStrLn "Inserte el estado inicial (los números separados por espacios)"
  start <- getLine

  putStrLn "Inserte el estado final deseado (los números separados por espacios)"
  end <- getLine

  let
    startGrid = map (\x -> read x :: Int) (words start)
    endGrid = map (\x -> read x :: Int) (words end)
    -- Two board states are connected if their parities are the same. If they differ, then they can't be "solved"
    areValid = parity startGrid == parity endGrid

  if areValid then
    let
      grid = Node {_prev = Nothing, _nums = startGrid, _len = 1, _target = endGrid}
    in
      printPath . fromJust $ solve grid
  else
    print "La configuración no se puede resolver"
