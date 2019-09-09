data Solver = Solver 
  { getScore :: b -> Int
  , evaluateScore :: g -> Int
  , buildNode :: a -> Int -> b
  }

minimax :: Solver -> Bool -> Tree a -> Tree b
minimax Solver{buildNode=build} _ (Node val []) = Node (build val $ evaluateScore g) []
minimax Solver{buildNode=build} maximizingPlayer (Node val ts) 
  | maximizingPlayer = Node (build val $ minMaxScore maximumBy) children
  | not maximizingPlayer = Node (build val $ minMaxScore minimumBy) children
  where 
    nextT = nextPlayer t
    children = map (minimax (not maximizingPlayer)) ts
    getScore (Node (_, score) _) = score
    minMaxScore comp = getScore $ comp (comparing getScore) children

minimaxAB :: Solver -> Bool -> Tree a -> Tree b
minimaxAB t = minimaxAB' t NegInf PosInf

minimaxAB' :: Bool -> ExtendedNum Int -> ExtendedNum Int -> Tree a -> Tree b
minimaxAB' _ _ _ (Node a []) = Node (p, evaluateScore g) []
minimaxAB' maximizingPlayer a b (Node (p, g) ts) 
  | True <- maximizingPlayer = Node (p, minMaxScore maximumBy) children
  | False <- maximizingPlayer = Node (p, minMaxScore minimumBy) children
  where
    startingValue = if maximizingPlayer then NegInf else PosInf
    children = minimaxHelper ts nextT a b startingValue
    getScore (Node (_, score) _) = score
    minMaxScore comp = getScore $ comp (comparing getScore) children

minimaxHelper :: [Tree a] -> Bool -> ExtendedNum Int -> ExtendedNum Int -> ExtendedNum Int -> [Tree b]
minimaxHelper [] _ _ _ _ = []
minimaxHelper (x : xs) maximizingPlayer alpha beta value
  | alpha >= beta = []
  | otherwise =
    let 
      newNode@(Node (_, score) _) = minimaxAB' (not maximizingPlayer) alpha beta x
      newValue = comp value (Only score)
      newA = newAlpha maximizingPlayer alpha newValue
      newB = newBeta maximizingPlayer beta newValue
    in
      newNode : minimaxHelper xs t newA newB newValue
  where 
    comp = if maximizingPlayer then max else min
    newAlpha alpha v = if maximizingPlayer then max alpha v else alpha
    newBeta beta v = if maximizingPlayer then beta else min beta v

