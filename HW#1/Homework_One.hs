-- Compile with ghci for best outcome --

import HW1types

-- Exercise One --

-- Part A --
ins :: Eq a => a -> Bag a -> Bag a
ins x [] = [(x,1)]
ins x (b:bs) | x == fst b = (x, snd b + 1):bs
             | otherwise = b:(ins x bs)

-- Part B --
del :: Eq a => a -> Bag a -> Bag a
del x [] = []
del x ((b1, 1):bs) | x == b1 = bs
                   | otherwise = (b1,1):(del x bs)
del x (b:bs) | x == fst b = (x, snd b - 1):bs
              | otherwise = b:(del x bs)

-- Part C --
bag :: Eq a => [a] -> Bag a
bag (a:[]) = ins a []
bag (a:as) = ins a (bag as)

-- Part D --
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _     = True
subbag _ []     = False
subbag (a:as) b = if subbag' a b then subbag as b else False

subbag' :: Eq a => (a, Int) -> Bag a -> Bool
subbag' _ []            = False
subbag' (x, y) (b:[]) = x == fst b && y <= snd b
subbag' e@(x, y) (b:bs) | x /= fst b = subbag' e bs
                       | y <= snd b = True
                       | otherwise = False

-- Part E --
--isSet :: Eq a => Bag a -> Bool
--isSet seen [] = False
--isSet seen (x:xs) = 


-- Part F --
size :: Bag a -> Int
size []            = 0
size ((x, y):es) = y + size es

-- Exercise Two --

-- Part A --
nodes :: Graph -> [Node]
nodes g = norm $ concat [[fst x, snd x] | x <- g]

-- Part B --
suc :: Node -> Graph -> [Node]
suc n g = [y | (x,y) <- g, x == n]

-- Part C --
detach :: Node -> Graph -> Graph
detach n g = [x | x <- g , fst x /= n, snd x /= n]

-- Part D --
cyc :: Int -> Graph
cyc n = [(x, x `mod` n + 1) | x <- [1..n]]

-- Exercise Three --

-- Part A --
width :: Shape -> Length
width (Pt     _    ) = 0
width (Circle _ r  ) = 2 * r
width (Rect   _ w _) = w

-- Part B --
bbox :: Shape -> BBox
bbox (Pt p)       = (p, p)
bbox (Circle p r) = ((fst p - r, snd p - r), (fst p + r, snd p + r))
bbox (Rect p w h) = (p, (fst p + w, snd p + h))

-- Part C --
minX :: Shape -> Number
minX (Pt p)       = fst p
minX (Circle p r) = fst p - r
minX (Rect p _ _) = fst p

-- Part D --
addPt :: Point -> Point -> Point
addPt (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

move :: Shape -> Point -> Shape
move (Pt p) x       = Pt (addPt p x)
move (Circle p r) x = Circle (addPt p x) r
move (Rect p w h) x = Rect (addPt p x) w h