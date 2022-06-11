{-
Author: Michael Smith
Date: May 2021
-}
import Data.Maybe

-- Exercise One --
type Prog = [Cmd]
data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | DEC
         | SWAP
         | POP Int
type Stack = [Int]

-- Part A --
type Rank = Int
type CmdRank = (Int,Int)

rankC :: Cmd -> CmdRank
rankC (LD _) = (0,1)
rankC ADD = (2,1)
rankC MULT = (2,1)
rankC DUP = (1,2)
rankC DEC = (1,1)
rankC SWAP = (2,2)
rankC (POP k) = (k,0)

rankP :: Prog -> Maybe Rank
rankP cs = rank cs 0

d :: Cmd -> Rank
d c = (snd (rankC c)) - (fst (rankC c))

rank :: Prog -> Rank -> Maybe Rank
rank [] k = Just k
rank (c:cs) k | k < (fst (rankC c)) = Nothing
              | otherwise           = rank cs (k+d c)

-- Part B --
semStatTC :: Prog -> Maybe Stack
semStatTC p | rankP p == Nothing = Nothing
            | otherwise          = Just (sem p [])

semCmd :: Cmd -> Stack -> Stack
semCmd (LD x) c = x:c
semCmd ADD (x:y:c) = (x +y):c
semCmd MULT (x:y:c) = (x * y):c
semCmd DUP (x:c) = x:x:c
semCmd DEC (x:c) = (x-1):c
semCmd SWAP (x:y:c) = y:x:c
semCmd (POP k) c | k > 0     = semCmd (POP (k-1)) (tail c)
                 | otherwise = c

sem :: Prog -> Stack -> Stack
sem [] c = c
sem (x:xs) c = sem xs (semCmd x c)

-- Exercise Two --
data Shape = X
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type BBox = (Int,Int)

-- Part A --
bbox :: Shape -> BBox
bbox X = (1,1)
bbox (TD s1 s2) = (max (fst (bbox s1)) (fst (bbox s2)),
                        (snd (bbox s2)) + (snd (bbox s1)))
bbox (LR s1 s2) = ((fst (bbox s2)) + (fst (bbox s1)),
                        max (snd (bbox s1)) (snd (bbox s2)))

data Type = Shape | BBox | TypeError
                        deriving (Eq, Show)

bboxtc :: Shape -> Type
bboxtc X = BBox
bboxtc (TD s1 s2) | bboxtc s1 == BBox && bboxtc s2 == BBox = BBox
bboxtc (LR s1 s2) | bboxtc s1 == BBox && bboxtc s2 == BBox = BBox
bboxtc _ = TypeError

-- Part B --
rect :: Shape -> Maybe BBox
rect s@X = Just (bbox s)
rect s@(TD X X) = Just (bbox s)
rect s@(LR X X) = Just (bbox s)
rect s@(TD s1 s2) | snd (bbox s1) == snd (bbox s2) && (isJust (rect s1)) && (isJust (rect s2)) = Just (bbox s)
rect s@(LR s1 s2) | fst (bbox s1) == fst (bbox s2) && (isJust (rect s1)) && (isJust (rect s2)) = Just (bbox s)
rect _ = Nothing