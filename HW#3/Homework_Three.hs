{-
Name: Michael Smith
Date: May, 2021
-}
module Homework_Three where

-- Exercise One --
{-
I wasn't sure how to exactly test the finished stack
becuase of an error so I had to add a numbering system 
for each attribute of Cmd. In order to run the tests 
enter "stack testOne/Two/Three" in the ghci environment.
-}
type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP

type Stack = [Int]
type D = Stack -> Stack

type N = Int
type CmdNums = (Int,Int)

semCmd :: Cmd -> D
semCmd (LD i) s = (i:s)
semCmd ADD (n1:n2:ns) = (n1 + n2:ns)
semCmd MULT (n1:n2:ns) = (n1 * n2:ns)
semCmd DUP ns@(n:_) = (n:ns)

sem :: Prog -> D
sem [] s = s
sem (p:ps) s = sem ps (semCmd p s)

num :: Prog -> N -> Maybe N
num [] n = Just n
num (cmd:cs) n = if n >= x then num cs (n - x + y)
                 else Nothing
                 where (x,y) = numC cmd

check :: Prog -> Bool
check p = numP p /= Nothing

stack :: Prog -> Maybe Stack
stack c | check c = Just (sem c [])
        | otherwise = Nothing

numC :: Cmd -> CmdNums
numC (LD x) = (0,1)
numC ADD = (2,1)
numC MULT = (2,1)
numC DUP = (1,2)

numP :: Prog -> Maybe N
numP x = num x 0

testOne = [LD 3,DUP,ADD,DUP,MULT]
testTwo = [LD 3,ADD]
testThree = []

-- Exercise Two --

data Cmd' = Pen Mode
           | MoveTo Int Int
           | Seq Cmd' Cmd'
data Mode = Up | Down
  deriving Show

type State = (Mode,Int,Int)

type Line = (Int,Int,Int,Int)
type Lines = [Line]

-- Added some supporter functions for semS --
semSt :: (State,Lines) -> State
semSt ((a,x,y),b) = (a,x,y)

semM :: (State,Lines) -> (State,Lines) -> (State,Lines)
semM (x1,y1) (x2,y2) = (x2,y1++y2)

semS :: Cmd' -> State -> (State,Lines)
semS (Pen p) (_,x,y) = ((p,y,x),[])
semS (MoveTo x y) (Up,x0,y0) = ((Up,x,y),[])
semS (MoveTo x y) (Down,x0,y0) = ((Down,x,y),[(x0,y0,x,y)])
semS (Seq c d) (a,x,y) = semM (semS c (a,x,y)) (semS d (semSt (semS c (a,x,y))))

sem' :: Cmd' -> Lines
sem' c = snd (semS c (Down, 0, 0))
