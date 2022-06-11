import Data.Char

-- Exercise One --

-- Part A --
data Cmd = Pen Mode
         | Moveto Pos Pos
         | Def String Pars Cmd
         | Call String Vals
         | Cons Cmd Cmd
         deriving Show
data Mode = Up | Down
          deriving Show
data Pos = Lit Int | Ref String
         deriving Show
type Pars = [String]
type Vals = [Int]

-- Part B --
change :: [Cmd] -> Cmd
change (x:[]) = x
change (x:xs) = Cons x (change xs)
vector :: Cmd
vector = Def "vector" ["x1", "y1", "x2", "y2"]
         (change [Pen Up,
         Moveto (Ref "x1") (Ref "y1"),
         Pen Down,
         Moveto (Ref "x2") (Ref "y2")])

-- Part C --
steps :: Int -> Cmd
steps n
  | n<=0 = Pen Up
  | otherwise = change
                [Pen Up,
                Moveto (Lit n) (Lit n),
                Pen Down,
                Moveto (Lit (n-1)) (Lit(n)),
                Moveto (Lit (n-1)) (Lit (n-1)),
                steps (n-1)]

-- Exercise Two --

-- Part A --
data RegEx = Null
           | Empty
           | L Char
           | Union RegEx RegEx
           | Concat RegEx RegEx
           | Star RegEx
           deriving (Show, Eq)

-- Part B --
-- Helper functions for Accept --
successful :: RegEx -> Bool
successful Null = False
successful Empty = True
successful (L _) = False
successful (Union w w') = (successful w) || (successful w')
successful (Concat w w') = (successful w) && (successful w')
successful (Star _) = True

step :: RegEx -> Char -> RegEx
step Null _ = Null
step Empty _ = Null
step (L c) x | x == c = Empty
step (L c) _ = Null
step (Union w w') c = Union (step w c) (step w' c)
step (Concat w w') c = 
  if successful w then
    Union (step w' c) (Concat (step w c) w')
  else
    Concat (step w c) w'
step (Star w) c = Concat (step w c) (Star w)

run :: RegEx -> String -> RegEx
run = foldl step

accept :: RegEx -> String -> Bool
accept w = successful . run w

classify :: RegEx -> [String] -> IO ()
classify e ws = putStrLn ("ACCEPT:\n"++show acc++"\nREJECT:\n"++show rej)
   where acc = filter (accept e) ws
         rej = filter (not.(accept e)) ws


-- Part C --
-- Created a lot of extra regular expression values and was able to use some for commaSep --
cs :: RegEx
cs = Star (L 'a')

bs :: RegEx
bs = Star (L 'b')

str :: String -> RegEx
str = foldr (Concat . L) Empty

anyOf :: [Char] -> RegEx
anyOf = foldr (Union . L) Null

cat :: [RegEx] -> RegEx
cat = foldr Concat Empty

dot :: RegEx
dot = anyOf ['c','b']

has :: Char -> RegEx
has c = cat [Star dot, L c, Star dot]

prefix :: String -> RegEx
prefix s = Union (Union (str s) (str "bat")) (Union (str "cat,cat") (str "cat,bat")) 

commaSep :: RegEx
commaSep = prefix "cat"

commaSepTest = ["cat","cat,bat","cat,cat","bat","",",","dog",",cat","cat,","catcat","cat,,bat","cat,bat,"]