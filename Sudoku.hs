import Data.List

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char

boxsize :: Int
boxsize = 3

values :: [Value]
values = ['1'..'9']

empty :: Value -> Bool
empty = (== '.')

single :: [a] -> Bool
single [_] = True
single _ = False

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

boxs :: Matrix a -> [Row a]
boxs = unpack . map cols . pack
       where pack = split . map split
             split = chop boxsize
             unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs g)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = not (elem x xs) && nodups xs

type Choices = [Value]

choices :: Grid -> Matrix Choices
choices = map (map choice)
          where choice v = if empty v then values else [v]

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [y : ys | y <- xs, ys <- cp xss]

collapse :: Matrix [a] -> [Matrix a]
collapse = cp . map cp

basicSolve :: Grid -> [Grid]
basicSolve = filter valid . collapse . choices

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows
        where pruneBy f = f . map reduce . f

reduce :: Row Choices -> Row Choices
reduce xss = [xs `minus` singles | xs <- xss]
             where singles = concat (filter single xss)

minus :: Choices -> Choices -> Choices
xs `minus` ys = if single xs then xs else xs \\ ys

fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
          where x' = f x

pruningSolve :: Grid -> [Grid]
pruningSolve = filter valid . collapse . fix prune . choices

easy :: Grid
easy = ["2....1.38",
        "........5",
        ".7...6...",
        ".......13",
        ".981..257",
        "31....8..",
        "9..8...2.",
        ".5..69784",
        "4..25...."]

gentle :: Grid
gentle = [".1.42...5",
          "..2.71.39",
          ".......4.",
          "2.71....6",
          "....4....",
          "6....74.3",
          ".7.......",
          "12.73.5..",
          "3...82.7."]

diabolical :: Grid
diabolical = [".9.7..86.",
              ".31..5.2.",
              "8.6......",
              "..7.5...6",
              "...3.7...",
              "5...1.7..",
              "......1.9",
              ".2.6..35.",
              ".54..8.7."]

unsolvable :: Grid
unsolvable = ["1..9.7..3",
              ".8.....7.",
              "..9...6..",
              "..72.94..",
              "41.....95",
              "..85.43..",
              "..3...7..",
              ".5.....4.",
              "2..8.6..9"]

minimal :: Grid
minimal = [".98......",
           "....7....",
           "....15...",
           "1........",
           "...2....9",
           "...9.6.82",
           ".......3.",
           "5.1......",
           "...4...2."]

blank :: Grid
blank = replicate n (replicate n '.')
        where n = boxsize ^ 2
