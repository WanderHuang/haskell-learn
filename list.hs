import Data.Char
import Data.List

-- 生成数组 [2, 7]
favNums = 2 : 7 : [] 

-- 连续数组
zeroToTen = [0..10]
eventList = [2, 4..20]
letters = ['a', 'c'..'z']
infinPow10 = [10, 20..] --惰性求值
many2s = take 10 (repeat 2)
listTimes3 = [x * 3 | x <- [1..10], x * 3 <= 50] -- 条件过滤
multTable = [[x * y | y <- [1..10]] | x <- [1..10]] -- 条件过滤生成二维数组

-- list数据结构
ints :: [Int]
ints = [1, 2, 3]

-- 数组长度
len :: [Int] -> Int
len [] = 0
len (x : xs) = len xs + 1

-- 范型函数
flipFn :: (a -> b -> c) -> (b -> a -> c)
flipFn f = \b a -> f a b

filpDot :: (b -> c) -> (a -> b) -> (a -> c)
filpDot f g = \x -> f (g x)

appendFn :: [a] -> [a] -> [a]
appendFn [] ys = ys
appendFn (x : xs) ys = x : (appendFn xs ys)

takeAsLong :: [a] -> (a -> Bool) -> [a]
takeAsLong [] _ = []
takeAsLong (x : xs) p
  | p x = x : takeAsLong xs p
  | otherwise = []

-- 尾递归优化
fastReverse :: [a] -> [a]
fastReverse src = fastReverseInner [] src
  where
    fastReverseInner acc [] = acc
    fastReverseInner acc (x : xs) = fastReverseInner (x : acc) xs

-- 数组高阶函数处理
-- 映射
-- map :: (a -> b) -> ([a] -> [b])
-- map _ []     = []
-- map f (x:xs) = f x : map f xs
-- 过滤
-- filter :: (a -> Bool) -> ([a] -> [a])
-- filter _ [] = []
-- filter p (x:xs) | p x        = x : filter p xs
--                 | otherwise  = filter p xs
-- foldr 创建折叠函数 js内叫reduce函数
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr _ z []     = z
-- foldr f z (x:xs) = f x (foldr f z xs)
-- sum = foldr (+) 0
-- concat = foldr (++) []
-- all = foldr (&&) True
-- product = foldr (*) 1
-- elem x = any . (map (== x))
-- map f = foldr (\x xs -> f x:xs) []
-- filter p = foldr (\x xs -> if p x then x:xs else xs) []
-- foldr1 :: (a -> a -> a) -> [a]  -> a

wordFrequncies :: String -> [(String, Int)]
wordFrequncies s =
  let cleaned = (filter (\x -> isLetter x || x == ' ') . map toLower) s
      groupedWords = (group . sort . words) cleaned
   in map (\xs -> (head xs, length xs)) groupedWords

--  选出至少有5个count的词
frequentWordsOnly :: [(String, Int)] -> [String]
frequentWordsOnly xs =
  let filtered = map fst . filter (\(_, count) -> count >= 5)
   in filtered xs

maxInt :: [Int] -> Int
maxInt = foldr1 (\x y -> if x > y then x else y)

-- 偶数平方 {x^2 | x in N, x is even}
evenSquares :: [Integer]
evenSquares = [x ^ 2 | x <- [1 ..], x `mod` 2 == 0]

-- (map (^2) . filter (\x -> x `mod` 2 == 0)) [1..]
-- 语法
-- 列表生成器[expression | generators_and_guards]
-- 生成器generator:  pattern <- expression
-- x <- [1..]
-- (x, y) <- [(1, 'a'), (1, 'b')]
-- (x:xs) <- [[2,3,4], [9]]
--
-- [(x, y) | x <- [1..3], y <- "abc"]
-- 生成满足条件的所有元素，集合成数组
-- [(1, 'a'), (1, 'b'), (1, 'c'), (2, 'a'), (2, 'b'), (2, 'c'), (3, 'a'), (3, 'b'), (3, 'c')
--
-- [(x, y) | x <- [1, 7, 3], x > 5, y <- [x-1, x + 1]]
-- [(7,6), (7,7), (7,8)]
--

-- 函数：求整数n的约数集合
divisors :: Integer -> [Integer]
divisors n = [k | k <- [1 .. n], n `mod` k == 0]

-- 质数集合
primes :: [Integer]
primes = [p | p <- [1 ..], length (divisors p) == 2]

-- take 10 primes

-- 用list重写clean函数
-- import Data.Char
-- clean :: String -> String
-- clean = filter(\x -> isLetter x || x == ' ') . map toLower

listClean :: String -> String
listClean s = [toLower c | c <- s, isLetter c || c == ' ']

-- 用list重写concat函数
-- concat :: [[a]] -> [a]
-- concat = foldr (++) []
listConcat :: [[a]] -> [a]
listConcat xss = [x | xs <- xss, x <- xs]

-- 数据结构
-- data
-- deriving派生 deriving (Show) 可以print
data Geometry = Rectangle Double Double | Square Double | Circle Double deriving (Show)

-- 枚举
data Animal = Cat | Dog | Mouse | Elephant

-- data Boolean = True | False

perimeter :: Geometry -> Double
perimeter (Rectangle width height) = 2 * width + 2 * height
perimeter (Square width) = width * 4
perimeter (Circle radius) = 2 * pi * radius

-- 模式匹配
data Coordinates = Coordinates Double Double deriving (Show)

data LocatedShape = LocatedShape Geometry Coordinates deriving (Show)

containedIn :: Coordinates -> LocatedShape -> Bool
(Coordinates x y) `containedIn` (LocatedShape (Circle r) (Coordinates cx cy)) = (cx - x) ^ 2 + (cy - y) ^ 2 <= r ^ 2
(Coordinates x y) `containedIn` (LocatedShape (Rectangle a b) (Coordinates cx cy)) = abs (cx - x) <= a / 2 && abs (cy - y) <= b / 2
(Coordinates x y) `containedIn` (LocatedShape (Square a) (Coordinates cx cy)) = abs (cx - x) + abs (cy - y) <= a

-- move 移动一个locatedshape
data Vector = Vector Double Double deriving (Show)

move :: LocatedShape -> Vector -> LocatedShape
move (LocatedShape shape (Coordinates cx cy)) (Vector vx vy) = LocatedShape shape (Coordinates (cx + vx) (cy + vy))

-- 类/接口类型
-- class Show a where show :: a -> String
-- 通过deriving操作符来派生对应的方法
-- Show被默认实现，实现一个类型，通过instance操作符
-- instance Show Coordinates where show (Coordinates x y) = show (x, y)
-- 类型约束条件: (Class T) => expression
showHead :: (Show a) => [a] -> String
showHead = show . head

-- data Eq a where (==), (/=) :: a -> a -> Bool
-- x /= y = not (x == y)

-- 继承
-- Ord 继承 Eq
-- class (Eq a) => Ord a where
--   (<), (<=), (>=), (>) :: a -> a -> Bool
--   max, min :: a -> a -> a
--   compare :: a -> a -> Ordering

-- compare函数返回对比结果为 Ordering 枚举类型 data Ordering = EQ | LT | GT

-- 为一个类型实现Eq
-- 模式匹配得到 f g
-- 比较f g对输入的一致性
data BoolFunc = BoolFunc (Bool -> Bool)

instance Eq (BoolFunc) where
  BoolFunc f == BoolFunc g = (f True == g True) && (f False == g False)

-- type 类型重定义
-- 类型可以用到函数中去
type ConsTwo = (Double, Double)

type IPV6 = String

type URLAddress = String

-- IPV6 和URLAddress 可以混用，都是String类型 https:// + URLAddress + / == https:// + IPV6 + /
-- data定义的类型则不可以混用
data DataIPV6 = DataIPV6 String

data DataURLAddress = DataURLAddress String

-- 一个类型后面只有一个构造类型，一个类型，可以使用newtype定义
newtype NewTypeIPV6 = NewTypeIPV6 String

newtype Fraction = Fraction (Int, Int)

instance Eq Fraction where
  Fraction (a, b) == Fraction (c, d) = a * d == b * c

-- 类型多态
-- Maybe a = Nothing | Just a
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeShowHead :: ((Show a) => [a] -> String)
safeShowHead xs = case safeHead xs of
  Nothing -> "Empty"
  Just x -> show x

-- 递归定义
-- data类型TypedList定义中使用了自己
data TypedList a = Empty | NonEmpty a (TypedList a) deriving (Show)

-- [3, 2, 6] :: [Int]
-- Nonempty 3 (Nonempty 2 (Nonempty 6 Empty)) :: List Int

elemInList :: (Eq a) => a -> TypedList a -> Bool
elemInList _ Empty = False
elemInList a (NonEmpty x xs)
  | x == a = True
  | otherwise = elemInList a xs

-- 构造二叉树
-- Node current_value left_value right_value
data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show)

-- intTree :: Tree int
-- intTree =
--   Node
--     4
--   ( Node
--         7
--         (Node 19 Nil Nil)
--    Nil
--    )
--  ( Node
--    (-3)
--  (Node 8 Nil Nil)
--      (Node 12 Nil Nil)
--  )
-- 打印二叉树叶子节点
leaves :: Tree a -> [a]
leaves = foldTree combine []
  where
    combine x [] [] = [x]
    combine _ left right = left ++ right

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ z Nil = z
foldTree f z (Node x left right) = f x (foldTree f z left) (foldTree f z right)

main = do
  print
    ints
  print
    (len ints)
  print
    (takeAsLong [1, 7, 4, 2, 1] (\x -> x > 3))
  print
    (fastReverse [1, 2, 3])
  print
    (wordFrequncies "It was the best of times, it was the worst of times,")
  print
    (frequentWordsOnly [("aaa", 5), ("bbb", 2)])
  print
    (listClean "Ab c")
  print
    (listConcat [[1], [2]])
  print
    (elemInList 3 (NonEmpty 2 (NonEmpty 3 Empty)))
