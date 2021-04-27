isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

isZeroPipe :: Int -> Bool
isZeroPipe x
  | x == 0 = True
  | otherwise = False

hello :: () -> IO ()
hello () = print "hello world from haskell"

curryFn :: Int -> Int -> Int
curryFn x y = x + y

inRange :: Int -> Int -> Int -> Bool
inRange a b c
  | a <= c && c <= b = True
  | otherwise = False

tupleFn :: (Int, Int) -> Int
tupleFn (a, b) = a + b

-- 斐波那契数列
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- 剪枝斐波那契数列
fibIter :: Integer -> Integer -> Integer -> Integer
fibIter start end 0 = end
fibIter start end steps = fibIter end (start + end) (steps - 1)

fibBetter :: Integer -> Integer
fibBetter 0 = 0
fibBetter x = fibIter 0 1 (x - 1)

-- 阶乘
factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)

-- 区间和
rangeSum :: Int -> Int -> Int
rangeSum a b
  | a == b = a
  | otherwise = a + rangeSum (a + 1) b

-- 字符串重复
repeatFn :: String -> Int -> String
repeatFn word times
  | times == 1 = word
  | otherwise = repeatFn word (times - 1) ++ word

-- 懒计算
both :: Bool -> Bool -> Bool
both b True = b
both _ False = False

-- 无限循环
loop :: Bool -> Bool
loop = loop

-- lambda演算
applyTwice :: (Int -> Int) -> Int -> Int
-- 连续调用两次，前次的结果作为后次的参数
applyTwice f = f . f

-- applyTwice (\x -> x * 2)
-- applyTwice (\x -> x + 1)

-- 高阶函数
applyUntil :: (Int -> Bool) -> (Int -> Int) -> Int -> Int
applyUntil p f x
  | p x = x
  | otherwise = applyUntil p f (f x)

-- 最大的非偶数约数
greatestOddDivisor :: Int -> Int
-- hint 省略
-- greatestOddDivisor input = applyUntil (\x -> mod x 2 == 1) (\x -> div x 2) input
greatestOddDivisor = applyUntil (\x -> mod x 2 == 1) (\x -> div x 2)

-- 最小自然数
smallestNatThat :: (Int -> Bool) -> Int
smallestNatThat p = applyUntil p (\x -> x + 1) 1

-- 超过1000的2的倍数
-- 操作符省略规则
-- (> 1000)  ===> (\x -> > 1000 x)
-- (*2)      ===> (\x -> x * 2)
-- applyUntil (> 1000) (*2) 1

-- 操作符定义
-- 括号括起来的操作符可以作为前缀使用,二元函数用反引号括起来可以作为中缀使用，比如
-- ((+) 1 2)        ===> (1 + 2)
-- (7-) (6 `div` 3) ===> 7 - (div 6 3) ===> 7 - 2

-- $ 符号有最低的优先级，可以用来替代parentheses，即替换小括号对
-- print (double (next 4))
-- print $ double $ next 4
-- 操作符声明时可以指定优先级 fixity declaration:
-- association precedence operator_name
-- infixl: left associative
-- infixr: right associative
-- infix: non associative

infixl 5 ***

(***) :: String -> Int -> String
_ *** 0 = ""
s *** n = s ++ (s *** (n - 1))

-- 语法糖
-- if..else
-- if condition then expression_1 else expression_2
-- case..of
-- case expression of
--   pattern_1 -> expression_1
--   pattern_2 -> expression_2
--   pattern_n -> expression_n
-- let..in
-- let
--    pattern_1 = expression_1
--    pattern_2 = expression_2
--    pattern_n = expression_n
-- in expression
-- ...where
-- function_name :: type_declaration
-- function_name arguments_1 = expression_1
-- function_name arguments_n = expression_n
--    where pattern_1 = expression_1
--          pattern_n = expression_n
sign :: Int -> Int
sign n = if n > 0 then 1 else if n < 0 then -1 else 0

bothTrue :: Bool -> Bool -> Bool
bothTrue x y = case (x, y) of
  (True, True) -> True
  _ -> False

solveQuadratic :: Double -> Double -> (Double, Double)
solveQuadratic p q =
  let p' = - p / 2
      d = sqrt (p' ^ 2 - q)
   in (p' - d, p' + d)

solveQuadraticWhere :: Double -> Double -> (Double, Double)
solveQuadraticWhere p q = (p' - d, p' + d)
  where
    p' = - p / 2
    d = sqrt (p' ^ 2 - q)

-- fibonacci :: Int -> Int
-- fibonacci 0 = 0
-- fibonacci n = fibHelper 0 1 (n - 1)
--   where fibHelper :: Int -> Int -> Int -> Int
-- 		  fibHelper smaller larger 0 = larger
-- 		fibHelper smaller larger steps = fibHelper larger (smaller + larger) (steps - 1)

main = do
  hello ()
  print (isZero 1)
  print
    (isZeroPipe 1)
  print
    (curryFn 3 2)
  print
    (tupleFn (2, 3))
  print
    (inRange 1 4 2)
  print
    (fib 6)
  print
    (both (loop True) False)
  print
    (repeatFn "1" 4)
  print
    (greatestOddDivisor 44)
  print
    ("ab" *** 3)
  print
    (solveQuadratic 1 (-2))
  print
    (solveQuadraticWhere 1 (-2))
