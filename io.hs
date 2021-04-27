-- IO
-- IO () 中 ()为空类型
printTwoLines :: String -> String -> IO ()
printTwoLines s t = putStrLn s >> putStrLn t

-- pure a -> IO a
-- pure 表示IO的空类型
-- >> 忽略上一个IO产生的值
-- >>= 表示bind 本次IO拿到上一次的返回值作为参数
-- >>= :: IO a -> (a -> IO b) -> IO b
-- >>  :: IO a -> IO b -> IO b
-- ioa >> iob = ioa >>= \_ -> iob
printLines :: [String] -> IO ()
printLines [] = pure ()
printLines (x : xs) = putStrLn x >> printLines xs

echo :: IO ()
echo = getLine >>= \s -> putStrLn s

ioDouble :: IO ()
ioDouble = getLine >>= \s -> putStrLn ((show . (* 2) . read) s)

ioDoubleRead :: IO ()
ioDoubleRead = readLn >>= \a -> print (a * 2)

-- pure & impure combining

myGcd :: Integer -> Integer -> Integer
myGcd a b | a < b = myGcd b a
myGcd a 0 = a
myGcd a b = myGcd b (a `mod` b)

readInput :: IO (Integer, Integer)
readInput = readLn >>= \a -> readLn >>= \b -> pure (a, b)

printOutput :: Integer -> Integer -> IO ()
printOutput g l = print g >> print l

calc :: IO ()
calc = readInput >>= \(a, b) -> printOutput (myGcd a b) (lcm a b)

-- 语法糖 do
-- 联合多个IO
-- do
--  x <- action_a
--  y <- action_b
--  action_c
--  action_d
--
-- x <- action 语法利用了>>=
-- action 语法利用了>>

addInput :: IO ()
addInput = do
  putStrLn "first ?"
  first <- readLn
  putStrLn "seconed ?"
  second <- readLn
  putStrLn ("sum: " ++ (show (first + second) ++ "."))

readInputWithDo :: IO (Integer, Integer)
readInputWithDo = do
  a <- readLn
  b <- readLn
  pure (a, b)

-- 语法糖 do..let
-- do
--  a <- action
--  b <- action
--  let x = a + b
--  action_x

readInputWithDoLet :: IO Integer
readInputWithDoLet = do
  a <- readLn
  b <- readLn
  let c = a + b
  pure c
