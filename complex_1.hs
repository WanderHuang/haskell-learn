{-# LANGUAGE FlexibleInstances #-}

data Person = Man | Woman deriving (Show, Eq)

data Leader = Emperor | Queen deriving (Show)

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

whoAreYou :: (Person -> IO ())
whoAreYou x = print (if' (x == Man) (show Emperor) (show Queen))

-- 就是rust里面的trait的概念!
-- 接口、模型
class Say a where
  say :: a -> IO ()

-- 实现一个模型
instance Say String where
  say x = print x

main = do
  whoAreYou Man
  whoAreYou Woman
  say "abc"
