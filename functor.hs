-- syntax
--
-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b
--     (<$) :: a -> f b -> f a
-- 最小实现为fmap函数
--
-- Functor是一个抽象数据类型 f a
-- 一个新的Functor f b可以根据 f a 变换所有的值来得到，这个过程不改变f的结构
-- f表示实现了Functor的一个数据结果，可以对a类型的数据进行映射
--
--
-- rules
--
-- 1. 交换律，可以透传id函数
-- fmap id = id
-- 2. 结合律
-- fmap (f . g) == fmap f . fmap g
-- 
-- 数据值仅允许使用fmap进行修改，映射操作(fmap)本身不修改数据，而是fmap的参数-函数-对数据进行修改，然后生成新的Functor
-- 修改的过程Functor不改变，只有值被改变了。
--
--  在每个f a上调用函数来把f a变为f b
-- fmap :: (a -> b) -> f a -> f b
-- 通过替换f b的值为a把数据改为f a
-- (<$) :: a -> f b -> f a
--
-- 相关函数
--
-- ($>) :: f a -> b -> f b
-- 同义操作
-- (<$>):: (a -> b) -> f a -> f b

{-
  Applicative Functor
-}

-- 继承自Functor
-- Applicative Functor比Monad要简单一些，也更容易写出通用的代码
-- eg: zipList是list上的applicative functor，liftA2由zipWith实现
-- liftM liftM2 ap
-- 如何使用?
--
-- 原来Monad写法
-- do x <- fx
--    y <- fy
--    return g x y
-- 可以写为
--    liftM2 g fx fy
-- 通常来说applicative functor的写法不依赖于之前的单子的行为，这让liftM可以使用在任何场景。
--
-- 当你发现你一直只使用Monad方法，那么导入Control.Applicative包，使用pure替代return，<$>替换liftM，liftA2替换liftM2，<*>替换ap.
-- 如果你的函数签名是Monad m => ...那么修改为Applicative m => ...就可以了。


