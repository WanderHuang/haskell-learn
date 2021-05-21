import Data.Maybe

-- 取出一个值，加入到另一个的计算中去,连续计算
-- >>=
-- 也叫bind函数
comb :: Maybe a -> (a -> Maybe b) -> Maybe b
comb Nothing _ = Nothing
comb (Just x) f = f x

-- 同上
-- =<<
combr :: (a -> Maybe b) -> Maybe a -> Maybe b
combr _ Nothing = Nothing
combr f (Just x) = f x

-- 只采用后一个值
-- >>
end :: Maybe a -> Maybe b -> Maybe b
end _ Nothing = Nothing
end _ (Just x) = Just x

-- 用类型类来表示Monad
-- 类型类就类似于其他语言里面的接口
-- class Monad m where
--   (>>=)  :: m a -> (a -> m b) -> m b
--   (>>)   :: m a -> m b        -> m b
--   return :: a   -> m a

-- 实现一个类型类，这里的语法用于生成了一个Monad Maybe
-- instance Monad Maybe where
--   Nothing >>= f   = Nothing
--   (Just x) >>= f  = f x
--   return          = Just
--   _ >> m          = m

-- monad支持do notation.
-- 在do语句中可以使用<-获取返回值, 类似于let，把monad里面的值给到<-左边的变量
-- do语句的返回值是最后一个语句
-- 返回一个monad

-- mySequence :: MyMonad m => [m a] -> m [a]
-- mySequence = foldr mcons (return [])
--   where
--     mcons p q = p >>= \x -> q >>= \y -> return (x : y)

myMapM :: Monad m => (a -> m b) -> [a] -> m [b]
myMapM f as = sequence (map f as)

-- 所有的Monad
-- Identity 不包含任意计算策略，仅提供bind函数
--          Identity x >>= f ==== f x
--          可以用来派生一个Monad
--          newtype Identity a = Identity { runIdentity :: a }
--
--          instance Monad Identity where
--              return a           = Identity a
--              (Identity x) >>= f = f x
--          type State s a = StateT s Identity a
-- Maybe    处理那些可能存在空值的计算逻辑，避免因空值而无法计算。计算序列中出现Nothing后我们可以选择适当的退出计算的方案
--          data Maybe a = Just a | Nothing
--
--          instance Monad Maybe where
--              return         = Just
--              Nothing >>= f  = Nothing
--              (Just x) >>= f = f x
-- Error    处理那些可能失败或者抛出异常的计算逻辑
--          class Error a where
--              noMsg  :: a
--              strMsg :: String -> a
--
--          class (Monad m) => MonadError e m | m -> e where
--              throwError :: e   -> m a
--              catchError :: m a -> (e -> m a) -> m a
--
--          用于do语句
--          do { action1; action2; action3 } `catchError` handler
--
--          instance MonadError e (Either e) where
--              throwError                    = Left
--              (Left e) `catchError` handler = handler e
--              a        `catchError` _       = a
--
--
-- List     表示一个计算可以返回多个结果的情况
--          [] 表示空  ++ 表示加法
--          instance Monad [] where
--              m >>= f  = concatMap f m
--              return x = [x]
--              fail s   = []
--
--          instance MonadPlus [] where
--              mzero = []
--              mplus = (++)
-- IO       处理带有IO操作的计算，IO操作表示那些影响到计算之外的动作，也就是说做一个计算过程的时候，对该计算过程之外的其他部分产生了影响、每次产生不同的数据、调用了IO设备、网络、文件等等操作。
--          处理那些不纯粹的函数
--          instance Monad IO where
--              return a = ... -- function from a -> IO a
--              m >>= k  = ... -- executes the IO action m and binds the value to k's input
--              fail s   = ioError (userError s)
--
-- State    处理那些存在状态值的计算
--          一序列的函数操作可以请求同一个state
--
--          纯函数无法原地修改数据(变成不纯函数、IO)
--          data MyType = MT Int Bool Char Int deriving Show
--
--          makeRandomValue :: StdGen -> (MyType, StdGen)
--          makeRandomValue g = let (n, g1) = randomR(1, 100) g
--                                  (b, g2) = random g1
--                                  (c, g3) = randomR ('a', 'z') g2
--                                  (m, g4) = randomR (-n, n) g3
--                              in (MT n b c m, g4)
--
--          newtype State s a = State { runState :: (s -> (a, s)) }
--
--          instance Monad (State s) where
--              return a        = State $ \s -> (a, s)
--              (State x) >>= f = State $ \s -> let (v, s') = x s in runState (f v) s'
--
--          class MonadState m s | m -> s where
--              get :: m s
--              put :: s -> m()
--
--          instance MonadState (State s) s where
--              get   = State $ \s -> (s, s)
--              put s = State $ \_ -> ((), s)
-- Reader   从共享环境中获取值
--          newtype Reader e a = Reader { runReader :: (e -> a) }
--
--          instance Monad (Reader e) where
--              return a         = Reader $ \e -> a
--              (Reader r) >>= f = Reader $ \e -> runReader (f (r e)) e
-- Writer   给外部环境添加值
-- Cont     可以打断的值
--
