module SupplyInstance where

import           Control.Monad

newtype Reader e a = WrapReader {unwrapReader::e->a}

instance Functor (Reader e) where
    -- fmap :: (a->b) -> Reader e a -> Reader e b
    fab `fmap` (WrapReader fea) = WrapReader (fab. fea)

instance Applicative (Reader e) where
    -- pure :: a -> Reader e a
    pure va = WrapReader (\_ -> va  )
    -- <*>  :: (Reader e (a->b)) -> Reader e a -> Reader e b
    WrapReader feab <*> WrapReader fea = WrapReader (\e-> feab e (fea e))

instance Monad (Reader e) where
    return = pure
    -- >>= :: Reader e a -> (a-> Reader e b) -> Reader e b
    (WrapReader m) >>=  k = WrapReader $ \e -> let ra = m e in unwrapReader (k ra) e

-- get m context  (通过放到a 域)
ask :: Reader e e
ask = WrapReader id
-- *************************************************************************************************
-- | 测试： ask是取e 到a 域 （其实就是最后传入的2);    然后e 作为a 与往后传进行*3 , 最后得到6嘛
-- Wrapper 里就是一个会把e 乘以3的函数！ 然后unwrap 以后得到这个函数、以参数e=2调用
v00 = unwrapReader (ask >>= \x -> return (x * 3)) 2
-- v00 = 6
