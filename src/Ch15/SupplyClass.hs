{-# LANGUAGE FlexibleInstances #-} -- 注册typeclass 的instance 时，可显式写其中函数的类型签名
{-# LANGUAGE FunctionalDependencies #-} -- 功能依赖
{-# LANGUAGE MultiParamTypeClasses #-} -- 多参数typeclass
module SupplyClass
        ( MonadSupply(..)
        , S.Supply
        , S.runSupply
        )
where
import qualified Supply                        as S
-- ---  ---  ---  --- 
import           Control.Monad

-- ******************************************************************************************************
-- | ”MonadSupply s“  作为一个typeclass.   可以将一个 Monad 类型m 注册为其instance。   
-- | 通过功能依赖，我们告诉类型检查器： 一旦它看到一些 monad m 在 MonadSupply s 的上下文中被使用，那么类型 s 就是唯一可以接受的类型（m s 是唯一的monadic value 类型）
class (Monad m) => MonadSupply s m | m -> s where
        next :: m (Maybe s)


instance MonadSupply s (S.Supply s) where
        next = S.next
