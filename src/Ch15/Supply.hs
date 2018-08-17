{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Supply
    ( Supply
    , next
    , runSupply
    )
where

import           Control.Monad.State
import           System.Random           hiding ( next )
-- ******************************************************************************************************
-- Supply 是 State 的化名、而我们通过将 State 化名 Supply,且本module 不导出 Supply 的值构造子的方式   来实现 模块封装
-- 由于 State s 已经注册了 Monad，所以 Supply s 可直接利用 GHC 扩展功能： NewType-Deriving
newtype Supply s a = WrapSupply (State [s] a)
    deriving (Functor,Applicative,Monad)

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (WrapSupply m) = runState m



-- 在这个 monad 中，每当用户要求获取一个值时， next 就会从列表中取出下一个值并将其交给用户。每个值都被 Maybe 构造器包装以防止这个列表的长度不满足需求。
next :: Supply a (Maybe a)
next = WrapSupply $ do
    st <- get
    case st of
        []       -> return Nothing
        (x : xs) -> do
            put xs
            return (Just x)
-- ********************************* SupplyRandom.hs ************************************************************

randomsIO :: Random a => IO [a]
randomsIO = getStdRandom $ \g -> let (a, b) = split g in (randoms a, b)

showTwo :: (Show s) => Supply s String
showTwo = do
    a <- next
    b <- next
    return (show "a: " ++ show a ++ ", b: " ++ show b)

