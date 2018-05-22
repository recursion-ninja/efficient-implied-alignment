{-# LANGUAGE BangPatterns, Strict #-}

module System.Timing
  ( CPUTime()
  , timeOp
  ) where


import Control.DeepSeq
import Control.Monad.IO.Class
import System.CPUTime


-- | CPU time with picosecond resolution
newtype CPUTime = CPUTime Integer


instance NFData CPUTime where

    rnf (CPUTime !_) = ()


instance Show CPUTime where

    show (CPUTime x)
      | x < 1000          = let (q,_) = x `quotRem` 1             in mconcat [show q, ".", "???"                   , "ps"]
      | x < 1000000       = let (q,r) = x `quotRem` 1000          in mconcat [show q, ".", show (r `div` 1        ), "ns"]
      | x < 1000000000    = let (q,r) = x `quotRem` 1000000       in mconcat [show q, ".", show (r `div` 1000      ), "μs"]
      | x < 1000000000000 = let (q,r) = x `quotRem` 1000000000    in mconcat [show q, ".", show (r `div` 1000000   ), "ms"]
      | otherwise         = let (q,r) = x `quotRem` 1000000000000 in mconcat [show q, ".", show (r `div` 1000000000), "s "]


timeOp :: MonadIO m => m a -> m (CPUTime, a)
timeOp ioa = do
    t1 <- liftIO getCPUTime
    a  <- ioa
    t2 <- liftIO getCPUTime
    let t = CPUTime (t2 - t1)
    return (t, a)
