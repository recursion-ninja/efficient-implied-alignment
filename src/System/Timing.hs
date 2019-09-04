{-# LANGUAGE BangPatterns, Strict #-}

module System.Timing
  ( CPUTime()
  , fromPicoseconds
  , fromMilliseconds
  , fromMicroseconds
  , toPicoseconds
  , toMicroseconds
  , toMilliseconds
  , timeOp
  ) where


import Control.DeepSeq
import Control.Monad.IO.Class
import Numeric.Natural
import System.CPUTime


-- | CPU time with picosecond resolution
newtype CPUTime = CPUTime Integer


instance NFData CPUTime where

    rnf (CPUTime !_) = ()


instance Show CPUTime where

    show (CPUTime x)
      | x < nSecond = let (q,_) = x `quotRem` 1       in mconcat [show q, ".", "???"                       , "ps" ]
      | x < μSecond = let (q,r) = x `quotRem` nSecond in mconcat [show q, ".", zeroPad 3 (r `div` 1       ), "ns" ]
      | x < mSecond = let (q,r) = x `quotRem` μSecond in mconcat [show q, ".", zeroPad 3 (r `div` nSecond ), "μs" ]
      | x <  second = let (q,r) = x `quotRem` mSecond in mconcat [show q, ".", zeroPad 3 (r `div` μSecond ), "ms" ]
      | x <  minute = let (q,r) = x `quotRem`  second in mconcat [show q, ".", zeroPad 3 (r `div` mSecond ), "s " ]
      | x <    hour = let (q,r) = x `quotRem`  minute in mconcat [show q, "m", zeroPad 2 (r `div`  second ), "sec"]
      | x <     day = let (q,r) = x `quotRem`    hour in mconcat [show q, "h", zeroPad 2 (r `div`  minute ), "min"]
      | otherwise   = let (q,r) = x `quotRem`     day in mconcat [show q, "d", zeroPad 2 (r `div`    hour ), "hrs"]
      where
        nSecond = 1000
        μSecond = 1000 * nSecond
        mSecond = 1000 * μSecond
        second  = 1000 * mSecond
        minute  = 60   *  second
        hour    = 60   *  minute
        day     = 24   *  hour


zeroPad :: Int -> Integer -> String
zeroPad k i = replicate (k - length shown) '0' <> shown
  where
    shown = show i


timeOp :: MonadIO m => m a -> m (CPUTime, a)
timeOp ioa = do
    t1 <- liftIO getCPUTime
    a  <- ioa
    t2 <- liftIO getCPUTime
    let t = CPUTime (t2 - t1)
    pure (t, a)


fromPicoseconds :: Natural -> CPUTime
fromPicoseconds = CPUTime . toInteger


fromMicroseconds :: Natural -> CPUTime
fromMicroseconds = CPUTime . (*1000000000) .  toInteger


fromMilliseconds :: Natural -> CPUTime
fromMilliseconds = CPUTime . (*1000000) .  toInteger


toPicoseconds :: CPUTime -> Natural
toPicoseconds (CPUTime x) = fromInteger x


toMicroseconds :: CPUTime -> Natural
toMicroseconds (CPUTime x) = fromInteger $ x `div` 1000000


toMilliseconds :: CPUTime -> Natural
toMilliseconds (CPUTime x) = fromInteger $ x `div` 1000000000
