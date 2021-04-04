module Test.Control where

import           Prelude hiding (repeat)

-- | Functions to control animation progress and completion. First argument is the result of GLFW.getTime.
class Controller c where
    -- | Are we done yet?
    done :: Double -> c -> Bool
    done now c = frac now c >= 1.0

    -- | What fraction of completion have we reached?
    frac :: Double -> c -> Double

    -- | Advance to the next frame.
    next :: Double -> c -> c

-- | Render a fixed number of frames. Ignore time.
newtype Fixed = Fixed (Int, Int) deriving Show
instance Controller Fixed where
    frac _ (Fixed (max, count)) = fromIntegral count / fromIntegral max
    next _ (Fixed tup) = Fixed $ fmap (+1) tup
frames :: Int -> Fixed
frames max = Fixed (max, 0) -- (frame count, current frame)

-- | Render for a time period.
newtype Timed = Timed (Double, Double) deriving Show
instance Controller Timed where
    frac now (Timed (dur, s)) = let start = if s < 0 then now else s
        in (now - start) / dur
    next now f@(Timed (dur, s)) = if s < 0 then Timed (dur, now) else f
seconds :: Double -> Timed
seconds duration = Timed (duration, -1) -- start time is <0 until first call to next

-- | Render another Controller forever.
newtype Repeat a = Repeat (a, Int, a) deriving Show
instance Controller a => Controller (Repeat a) where
    frac now (Repeat (begin, _, c)) = if done now c then frac now begin else frac now c
    next now (Repeat (begin, count, c)) = let nc = next now c
        in if done now nc then Repeat (begin, count + 1, begin) else Repeat (begin, count, nc)
repeat :: a -> Repeat a
repeat controller = Repeat (controller, 0, controller)

-- | Render another Controller a fixed set of times.
