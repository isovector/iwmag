module Main where

import Data.Default
import FRP.Helm
import FRP.Helm.Signal
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window

data SnakeState = SnakeState {body :: [(Int, Int)], momentum :: (Int, Int)}

snakeSignal :: Signal SnakeState
snakeSignal = moveSignal
  where
    orthogonal (x, y) | x == 0    = (0, y)
                      | otherwise = (x, 0)

    initialState = SnakeState { body = map (\x -> (0, x)) [-6..0], momentum = (0, 1) }

    newState :: Either (Int, Int) Double -> SnakeState -> SnakeState
    newState (Left (dx, dy)) state =
        state { body = body state
              , momentum = momentum' }
      where
        momentum' | (dx, dy) == (0, 0) = momentum state
                  | otherwise          = (dx, dy)

    newState (Right dt) state =
        state { body = body'
              , momentum = momentum state }
      where
        body' = (tail . body $ state) ++ [next]
        next  = let (mx, my) = momentum state
                    (x, y) = last . body $ state
                in (x + mx, y + my)

    moveSignal = foldp newState initialState
        (mux  [ (Left . orthogonal) <~ Keyboard.arrows
              , Right <~ stepSignal])


stepSignal :: Signal Double
stepSignal = signal
  where
    signal = foldp step 0 (Time.fps 5)
    step :: Time -> Double -> Double
    step dt n = n + Time.inSeconds dt


{-Ensure our Either value is constructable with a mux-}
instance (Default b) => Default (Either a b) where
    def = Right def

mux :: (Eq a, Default a) => [Signal a] -> Signal a
mux xs = snd <~ (foldp select def $ combine xs)
  where
    select xs (old, result) = (xs, result')
      where
        result' | diff == [] = result
                | otherwise  = snd . head $ diff
        diff = filter (\(o, n) -> o /= n) $ zip old xs



stroke = outlined $ solid red
frame = stroke $ rect 400 760

block :: Int -> Int -> Form
block x y = move (fromIntegral $ 40 * x, fromIntegral $ 40 * y) $
    filled white $ rect 40 40

snake :: SnakeState -> [Form]
snake state = map (\(x, y) -> block x y) . body $ state

render :: SnakeState -> (Int, Int) -> Element
render state (w, h) = centeredCollage w h $ [frame] ++ snake state

main :: IO ()
main = do
    run config $ render <~ snakeSignal ~~ Window.dimensions

  where
    config = defaultConfig { windowTitle = "Ferocious Snakes" }
