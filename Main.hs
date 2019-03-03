{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Complex
import Data.Monoid
import Control.Arrow ((***))
import Control.Monad.State 
import Control.Monad.Identity
import System.Random
import Lucid.Svg

import Data.Text (pack)
import Data.List (foldl')

-- |Defines the finite trace of a coalgebra up to n iterations
traceN :: (Monad m) => Int -> (x -> m [x]) -> x -> m [x]
traceN 0 c seed = return []
traceN 1 c seed = c seed
traceN n c seed = concat <$> (traceN (n-1) c seed >>= mapM c)

data NDFPP = F | T Bool | Push | Pop | X 
  deriving (Eq , Show)

-- |Stochastic seaweed L-system
--
-- > F=FF
-- > X=F-[[X]+X]+F[+FX]-X
--    ,F+[[X]-X]-F[-FX]+X
--
stochweed :: (RandomGen g) => NDFPP -> State g [NDFPP]
stochweed F = return [F , F]
stochweed X = run <$> state random 
  where
    run b = [F , T (not b) , Push , Push , X , Pop , T b , X , Pop
            ,T b , F , Push , T b , F , X , Pop , T (not b) , X]
stochweed x = return [x]

type Seed = Int

stochweedDet :: Seed -> Int -> [NDFPP]
stochweedDet seed n = evalState (traceN n stochweed X) (mkStdGen seed)

-- * Translates to a better datatype

data Seaweed
  = Up Seaweed
  | TL Seaweed
  | TR Seaweed
  | Branch Seaweed Seaweed
  | Tip
  deriving (Eq , Show)

toSeaweed :: [NDFPP] -> (Seaweed , [NDFPP])
toSeaweed []        = (Tip , [])
toSeaweed (Pop:is)  = (Tip , is)
toSeaweed (Push:is) = 
  let (sw  , rest)  = toSeaweed is
      (sw' , rest') = toSeaweed rest
   in (Branch sw sw' , rest')
toSeaweed (T True:is)  = (TL *** id) $ toSeaweed is
toSeaweed (T False:is) = (TR *** id) $ toSeaweed is
toSeaweed (F:is)       = (Up *** id) $ toSeaweed is
toSeaweed (X:is)       = (Up *** id) $ toSeaweed is

onHead :: (a -> a) -> [a] -> [a]
onHead f [] = []
onHead f (x:xs) = f x : xs

type Dir = Point

a :: Float
a = pi / 8

step :: Float
step = 30

render :: Float -> Point -> Seaweed -> [Path]
render angle p Tip = [[p]]
render angle p (Up swd)
  -- complex rotation. Watch out, angles are in radians
  = let p' = p + (0 :+ step) * cis angle
     in map ([p] ++) $ render angle p' swd
render angle p (TL swd) = render (angle + a) p swd
render angle p (TR swd) = render (angle - a) p swd
render angle p (Branch l r)
  = render angle p l ++ (render angle p r)

type Point = Complex Float
type Path  = [Point]

render0 :: Seaweed -> [Path]
render0 = render (pi/2) (0 :+ 0)

path2svg :: (Point -> Point) -> Path -> Svg ()
path2svg tr ps = do
  polyline_ [points_ (pack $ concatMap (toStr . tr) ps)
            , fill_ "none"
            , stroke_ "black"
            , stroke_width_ (pack $ show $ strokew $ fromIntegral (length ps))
            ]
  where
    toStr (x :+ y) = show x ++ " " ++ show y

    strokew n = let minstr = 0.02
                    maxstr = 2
                    delta   = 60
                 in (maxstr - minstr) / delta * n

seaweed2svg :: (Point -> Point) -> Seaweed -> Svg ()
seaweed2svg tr = mapM_ (path2svg tr) . render0

genSvg :: Int -> Int -> Svg ()
genSvg seed n = do
  seaweed2svg (mkTr $ fromIntegral n) . fst . toSeaweed . stochweedDet seed $ n
 where
   mkTr n (x :+ y) = (x :+ (100 * n * n + y))

main :: IO ()
main = do
  writeFile "res.svg" $ show $ svg $
    do let seed = 42
       mapM_ (\i -> g_ $ genSvg ((seed ^ i) `mod` 63557) i) [1..5]

svg :: Svg () -> Svg ()
svg content = do
  doctype_
  with (svg11_ content) [version_ "1.1", width_ "300" , height_ "200"]

