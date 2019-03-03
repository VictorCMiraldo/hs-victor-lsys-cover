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


import Debug.Trace

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

render :: Float -> Point -> Seaweed -> [Path]
render angle p Tip = [[p]]
render angle p (Up swd)
  -- complex rotation. Watch out, angles are in radians
  = let p' = p + (0 :+ 30) * cis angle
     in onHead ([p] ++) $ render angle p' swd
render angle p (TL swd) = render (angle + a) p swd
render angle p (TR swd) = render (angle - a) p swd
render angle p (Branch l r)
  = render angle p l ++ (render angle p r)

type Point = Complex Float
type Path  = [Point]

render0 :: Seaweed -> [Path]
render0 = render (pi/2) (0 :+ 0)

path2svg :: Path -> Svg ()
path2svg ps = do
  polyline_ [points_ (pack $ concatMap toStr ps)
            , fill_ "none"
            , stroke_ "black"
            ]
  where
    toStr (x :+ y) = show x ++ " " ++ show y

seaweed2svg :: Seaweed -> Svg ()
seaweed2svg = mapM_ path2svg . render0

main :: IO ()
main = let tree = stochweedDet 123513 3
           (swd , _) = toSeaweed tree
        in do
          writeFile "res.svg" (show $ svg $ seaweed2svg swd)

svg :: Svg () -> Svg ()
svg content = do
  doctype_
  with (svg11_ content) [version_ "1.1", width_ "300" , height_ "200"]

