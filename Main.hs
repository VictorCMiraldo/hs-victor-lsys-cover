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

rot :: Float -> Complex Float -> Complex Float
rot angle = (* cis angle)

{-
render :: Float -> [NDFPP] -> State [Complex Float] (Svg ())
render angle = foldl' (\r x -> r >>= go x) (return $ return ())
  where
    go :: NDFPP -> Svg () -> State [Complex Float] (Svg ())
    go F = path_ [ d_ (mA 
-}

mvTo   c = mA (realPart c) (imagPart c)
lineTo c = lA (realPart c) (imagPart c)

-- state is current dir and current pos
renderS :: Float -> [NDFPP] -> StateT [(Complex Float , Complex Float)] Svg ()
renderS angle []     = return ()
renderS angle (i:is) = render1 i >> renderS angle is
  where
    render1 :: NDFPP -> StateT [(Complex Float , Complex Float)] Svg ()
    render1 i = do
      (dir , p):ps <- get
      case i of
        T True  -> put ((rot   angle  dir , p):ps)
        T False -> put ((rot (-angle) dir , p):ps)
        Push -> put ((dir , p):(dir , p):ps)
        Pop  -> put ps
        _ -> let p' = dir + p
              in put ((dir, p'):ps)
              >> lift (path_ [ d_ (mvTo p <> lineTo p' <> z) , stroke_ "black" ])

svg :: Svg () -> Svg ()
svg content = do
  doctype_
  with (svg11_ content) [version_ "1.1", width_ "300" , height_ "200"]

main :: IO ()
main = let tree = stochweedDet 123513 3
        in print $ svg $ evalStateT (renderS 22.5 tree) [(0 :+ 10 , mkPolar 0 0)]
