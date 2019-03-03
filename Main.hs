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

type Point = Complex Float
type Path  = [Point]

type PointDirStack = [(Point , Point)]

interpret :: Float -> NDFPP -> PointDirStack -> PointDirStack
interpret angle (T True)  ((dir , p):ps) = (rot angle dir , p):ps
interpret angle (T False) ((dir , p):ps) = (rot (-angle) dir , p):ps
interpret angle Push      (dp:ps)        = dp:dp:ps
interpret angle Pop       (dp:ps)        = ps
interpret angle _         ((dir , p):ps) = (dir , p + dir):ps

curr :: PointDirStack -> Point
curr ((_ , p):_) = p

renderS :: Float -> [NDFPP] -> State PointDirStack [Path]
renderS angle [] = return []
renderS angle is = do
    (p , is') <- renderUntilPop is
    (p:) <$> renderS angle is'
  where
    renderUntilPop []       = return ([] , [])
    renderUntilPop (Pop:is) = do
      pt <- gets curr
      modify tail
      return ([pt] , is)
    renderUntilPop (i:is)   = do
      trace (show i) (return ())
      pt  <- gets curr
      modify (interpret angle i)
      (p , is') <- renderUntilPop is
      if i == F || i == X
      then return (pt:p , is')
      else return (p , is')
           


{-
renderS :: Float -> [NDFPP] -> State [(Complex Float , Complex Float)] [Maybe Path]
renderS angle []     = return []
renderS angle (i:is) = render1 i >>= \f -> f <$> renderS angle is
  where
    render1 :: NDFPP -> State [(Complex Float , Complex Float)]
                              (Maybe [Path])
    render1 i = do
      (dir , p):ps <- get
      case i of
        T True  -> put ((rot   angle  dir , p):ps)
                >> return id
        T False -> put ((rot (-angle) dir , p):ps)
                >> return id
        Push -> put ((dir , p):(dir , p):ps)
             >> return id
        Pop  -> put ps
             >> return (\
        _ -> let p' = dir + p
              in put ((dir, p'):ps)
              >> lift (path_ [ d_ (mvTo p <> lineTo p' <> z) , stroke_ "black" ])
-}

main :: IO ()
main = let tree = stochweedDet 123513 1
        in print $ evalState (renderS 22.5 tree) [(0 :+ 1 , mkPolar 0 0)]

{-
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

-}
