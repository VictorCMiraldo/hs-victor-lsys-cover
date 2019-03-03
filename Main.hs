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

{-
path2ps :: Path -> String
path2ps = unwords . map pp
  where
    pp (x :+ y) = show x ++ "," ++ show y 

seaweed2svg :: Seaweed -> [String]
seaweed2svg = map path2ps . render0
-}


main :: IO ()
main = let tree = stochweedDet 123513 3
           (swd , _) = toSeaweed tree
        in do
          writeFile "res.svg" (show $ svg $ seaweed2svg swd)

svg :: Svg () -> Svg ()
svg content = do
  doctype_
  with (svg11_ content) [version_ "1.1", width_ "300" , height_ "200"]



-- ps = "24.338237,157.42167 19.353297,-9.38342 2.345854,-10.55634 15.248051,-12.0225"
buildPath :: String -> String
buildPath ps = unlines $
  [ "    <path"
  , "       style=\"fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1\""
  , "       d=\"m" ++ ps ++ "\""
  , "       id=\"path1448\""
  , "       inkscape:connector-curvature=\"0\" />"
  ]

builder :: [String] -> String
builder ps = unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
  , "<!-- Created with Inkscape (http://www.inkscape.org/) -->"
  , ""
  , "<svg"
  , "   xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
  , "   xmlns:cc=\"http://creativecommons.org/ns#\""
  , "   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\""
  , "   xmlns:svg=\"http://www.w3.org/2000/svg\""
  , "   xmlns=\"http://www.w3.org/2000/svg\""
  , "   xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\""
  , "   xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\""
  , "   width=\"210mm\""
  , "   height=\"297mm\""
  , "   viewBox=\"0 0 210 297\""
  , "   version=\"1.1\""
  , "   id=\"svg882\""
  , "   inkscape:version=\"0.92.3 (2405546, 2018-03-11)\""
  , "   sodipodi:docname=\"inkscape-settings.svg\">"
  , "  <defs"
  , "     id=\"defs876\" />"
  , "  <sodipodi:namedview"
  , "     id=\"base\""
  , "     pagecolor=\"#ffffff\""
  , "     bordercolor=\"#666666\""
  , "     borderopacity=\"1.0\""
  , "     inkscape:pageopacity=\"0.0\""
  , "     inkscape:pageshadow=\"2\""
  , "     inkscape:zoom=\"0.90230103\""
  , "     inkscape:cx=\"35.38674\""
  , "     inkscape:cy=\"604.30222\""
  , "     inkscape:document-units=\"mm\""
  , "     inkscape:current-layer=\"layer1\""
  , "     showgrid=\"false\""
  , "     inkscape:window-width=\"1920\""
  , "     inkscape:window-height=\"1033\""
  , "     inkscape:window-x=\"0\""
  , "     inkscape:window-y=\"28\""
  , "     inkscape:window-maximized=\"0\" />"
  , "  <metadata"
  , "     id=\"metadata879\">"
  , "    <rdf:RDF>"
  , "      <cc:Work"
  , "         rdf:about=\"\">"
  , "        <dc:format>image/svg+xml</dc:format>"
  , "        <dc:type"
  , "           rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />"
  , "        <dc:title></dc:title>"
  , "      </cc:Work>"
  , "    </rdf:RDF>"
  , "  </metadata>"
  , "  <g"
  , "     inkscape:label=\"Layer 1\""
  , "     inkscape:groupmode=\"layer\""
  , "     id=\"layer1\">"
  ] ++ map buildPath ps ++
  [ "  </g>"
  , "</svg>"
  ]
{-
main :: IO ()
main = let tree = stochweedDet 123513 3
        in print $ svg $ evalStateT (renderS 22.5 tree) [(0 :+ 10 , mkPolar 0 0)]
-}


{-

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


main :: IO ()
main = let tree = stochweedDet 123513 3
        in print $ svg $ evalStateT (renderS 22.5 tree) [(0 :+ 10 , mkPolar 0 0)]

-}

-}
