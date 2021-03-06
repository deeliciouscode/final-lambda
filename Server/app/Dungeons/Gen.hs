{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Dungeons.Gen where

import Prelude as P
import Dungeons.Config
import Helpers
import Dungeons.NaiveMST
import Types
import Data.List as LIST
import qualified Data.List.NonEmpty as LNE
import Data.Random.Normal
import Data.Tree as Tree
import Data.Ext as EXT
import Data.Map.Internal as MAP
import Graphics.Gloss as GLOSS
import Graphics.Gloss.Export.Image
import Codec.Picture
import System.Random
import Control.Lens as LENS
import Data.Geometry as GEO
import Algorithms.Geometry.DelaunayTriangulation.Naive
import Algorithms.Geometry.DelaunayTriangulation.Types

------------------------------- Generate -------------------------------

generateDungeon :: Int -> Float -> IO (Picture, PointF, Circles)
generateDungeon seed sideLen = do
    let circles = zip (radii seed) (centers seed sideLen)
    let circles' = flockCircles circles
    let circles'' = removeCircles'' circles'
    let mst = makeMST . toNonEmptyList $ toPoints' circles''
    let triangles = triangulate . toNonEmptyList $ toPoints' circles''
    let picture = combinePictures triangles mst circles''
    return (picture, snd $ last circles'', LIST.take 30 circles'') 

------------------------------- Setup -------------------------------

radii :: Int -> [Float]
radii seed = reverse . sort $ LIST.take nCircles (mkNormals' (meanRadius, sd) seed)

centers :: Int -> Float -> [PointF]
centers seed sideLen = zip xCors yCors
            where
                xCors = nRandoms nCircles (sideLen * 0.2) (sideLen * 0.8) (mkStdGen seed)
                yCors = nRandoms nCircles (sideLen * 0.2) (sideLen * 0.8) (mkStdGen (seed+1))

circles :: Circles 
circles = zip (radii testSeed) (centers testSeed sideLen)

testCircles :: Circles
testCircles = [(26.77658,(196.68921,236.81659)),(30.364641,(339.12732,210.95541)),(33.663124,(227.78206,344.70084)),(35.233494,(275.62262,281.27032)),(38.001255,(257.7942,301.88495)),(39.244896,(288.4538,203.14005))]


------------------------------- Flocking -------------------------------    

circles' = flockCircles circles

flockCircles :: Circles -> Circles
flockCircles = stripCircles . iterateOver 0 . enrichCircles

enrichCircles :: Circles -> Agents
enrichCircles [] = []
enrichCircles ((r, (x,y)):rest) = (r, (x,y), (vx',vy'), 0) : enrichCircles rest
                    where
                        gen = mkStdGen $ round (420 / x + y) :: StdGen
                        (vx, gen') = randomR (-1,1) gen :: (Float, StdGen)
                        (vy, _) = randomR (-1,1) gen' :: (Float, StdGen)
                        (vx', vy') = normalize' (vx, vy)

stripCircles :: Agents -> Circles
stripCircles [] = []
stripCircles ((r, (x,y), (vx,vy), n):rest) = (r, (x,y)) : stripCircles rest


------------------------------- Flocking.Iteration -------------------------------    

-- try out best combinations of alignment, cohesion and separation
iterateOver :: Int -> Agents -> Agents
iterateOver n agents
            | n >= iterations = agents
            | otherwise       = iterateOver (n + 1) (flock agents)

flock :: Agents -> Agents
flock agents = applyIteration $ LIST.transpose [agents, align agents, separate agents] -- [align agents, makeCoherent agents, separate agents]

flock' :: Agents -> Agents
flock' agents = applyIteration $ LIST.transpose [agents, align agents, makeCoherent agents, separate agents]

applyIteration :: [Agents] -> Agents
applyIteration = LIST.map applyVectors

applyVectors :: Agents -> Agent
applyVectors [] = (0, (0,0), (0,0), 0)
applyVectors (first:rest) = moveAgent $ normalizeAgent genVelocity $ foldIntoOne first rest

foldIntoOne :: Agent -> Agents -> Agent
foldIntoOne agent [] = agent
foldIntoOne (r, (x,y), (vx,vy), _) ((_, _, (vx',vy'), _):rest) = foldIntoOne (r, (x,y), (vx+vx',vy+vy'), 0) rest

normalizeAgent :: Float -> Agent -> Agent
normalizeAgent multiplier (r, (x,y), (vx,vy), n) = (r, (x,y), (vx'', vy''), n)
                            where
                                (vx',vy') = normalize' (vx,vy)
                                vx'' = vx' * multiplier
                                vy'' = vy' * multiplier

moveAgent :: Agent -> Agent
moveAgent (r, (x,y), (vx,vy), n) = (r, (x+vx,y+vy), (vx,vy), n)


------------------------------- Flocking.Neigbors -------------------------------    

countNeigborsWith :: Agents -> FlockingFunction -> Agents
countNeigborsWith agents = countNeigborsIWith agents 0

countNeigborsIWith :: Agents -> Int -> FlockingFunction -> Agents
countNeigborsIWith agents i f
                    | i == length agents = agents
                    | otherwise = countNeigborsIWith (countNeigborsIWith' agents i f) (i+1) f

countNeigborsIWith' :: Agents -> Int -> FlockingFunction -> Agents
countNeigborsIWith' agents i f = LIST.take i agents ++ (agent' : LIST.drop (i+1) agents)
                    where
                        agent' = f (agents !! i) agents


------------------------------- Flocking.Alignment -------------------------------    

align :: Agents -> Agents
align agents = normalizeAlignment $ countNeigborsWith agents calcAlignmentVector

calcAlignmentVector :: Agent -> Agents -> Agent
calcAlignmentVector agent [] = agent
calcAlignmentVector agent@(r, (x,y), (vx,vy), n) ((r', (x',y'), (vx',vy'), _):rest)
                    | r == r' && x == x' && y == y' = calcAlignmentVector agent rest
                    | distance (x,y) (x',y') <= neighborThreshold = calcAlignmentVector (r, (x,y), (vx + vx',vy + vy'), n+1) rest
                    | otherwise = calcAlignmentVector agent rest

normalizeAlignment :: Agents -> Agents
normalizeAlignment [] = []
normalizeAlignment ((r, (x,y), (vx,vy), n):rest) = (r, (x,y), (vx',vy'), n) : normalizeAlignment rest
                                where
                                    nF = fromIntegral n :: Float
                                    (vx',vy') = if n /= 0 then normalize' (vx/nF, vy/nF)
                                                else (0, 0)


------------------------------- Flocking.Cohesion -------------------------------    

makeCoherent :: Agents -> Agents
makeCoherent agents = normalizeCohesion $ countNeigborsWith agents calcCohesionVector

calcCohesionVector :: Agent -> Agents -> Agent
calcCohesionVector agent [] = agent
calcCohesionVector agent@(r, (x,y), (vx,vy), n) ((r', (x',y'), (vx',vy'), _):rest)
                    | r == r' && x == x' && y == y' = calcCohesionVector agent rest
                    | distance (x,y) (x',y') <= neighborThreshold = calcCohesionVector (r, (x,y), (vx + x',vy + y'), n+1) rest
                    | otherwise = calcCohesionVector agent rest

normalizeCohesion :: Agents -> Agents
normalizeCohesion [] = []
normalizeCohesion ((r, (x,y), (vx,vy), n):rest) = (r, (x,y), (vx',vy'), n) : normalizeCohesion rest
                                where
                                    nF = fromIntegral n :: Float
                                    vxCenter = vx / nF  - x
                                    vyCenter = vy / nF - y
                                    (vx',vy') = if n /= 0 then normalize' (vxCenter, vyCenter)
                                                else (0, 0)


------------------------------- Flocking.Separation -------------------------------    

separate :: Agents -> Agents
separate agents = normalizeSeparation $ countNeigborsWith agents calcSeparationVector

calcSeparationVector :: Agent -> Agents -> Agent
calcSeparationVector agent [] = agent
calcSeparationVector agent@(r, (x,y), (vx,vy), n) ((r', (x',y'), (_,_), _):rest)
                    | r == r' && x == x' && y == y' = calcSeparationVector agent rest
                    | distance (x,y) (x',y') <= neighborThreshold = calcSeparationVector (r, (x,y), (vx'',vy''), n+1) rest
                    | otherwise = calcSeparationVector agent rest
                        where
                            vx'' = vx + x' - x
                            vy'' = vy + y' - y

normalizeSeparation :: Agents -> Agents
normalizeSeparation [] = []
normalizeSeparation ((r, (x,y), (vx,vy), n):rest) = (r, (x,y), (vx',vy'), n) : normalizeSeparation rest
                                where
                                    nF = fromIntegral n :: Float
                                    (vx',vy') = if n /= 0 then normalize' (-1*vx/nF, -1*vy/nF)
                                                else (0, 0)


------------------------------- Graph -------------------------------    

-- TODO: keep all in convex hull and then some in the middle too
-- https://hackage.haskell.org/package/hgeometry-0.14/docs/Data-Geometry-Polygon-Convex.html#t:ConvexPolygon

-- circles''' = removeCircles circles''
circles'' = removeCircles'' circles'

-- TODO: remove the ones closer to center and smaller ones with higher probability
removeCircles :: Circles -> Circles
removeCircles = removeCircles' (mkStdGen 42)
-- removeCircles circles = removeCircles'' circles

removeCircles' :: StdGen -> Circles -> Circles
removeCircles' _ [] = []
removeCircles' gen (x:xs) = xOrEmpty ++ removeCircles' gen' xs
                    where
                        (p, gen') = randomR (0,1) gen :: (Float, StdGen)
                        xOrEmpty = [x | p < 0.6]

removeCircles'' :: Circles -> Circles
removeCircles'' = LIST.take (round $ fromIntegral nCircles / 2)


------------------------------- Delauny / Minimum Spanning Tree -------------------------------    

-- https://hackage.haskell.org/package/hgeometry-0.14/docs/Algorithms-Geometry-DelaunayTriangulation-DivideAndConquer.html
-- https://hackage.haskell.org/package/hgeometry-0.14/docs/Algorithms-Geometry-EuclideanMST.html
-- https://travellermap.com/tmp/delaunay.htm

showTree = putStr $ drawTree $ fmap show mst
mst = makeMST . toNonEmptyList $ toPoints' circles''
triangles = triangulate . toNonEmptyList $ toPoints' circles''

toPoints' :: Circles -> [GEO.Point 2 Float EXT.:+ Float]
toPoints' [] = []
toPoints' ((r, (x, y)):xs) = (Point2 x y EXT.:+ r) : toPoints' xs

toNonEmptyList :: [GEO.Point 2 Float EXT.:+ Float] -> LNE.NonEmpty (GEO.Point 2 Float EXT.:+ Float)
toNonEmptyList = LNE.fromList

triangulate :: LNE.NonEmpty (GEO.Point 2 Float EXT.:+ Float) -> Triangulation Float Float
triangulate = delaunayTriangulation

-- from some reason any list longer than 15 does not terminate
makeMST :: LNE.NonEmpty (GEO.Point 2 Float EXT.:+ Float) -> Tree.Tree (GEO.Point 2 Float EXT.:+ Float)
makeMST = euclideanMST

------------------------------- Gloss -------------------------------    

-- Tutorial: https://andrew.gibiansky.com/blog/haskell/haskell-gloss/

saveGloss :: IO (PointF, Circles)
saveGloss = do
    exportPictureToFormat writePng (round sideLen, round sideLen) black "images/test_gloss.png" objects
    let metaInfo = (snd $ last circles'', LIST.take 30 circles'')
    return metaInfo

objects :: Picture
objects = combinePictures triangles mst circles''

combinePictures :: Triangulation Float Float -> Tree (GEO.Point 2 Float :+ Float) -> Circles -> Picture
combinePictures triangles mst circles = pictures $ circlesToPictures circles ++ treeToTunnels triangles mst

circlesToPictures :: Circles -> [Picture]
circlesToPictures [] = []
circlesToPictures ((r, (x,y)):xs) = circle : circlesToPictures xs
                    where
                        ballColor = white
                        circle = GLOSS.translate (-midDungeon+x) (-midDungeon+y) $ color ballColor $ circleSolid r

treeToTunnels :: Triangulation Float Float -> Tree.Tree (GEO.Point 2 Float EXT.:+ Float) -> [Picture]
treeToTunnels triangles tree =
    case subForest tree of
        [] -> []
        subTrees -> makeLines triangles (rootLabel tree) subTrees ++ treesToTunnels triangles subTrees

treesToTunnels :: Triangulation Float Float -> [Tree.Tree (GEO.Point 2 Float EXT.:+ Float)] -> [Picture]
treesToTunnels triangles = concatMap $ treeToTunnels triangles

makeLines :: Triangulation Float Float -> GEO.Point 2 Float EXT.:+ Float -> [Tree.Tree (GEO.Point 2 Float EXT.:+ Float)] -> [Picture]
makeLines _ _ [] = []
makeLines triangles point (t:ts) = picture' : makeLines triangles point ts
                where
                    x = _core point ^. xCoord
                    y = _core point ^. yCoord
                    r = _extra point
                    point' = rootLabel t
                    x' = _core point' ^. xCoord
                    y' = _core point' ^. yCoord
                    r' = _extra point'
                    paths = calcPaths (x,y) (x',y') (r,r')
                    additionalPaths = additionalTunnel triangles point $ mkStdGen $ round $ x*y
                    pictures' = LIST.map (color white . polygon) (paths ++ additionalPaths)
                    picture = pictures pictures'
                    picture' = GLOSS.translate (-midDungeon) (-midDungeon) picture

additionalTunnel :: Triangulation Float Float -> GEO.Point 2 Float EXT.:+ Float -> StdGen -> [Path]
additionalTunnel triangles point gen = tunnelOrEmpty
                    where
                        (p, gen') = randomR (0,1) gen :: (Float, StdGen)
                        tunnelOrEmpty = if p < 0.7 then []
                                        else pathToRandomNeigbor triangles point gen'

pathToRandomNeigbor :: Triangulation Float Float -> GEO.Point 2 Float EXT.:+ Float -> StdGen -> [Path]
pathToRandomNeigbor triangles point gen = case MAP.lookup (_core point) $ view vertexIds triangles of
                                    Nothing -> []
                                    Just i -> case view neighbours triangles ^? LENS.element i of
                                        Nothing -> []
                                        Just fList -> case fList ^? LENS.element randy of
                                            Nothing -> []
                                            Just iNeighbour -> case view positions triangles ^? LENS.element iNeighbour of
                                                Nothing -> []
                                                Just neighbour -> calcPaths (x,y) (x',y') (r,r')
                                                    where
                                                        x = _core point ^. xCoord
                                                        y = _core point ^. yCoord
                                                        r = _extra point
                                                        x' = _core neighbour ^. xCoord
                                                        y' = _core neighbour ^. yCoord
                                                        r' = _extra neighbour
                                            where
                                                (i, _) = randomR (0,fromIntegral $ length fList - 1) gen :: (Float, StdGen)
                                                randy = round i

calcPaths :: PointF -> PointF -> PointF -> [Path]
calcPaths (x,y) (x',y') (r,r') = paths
                where
                    a = y'-y
                    b = x'-x

                    mx = a / b
                    mx' = -1 / mx

                    dx1 = r / sqrt (mx'**2 + 1)
                    dy1 = dx1 * mx'
                    dx2 = -dx1
                    dy2 = -dy1

                    dx'1 = r' / sqrt (mx'**2 + 1)
                    dy'1 = dx'1 * mx'
                    dx'2 = -dx'1
                    dy'2 = -dy'1

                    tri1 = [(x,y),(x',y'),(x+dx1,y+dy1)]
                    tri2 = [(x,y),(x',y'),(x+dx2,y+dy2)]
                    tri3 = [(x,y),(x',y'),(x'+dx'1,y'+dy'1)]
                    tri4 = [(x,y),(x',y'),(x'+dx'2,y'+dy'2)]

                    paths = [tri1,tri2,tri3,tri4]
