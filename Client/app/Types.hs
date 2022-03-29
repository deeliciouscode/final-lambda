module Types where

type PointF = (Float, Float)
type PointI = (Int, Int)
type Circle = (Float, PointF)
type Circles = [Circle]

--   Agent = (r, (x,y), (vx,vy), n)
type Agent = (Float, PointF, PointF, Int)
type Agents = [Agent]
type FlockingFunction = (Agent -> Agents -> Agent)

--                    pos    dir     hb      vel    per
type MovementAttr = (PointF, PointF, PointF, Float, Float)
--                 id   stam   stre   
type CombatAttr = (Int, Float, Float)
--                         pos     dir     vel 
type PlayerMovementAttr = (PointF, PointF, Float)