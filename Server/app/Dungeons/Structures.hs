module Dungeons.Structures where

--   Circle = (r, (x,y))
type Circle = (Float, (Float, Float))
type Circles = [Circle]
--   Agent = (r, (x,y), (vx,vy), n)
type Agent = (Float, (Float, Float), (Float, Float), Int)
type Agents = [Agent]
type FlockingFunction = (Agent -> Agents -> Agent)
type Matrix = [[Int]]