module Foo where

--Basic data declaration
data A = B Int | C String String | D 

type AnotherA = A

--newtype, should get translated as Data
newtype OtherInt = OtherInt Int

--Where in function declaration
add13 z = 
  let 
    ww = 10
    w = ww
  in w + x + y + z
  where x = 1
        y = 2
        
--Where in value declaration
someValue = x
  where x = 3

--Guarded function bodies
theMin x y 
  | x < y     = x
  | x == y    = x
  | otherwise = y

  
--Multiple-clause function defs  
allZero 0 0 0 = True
allZero _ _ _ = False

--As-patterns
wrapMaybe [] = Nothing
maybeZero l@(h:_:_) = Just l

myRange = [1 .. 10]

myOtherRange = [33 .. 91]

--basic record type
data Point = Point {px::Int, py::Int, pz::Float}

notTheOrigin = Point {px=20, py=100, pz=0.1}

alsoNotTheOrigin = notTheOrigin {px = 30, pz=10.3}

origin = Point 0 0 0.0

funInLet = 
  let 
    plus1 x = x + 1
  in plus1 3