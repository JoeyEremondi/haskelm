module Foo where

import Data.List((\\),(!!))

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
maybeZero aList@(h:_:_) = Just aList

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

--Test the different operators
plusTest = 3 + 4
minusTest = 3-4
multTest = 3*4
divTest = 3.1/4.32

andTest = (3 < 2) && (4 > 3)
orTest = (3 <= 2) || (4 >= 3)
notTest = not (3 == 4)
neqTest = 4 /= 4

expCardinal = 4^2
intExp = 3.2^^(-1)
floatExp = 3.14**(3.14)

concatTest = [3] ++ [4]

--listDiffTest = [3,4] \\ [3]
consTest = (3 : 4 : 5 : [])
--indexTest = [1,2,3,4] !! 2


compTest = id . add13
-- reverseCompTest = add13 >> id
appTest = head $ concat [[1,2,3], [4,5,6]]
--reverseAppTest = ( concat [[1,2,3], [4,5,6]]) <| first

--Test pattern matching on multiple list aguments
isTwoElems someList = case someList of
  [_, _] -> True
  _ -> False
  
theEmptyList = []

isNull someList = case someList of
  [] -> True
  _ -> False

listInsideLists someNestedList = case someNestedList of
  [[1,2,3], [4,5], [6]] -> True
  _ -> False