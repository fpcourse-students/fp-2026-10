module SpecLevel2 where

import Data.List (sort)
import Level1 (Point (..))
import Level2
import MyIO
import Point3
import Test.Prelude

tests :: NamedTests
tests = nameTests 2
  [ testDesugared
  , testSumAndMult
  , testEdges
  , testCenterMass
  , testProbSolution
  ]

testDesugared :: Test
testDesugared = TestList
  [ propertyToTest "результат и последовательность действий" \(x :: Int) y ->
      let input = world [x, 99, y, 17] in
      let mark = readInt >>= writeInt in
      runUnsafeMyIO (sugared readInt mark) input === runUnsafeMyIO (desugared readInt mark) input
  , propertyToTest "Maybe: любой шаг может завершиться неудачей" \(a :: Maybe Int) (b :: Maybe ()) ->
      sugared a b === desugared a b
  , TestCase $ assertEqual "порядок ветвления в списке"
      (sugared [2, 3] [(), ()]) (desugared [2, 3] [(), ()])
  ]

testSumAndMult :: Test
testSumAndMult = TestList
  [ TestCase $ assertEqual "текущая компонента" (Point True False) isFirstComponent
  , propertyToTest "sumAndMult" \(x1 :: Int, y1, x2, y2) ->
      Point (x1 + x2) (y1 * y2) === sumAndMult (Point x1 y1) (Point x2 y2)
  ]

testEdges :: Test
testEdges = TestList
  [ TestCase $ assertEqual "edges1" result $ take (length result) edges1
  , TestCase $ assertEqual "edges2" result $ take (length result) edges2
  ]
  where
    result =
      [ (5, 4, 3)
      , (10, 8, 6)
      , (13, 12, 5)
      , (15, 12, 9)
      , (17, 15, 8)
      , (20, 16, 12)
      , (25, 20, 15)
      , (25, 24, 7)
      , (26, 24, 10)
      , (29, 21, 20)
      , (30, 24, 18)
      , (34, 30, 16)
      , (35, 28, 21)
      , (37, 35, 12)
      , (39, 36, 15)
      , (40, 32, 24)
      , (41, 40, 9)
      , (45, 36, 27)
      , (50, 40, 30)
      , (50, 48, 14)
      ]

testCenterMass :: Test
testCenterMass = TestList
  [ TestCase $ assertEqual "центр масс: отрицательные координаты и div" (Point3 (-2) 1 4) $
      massCenter [Object 1 (Point3 0 4 2), Object 2 (Point3 (-2) 0 5)]
  , TestCase $ assertEqual "one object" (unsafeWorld [] [2, 3, 4]) $ fst $ runUnsafeMyIO centerMain $ world [1, 2, 3, 4]
  , TestCase $ assertEqual "three objs" (unsafeWorld [] [4, 10, 5]) $ fst $ runUnsafeMyIO centerMain $ world
      [ 3, 4, 42, 5
      , 2, 3, 10, 23
      , 10, 5, 1, 2
      ]
  ]

testProbSolution :: Test
testProbSolution = TestList
  [ TestCase $ assertEqual "повторы исходов при двух гранях" [4, 4, 4, 6] $ sort (allEvenOutcomes 2)
  , TestCase $ assertBool "сумма 14 недостижима" $ cmp 0 $ probSolution 4
  , TestCase $ assertBool "probSolution 10" $ cmp 0.138 $ probSolution 10
  , TestCase $ assertBool "probSolution 33" $ cmp 0.00434 $ probSolution 33
  ]
  where
    cmp q w = abs (q - w) < 0.00001
