import Prob
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit

tests =
  [ testGroup
      "Prob"
      [ testGroup
          "uniform"
          [ testCase "dist 1" (uniform ['A', 'B'] @?= D [('A', 1 / 2), ('B', 1 / 2)]),
            testCase "dist 2" (uniform [1, 2, 3] @?= D [(1, 1 / 3), (2, 1 / 3), (3, 1 / 3)]),
            testCase "show 1" (show (uniform ['A', 'B']) @?= "[('A',0.5),('B',0.5)]"),
            testCase "show 2" (show (uniform ['A', 'A', 'B', 'C']) @?= "[('A',0.5),('B',0.25),('C',0.25)]")
          ],
        testCase "(??) 1" ((==) 'A' ?? uniform ['A', 'B'] @?= 1 / 2),
        testCase "(??) 2" ((==) 'A' ?? uniform ['A', 'A', 'B'] @?= 2 / 3)
      ]
  ]

main :: IO ()
main = defaultMain tests
