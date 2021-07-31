import Prob
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit

tests =
  [ testGroup
      "Prob"
      [ testCase "compress 1" (compress (uniform ['A', 'B']) @?= "[('A',0.5),('B',0.5)]"),
        testCase "compress 2" (compress (uniform ['A', 'A', 'B', 'C']) @?= "[('A',0.5),('B',0.25),('C',0.25)]"),
        testCase "uniform 1" (uniform ['A', 'B'] @?= D [('A', 1 / 2), ('B', 1 / 2)]),
        testCase "uniform 2" (uniform [1, 2, 3] @?= D [(1, 1 / 3), (2, 1 / 3), (3, 1 / 3)]),
        testCase "(??) 1" ((==) 'A' ?? uniform ['A', 'B'] @?= 1 / 2),
        testCase "(??) 2" ((==) 'A' ?? uniform ['A', 'A', 'B'] @?= 2 / 3),
        testCase
          "join 1"
          ( join (,) (uniform [1, 2]) (uniform [3, 4])
              @?= D [((1, 3), 0.25), ((1, 4), 0.25), ((2, 3), 0.25), ((2, 4), 0.25)]
          ),
        testCase
          "join 2"
          ( join (+) (uniform [1, 2]) (uniform [3, 4])
              @?= D [(4, 0.25), (5, 0.25), (5, 0.25), (6, 0.25)]
          ),
        testCase
          "prod"
          ( prod (uniform [1, 2]) (uniform [3, 4])
              @?= D [((1, 3), 0.25), ((1, 4), 0.25), ((2, 3), 0.25), ((2, 4), 0.25)]
          ),
        testGroup
          "Functor"
          [ testCase "fmap" (fmap (+ 1) (uniform [1 .. 3]) @?= D [(2, 1 / 3), (3, 1 / 3), (4, 1 / 3)])
          ],
        testGroup
          "Applicative"
          [ testCase "pure" ((pure 'A' :: Dist Char) @?= D [('A', 1)]),
            testCase
              "<*>"
              ( uniform [(+ 1), (+ 2)] <*> uniform [1, 2]
                  @?= D [(2, 0.25), (3, 0.25), (3, 0.25), (4, 0.25)]
              )
          ],
        testGroup
          "Monad"
          [ testCase
              "left identity"
              ( let a = 1
                    m = return a :: Dist Int
                    f = \x -> D [(x + 1, 1)]
                 in (m >>= f) @?= f a
              ),
            testCase
              "right identity"
              ( let m = uniform [1, 2]
                 in (m >>= return) @?= m
              ),
            testCase
              "associativity"
              ( let m = uniform [1, 2]
                    f = \x -> D [(x + 1, 1)]
                    g = \x -> D [(x - 1, 1)]
                 in ((m >>= f) >>= g) @?= (m >>= (\x -> f x >>= g))
              )
          ],
        testCase
          "(>@>)"
          ( let m = uniform [1, 2]
                f = \x -> D [(x + 1, 1)]
                g = \x -> D [(x - 1, 1)]
             in (m >>= (f >@> g)) @?= (m >>= f >>= g)
          ),
        testCase
          "sequ"
          ( let m = uniform [1, 2]
                f = \x -> D [(x + 1, 1)]
                g = \x -> D [(x - 1, 1)]
                h = \x -> D [(x * 2, 1)]
             in (m >>= sequ [f, g, h]) @?= (m >>= f >>= g >>= h)
          ),
        testCase "selectOne" (selectOne [1, 2] @?= D [((1, [2]), 0.5), ((2, [1]), 0.5)]),
        testCase "selectMany 1" (selectMany 0 [1, 2] @?= D [(([], [1, 2]), 1.0)]),
        testCase "selectMany 2" (selectMany 2 [1, 2] @?= D [(([1, 2], []), 0.5), (([2, 1], []), 0.5)]),
        testCase "select" (select 1 [1, 2] @?= D [([1], 0.5), ([2], 0.5)])
      ]
  ]

main :: IO ()
main = defaultMain tests
