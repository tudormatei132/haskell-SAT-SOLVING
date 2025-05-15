module TestFormula where

import qualified Data.Map as Map
import qualified Data.Set as Set

import ExtendedFormula
import Formula
import TestPP

testPromote :: TestData
testPromote =
    tests
        1
        6
        [ testVal "promote example"           (Set.fromList [-2, 1, 2])  $ promote formulaLiterals $ toExtendedFormula [[1, 2], [-2]]
        , testVal "promote isPureLiteral"     True                       $ promote (isPureLiteral 3) $ toExtendedFormula [[1, -2, 3], [-1, 2, 3]]
        , testVal "promote non-base"          (toFormula [[1, -2], [3]]) $ promote id nonBaseExtendedFormula
        , testVal "promote literals non-base" (Set.fromList [-2, 1, 3])  $ promote formulaLiterals nonBaseExtendedFormula
        , testVal "promote pure non-base"     [False, True]              $ promote (\form -> map (`isPureLiteral` form) [2, 5]) nonBaseExtendedFormula
        ]
          where nonBaseExtendedFormula = Map.fromList [ (toClause [1,-2], toClause [1,-2,4])
                                                      , (toClause [3], toClause [-5,3])
                                                      ]

testEliminate :: TestData
testEliminate =
    tests
        2
        10
        [ testVal "eliminate example1"   (Map.fromList [(toClause [3], toClause [-1, 3])]) $ eliminate 1 $ toExtendedFormula [[1, 2], [-1, 3]]
        , testVal "eliminate example2"   (Map.fromList [(toClause [2], toClause [1, 2])])  $ eliminate (-1) $ toExtendedFormula [[1, 2], [-1, 3]]
        , testVal "eliminate nothing"    (toExtendedFormula [[1, 2], [-1, 3]])             $ eliminate 4 $ toExtendedFormula [[1, 2], [-1, 3]]
        , testVal "eliminate complement" (Map.fromList [(toClause [1, 2], toClause [1, 2]), (toClause [-1], toClause [-1, 3])])
                                         $ eliminate (-3) $ toExtendedFormula [[1, 2], [-1, 3]]
        , testVal "eliminate literal"    (toExtendedFormula [[1, 2]]) $ eliminate 3 $ toExtendedFormula [[1, 2], [-1, 3]]
        , testVal "eliminate large1"     (Map.fromList [ (toClause [3], toClause [1, 3])
                                                       , (toClause [-5, -4, 2], toClause [-5, -4, 2])
                                                       , (toClause [4], toClause [4])
                                                       , (toClause [-5, 3], toClause [-5, 1, 3])
                                                       , (toClause [-2, 2, 5], toClause [-2, 2, 5])
                                                       ])
                                         $ eliminate (-1) large1
        , testVal "eliminate preserve v"  (Map.fromList [ (toClause [3], toClause [1, 3])
                                                       , (toClause [-5, -4], toClause [-5, -4, 2])
                                                       , (toClause [4], toClause [4])
                                                       , (toClause [-5, 3], toClause [-5, 1, 3])
                                                       ])
                                         $ eliminate (-2)
                                         $ eliminate (-1) large1
        , testVal "eliminate large2"     (Map.fromList [ (toClause [-9, -3, 10], toClause [-9, -6, -3, 8, 10])
                                                       , (toClause [1], toClause [-4, 1])
                                                       ])
                                         $ eliminate 4
                                         $ eliminate (-8)
                                         $ eliminate (-2)
                                         $ eliminate 6
                                         $ toExtendedFormula [[-2], [-9, -6, -3, 8, 10], [-4, 1], [-2, 1, 4, 5, 7], [6]]
        ]
          where large1 = toExtendedFormula [[1, 3], [-5, -4, 2], [4], [-5, 1, 3], [-2, 2, 5]]

testFirstPureLiteral :: TestData
testFirstPureLiteral =
    tests
        3
        12
        [
            testVal "firstPureLiteral - empty formula" Nothing $ firstPureLiteral $ toExtendedFormula [],
            testVal "firstPureLiteral - empty clause" Nothing $ firstPureLiteral $ toExtendedFormula [[]],
            testVal "firstPureLiteral - clause - pure literal" (Just 1) $ firstPureLiteral $ toExtendedFormula [[1]],
            testVal "firstPureLiteral - clauses - pure literal" (Just 3) $ firstPureLiteral $ toExtendedFormula [[1, -1], [2, -2], [3, 4], [-4, 3]],
            testVal "firstPureLiteral - clause - pure literals" (Just (-4)) $ firstPureLiteral $ toExtendedFormula [[-1, 2, 3, -4]],
            testVal "firstPureLiteral - clauses - pure literals" (Just 1) $ firstPureLiteral $ toExtendedFormula [[x, x ^ 2] | x <- [1..10]],
            testVal "firstPureLiteral - clause - non-pure literal" Nothing $ firstPureLiteral $ toExtendedFormula [[1, -1]],
            testVal "firstPureLiteral - clauses - non-pure literal" Nothing $ firstPureLiteral $ toExtendedFormula [[1, -1], [2, -2], [3, 4], [-4, -3]],
            testVal "firstPureLiteral - clause - non-pure literals" Nothing $ firstPureLiteral $ toExtendedFormula [[-1, -2, -3, -4, 4, 3, 2, 1]],
            testVal "firstPureLiteral - clauses - non-pure literals" Nothing $ firstPureLiteral $ toExtendedFormula $ [[x] | x <- [1..10]] ++ [[-x] | x <- [1..10]]
        ]

testFirstUnitClause :: TestData
testFirstUnitClause =
    tests
        4
        12
        [
            testVal "firstUnitClause example 1" Nothing $ firstUnitClause $ toExtendedFormula [[1, 2], [1, 3]],
            testVal "firstUnitClause example 2" (Just (1, toClause [1])) $ firstUnitClause $ toExtendedFormula [[1], [-1, 2]],
            testVal "firstUnitClause example 3" (Just (2, toClause [-1, 2]))$ firstUnitClause $ Map.fromList [(toClause [2], toClause [-1, 2])],
            testVal "firstUnitClause empty formula" Nothing $ firstUnitClause $ toExtendedFormula [],
            testVal "firstUnitClause formula with empty clause" Nothing $ firstUnitClause $ toExtendedFormula [[]],
            testVal "firstUnitClause multiple unit clauses" (Just (1, toClause [1])) $ firstUnitClause $ toExtendedFormula [[2], [1], [3]],
            testVal "firstUnitClause literal and complement" (Just (-1, toClause [-1])) $ firstUnitClause $ toExtendedFormula [[1], [-1]],
            testVal "firstUnitClause medium test" (Just (3, toClause [1, 2, 3])) $ firstUnitClause $ Map.fromList [(toClause [3], toClause [1, 2, 3])],
            testVal "firstUnitClause eliminated unit clause" Nothing $ firstUnitClause $ Map.fromList [(toClause [-3, 2], toClause [-3, -1, 2])],
            testVal "firstUnitClause hard test" (Just (1, toClause [-2,1,5])) $ firstUnitClause $ Map.fromList [(toClause [1], toClause [-2, 1, 5])]
        ]

testDecide :: TestData
testDecide =
  tests
    5
    20
    [ testVal
        "Decide NOP"
        [ (Decide (-1), Map.fromList [(Set.fromList [2], Set.fromList [2])]),
          (NOP, Map.fromList [(Set.fromList [-1, 1], Set.fromList [-1, 1]), (Set.fromList [2], Set.fromList [2])])
        ]
        (decide [(NOP, toExtendedFormula [[-1, 1], [2]])]),
      testVal
        "Decide Unit"
        [ (Decide (-2), Map.fromList [(Set.fromList [3], Set.fromList [3])]),
          (Unit 1 (Set.fromList [1, 4]), Map.fromList [(Set.fromList [-2], Set.fromList [-2]), (Set.fromList [3], Set.fromList [3])])
        ]
        (decide [(Unit 1 (Set.fromList [1, 4]), toExtendedFormula [[-2], [3]])]),
      testVal
        "Decide Pure"
        [ (Decide 2, Map.fromList [(Set.fromList [3, 4], Set.fromList [3, 4])]),
          (Pure (-1), Map.fromList [(Set.fromList [2], Set.fromList [2]), (Set.fromList [3, 4], Set.fromList [3, 4])])
        ]
        (decide [(Pure (-1), toExtendedFormula [[2], [3, 4]])]),
      testVal
        "Decide after decide"
        [ (Decide (-5), Map.empty),
          (Decide (-3), Map.fromList [(Set.fromList [-5, 1, 4], Set.fromList [-5, 1, 4]), (Set.fromList [-5, 6], Set.fromList [-5, 6])])
        ]
        (decide [(Decide (-3), toExtendedFormula [[4, -5, 1], [-5, 6]])]),
      testVal
        "Decide empty formula after action"
        [(Pure 7, Map.empty)]
        (decide [(Pure 7, Map.empty)]),
      testVal
        "Decide complex history"
        [ (Decide 1, Map.fromList [(Set.fromList [5], Set.fromList [5])]),
          (Unit 3 (Set.fromList [3, 8]), Map.fromList [(Set.fromList [1, 3], Set.fromList [1, 5, 6]), (Set.fromList [5], Set.fromList [5])]),
          (Pure 4, Map.fromList [(Set.fromList [1, -1], Set.fromList [-1, 1, 2]), (Set.fromList [3], Set.fromList [3, 8]), (Set.fromList [5], Set.fromList [5])])
        ]
        ( decide
            [ (Unit 3 (Set.fromList [3, 8]), Map.fromList [(Set.fromList [1, 3], Set.fromList [1, 5, 6]), (Set.fromList [5], Set.fromList [5])]),
              (Pure 4, Map.fromList [(Set.fromList [1, -1], Set.fromList [1, -1, 2]), (Set.fromList [3], Set.fromList [3, 8]), (Set.fromList [5], Set.fromList [5])])
            ]
        ),
      testVal
        "Decide complementary literals"
        [ (Decide (-3), Map.fromList [(Set.fromList [2], Set.fromList [2, 3])]),
          (Unit 1 (Set.fromList [1, 4]), Map.fromList [(Set.fromList [-3, -2, 1], Set.fromList [-3, -2, 1]), (Set.fromList [2, 3], Set.fromList [2, 3])])
        ]
        (decide [(Unit 1 (Set.fromList [1, 4]), toExtendedFormula [[-2, 1, -3], [2, 3]])]),
      testVal
        "Decide pure one literal"
        [ (Decide 5, Map.empty),
          (Pure (-3), Map.fromList [(Set.fromList [5], Set.fromList [5])])
        ]
        (decide [(Pure (-3), toExtendedFormula [[5]])]),
      testVal
        "Decide unit clauses"
        [ (Decide (-4), Map.fromList [(Set.fromList [1], Set.fromList [1]), (Set.fromList [2, 3], Set.fromList [2, 3, 4])]),
          (Pure 6, Map.fromList [(Set.fromList [-4], Set.fromList [-4]), (Set.fromList [1], Set.fromList [1]), (Set.fromList [2, 3, 4], Set.fromList [2, 3, 4])])
        ]
        (decide [(Pure 6, toExtendedFormula [[-4], [1], [4, 2, 3]])]),
      testVal
        "Decide contradictory unit clauses"
        [(Decide (-2), Map.fromList [(Set.empty, Set.fromList [2]), (Set.fromList [-1], Set.fromList [-1])]), (Unit 3 (Set.fromList [3, 4]), Map.fromList [(Set.fromList [-2], Set.fromList [-2]), (Set.fromList [-1], Set.fromList [-1]), (Set.fromList [2], Set.fromList [2])])]
        (decide [(Unit 3 (Set.fromList [3, 4]), toExtendedFormula [[-1], [2], [-2]])])
    ]

testProcessPureLiterals :: TestData
testProcessPureLiterals =
    tests
        6
        20
        [
            testVal "processPureLiterals - empty formula" expectedHistory1 $ processPureLiterals initialHistory1,
            testVal "processPureLiterals - empty clause" expectedHistory2 $ processPureLiterals initialHistory2,
            testVal "processPureLiterals - one pure literal - clause to be eliminated" expectedHistory3 $ processPureLiterals initialHistory3,
            testVal "processPureLiterals - one pure literal - clauses to be eliminated" expectedHistory4 $ processPureLiterals initialHistory4,
            testVal "processPureLiterals - multiple pure literals - multiple clauses to be eliminated" expectedHistory5 $ processPureLiterals initialHistory5,
            testVal "processPureLiterals - all pure literals - all clauses to be eliminated" expectedHistory6 $ processPureLiterals initialHistory6,
            testVal "processPureLiterals - one pure literal - multiple stages" expectedHistory7 $ processPureLiterals initialHistory7,
            testVal "processPureLiterals - one pure literal - all stages" expectedHistory8 $ processPureLiterals initialHistory8,
            testVal "processPureLiterals - multiple pure literals - all stages" expectedHistory9 $ processPureLiterals initialHistory9
        ]
        where
            initialHistory1 = [(NOP, toExtendedFormula [])]
            expectedHistory1 = [(NOP, toExtendedFormula [])]

            initialHistory2 = [(NOP, toExtendedFormula [[]])]
            expectedHistory2 = [(NOP, toExtendedFormula [[]])]
    
            initialHistory3 = [(NOP, toExtendedFormula [[1, -1, -4], [-4, 3], [2, -2, 4]])]
            expectedHistory3 = [
                (Pure { getLiteral = 3 }, toExtendedFormula [[1, -1, -4], [2, -2, 4]]),
                (NOP, toExtendedFormula [[1, -1, -4], [-4, 3], [2, -2, 4]])
                ]

            initialHistory4 = [(NOP, toExtendedFormula [[1, -1, -4], [-4, 3], [2, -2, 4], [5, -5, 3]])]
            expectedHistory4 = [
                (Pure { getLiteral = 3 }, toExtendedFormula [[1, -1, -4], [2, -2, 4]]),
                (NOP, toExtendedFormula [[1, -1, -4], [-4, 3], [2, -2, 4], [5, -5, 3]])
                ]

            initialHistory5 = [(NOP, toExtendedFormula [[1, -1, -4, -6], [-4, 3], [2, -2, 4], [5, -5, 3], [-6], [-4, -1, 1]])]
            expectedHistory5 = [
                (Pure { getLiteral = 3 }, toExtendedFormula [[2, -2, 4], [-4, -1, 1]]),
                (Pure { getLiteral = -6 }, toExtendedFormula [[-4, 3], [2, -2, 4], [5, -5, 3], [-4, -1, 1]]),
                (NOP, toExtendedFormula [[1, -1, -4, -6], [-4, 3], [2, -2, 4], [5, -5, 3], [-6], [-4, -1, 1]])
                ]

            initialHistory6 = [(NOP, toExtendedFormula [[x, x ^ 2] | x <- [1..maxLimit]])]
            expectedHistory6 = [
                (Pure { getLiteral = y }, toExtendedFormula [[x, x ^ 2] | x <- [(y + 1)..maxLimit]]) | y <- [maxLimit, (maxLimit - 1)..1]
                ] ++ [(NOP, toExtendedFormula [[x, x ^ 2] | x <- [1..maxLimit]])]

            initialHistory7 = [(NOP, toExtendedFormula [[1, -1, -4], [-4, 3, 4], [2, -2]])]
            expectedHistory7 = [
                (Pure { getLiteral = -4 }, toExtendedFormula [[2, -2]]),
                (Pure { getLiteral = 3 }, toExtendedFormula [[1, -1, -4], [2, -2]]),
                (NOP, toExtendedFormula [[1, -1, -4], [-4, 3, 4], [2, -2]])
                ]

            initialHistory8 = [(NOP, toExtendedFormula [[x, -x, -(x + 1)] | x <- [1..maxLimit]])]
            expectedHistory8 = [
                (Pure { getLiteral = y }, toExtendedFormula [[x, -x, -(x + 1)] | x <- [1..(-(y + 2))]]) | y <- [-2, -3..(-maxLimit - 1)]
                ] ++ [(NOP, toExtendedFormula [[x, -x, -(x + 1)] | x <- [1..maxLimit]])]

            initialHistory9 = [(NOP, toExtendedFormula [[x, -x, -(x + 1), -(x + maxLimit)] | x <- [1..maxLimit]])]
            expectedHistory9 = [
                (Pure { getLiteral = y }, toExtendedFormula [[x, -x, -(x + 1), -(x + maxLimit)] | x <- [1..(-(y + maxLimit + 1))]]) | y <- [(-maxLimit - 1), (-maxLimit - 2)..(-maxLimit) * 2]
                ] ++ [(NOP, toExtendedFormula [[x, -x, -(x + 1), -(x + maxLimit)] | x <- [1..maxLimit]])]

            maxLimit = 100
            

testProcessUnitClauses :: TestData
testProcessUnitClauses =
    tests
        7
        20
        [
            testVal "processUnitClauses example" expectedResult1 $ processUnitClauses baseHistory1,
            testVal "processUnitClauses no unit clauses" baseHistory2 $ processUnitClauses baseHistory2,
            testVal "processUnitClauses finish with empty clause" expectedResult3 $ processUnitClauses baseHistory3,
            testVal "processUnitClauses only unit clauses" expectedResult4 $ processUnitClauses baseHistory4,
            testVal "processUnitClauses decide history" expectedResult5 $ processUnitClauses baseHistory5
        ]
    where
        expectedResult1 = [
            (Unit {getLiteral = -2, getClause = toClause [-2,-1]}, Map.empty),
            (Unit {getLiteral = 1, getClause = toClause [1]}, Map.fromList [(toClause [-2], toClause [-2,-1])]),
            (NOP, toExtendedFormula [[-2, -1], [1]])
            ]
        baseHistory1 = [(NOP, toExtendedFormula [[1], [-2, -1]])]
        expectedResult2 = baseHistory2
        baseHistory2 = [(NOP, toExtendedFormula [[1, 2], [1, -2]])]
        expectedResult3 = [
            (Unit {getLiteral = -2, getClause = toClause [-2,-1]}, Map.fromList [(toClause [], toClause [2])]),
            (Unit {getLiteral = 1, getClause = toClause [1]}, Map.fromList [(toClause [-2], toClause [-2,-1]), (toClause [2], toClause [2])]),
            (NOP, toExtendedFormula [[-2, -1], [1], [2]])
            ]
        baseHistory3 = [(NOP, toExtendedFormula [[-2, -1], [1], [2]])]
        expectedResult4 = [
            (Unit {getLiteral = 3, getClause = toClause [3]}, Map.empty),
            (Unit {getLiteral = 2, getClause = toClause [2]}, Map.fromList [(toClause [3], toClause [3])]),
            (Unit {getLiteral = 1, getClause = toClause [1]}, Map.fromList [(toClause [2], toClause [2]), (toClause [3], toClause [3])]),
            (NOP, Map.fromList [(toClause [1], toClause [1]),(toClause [2], toClause [2]),(toClause [3], toClause [3])])
            ]
        baseHistory4 = [(NOP, toExtendedFormula [[1], [2], [3]])]
        expectedResult5 = [
            (Unit {getLiteral = 1, getClause = toClause [-2,1]}, Map.empty),
            (Decide {getLiteral = 2}, Map.fromList [(toClause [1], toClause [-2, 1])]),
            (NOP, toExtendedFormula [[1, 2], [1, -2], [2, 3]])
            ]
        baseHistory5 = [
            (Decide {getLiteral = 2}, Map.fromList [(toClause [1], toClause [-2, 1])]),
            (NOP, toExtendedFormula [[1, 2], [1, -2], [2, 3]])
            ]

testBacktrackToUnitClause :: TestData
testBacktrackToUnitClause =
  tests
    8
    20
    [ testVal
        "BacktrackToUnitClause through multiple actions"
        [ ( Decide (-4),
            Map.fromList
              [ (Set.fromList [-3, 1, 2], Set.fromList [-3, 1, 2]),
                (Set.fromList [-1], Set.fromList [-1, 4]),
                (Set.fromList [5], Set.fromList [4, 5])
              ]
          ),
          ( NOP,
            toExtendedFormula [[-4, -2, 3], [-3, 1, 2], [-1, 4], [4, 5]]
          )
        ]
        ( backtrackToUnitClause
            (toClause [4, 5])
            [ (Pure (-3), Map.empty),
              (Unit (-1) (Set.fromList [-1, 4]), Map.fromList [(Set.fromList [-3, 2], Set.fromList [-3, 1, 2])]),
              ( Decide (-4),
                Map.fromList
                  [ (Set.fromList [-3, 1, 2], Set.fromList [-3, 1, 2]),
                    (Set.fromList [-1], Set.fromList [-1, 4])
                  ]
              ),
              ( NOP,
                toExtendedFormula [[-4, -2, 3], [-3, 1, 2], [-1, 4]]
              )
            ]
        ),
      testVal
        "BacktrackToUnitClause empty history"
        []
        ( backtrackToUnitClause
            (toClause [4, 5])
            []
        ),
      testVal
        "BacktrackToUnitClause already unit clause"
        [ ( NOP,
            Map.fromList
              [ (Set.fromList [-1], Set.fromList [-1]),
                (Set.fromList [2, 3, 4], Set.fromList [2, 3, 4]),
                (Set.fromList [3], Set.fromList [3])
              ]
          )
        ]
        ( backtrackToUnitClause
            (toClause [3])
            [ ( Pure 2,
                Map.empty
              ),
              ( Decide (-1),
                toExtendedFormula [[2, 3, 4]]
              ),
              ( NOP,
                toExtendedFormula [[-1], [2, 3, 4]]
              )
            ]
        ),
      testVal
        "BacktrackToUnitClause empty clause"
        [ ( Decide (-2),
            Map.fromList
              [ (Set.empty, Set.empty),
                (Set.fromList [-1], Set.fromList [2, -1]),
                (Set.fromList [1, 3], Set.fromList [1, 2, 3]),
                (Set.fromList [4, 5, 6], Set.fromList [4, 5, 6])
              ]
          ),
          (NOP, toExtendedFormula [[-2, -1], [-1, 2], [1, 2, 3], [4, 5, 6], []])
        ]
        ( backtrackToUnitClause
            (toClause [2, -1])
            [ ( Pure 4,
                toExtendedFormula [[]]
              ),
              ( Decide 1,
                toExtendedFormula [[], [4, 5, 6]]
              ),
              ( Decide (-2),
                Map.fromList
                  [ (Set.empty, Set.empty),
                    (Set.fromList [1, 3], Set.fromList [1, 2, 3]),
                    (Set.fromList [4, 5, 6], Set.fromList [4, 5, 6])
                  ]
              ),
              ( NOP,
                toExtendedFormula [[-1, -2], [2, 3, 1], [5, 4, 6], []]
              )
            ]
        ),
      testVal
        "BacktrackToUnitClause complex history"
        [ ( Decide (-1),
            Map.fromList
              [ (Set.empty, Set.empty),
                (Set.fromList [-7], Set.fromList [1, 4, 5, -7]),
                (Set.fromList [2, 7], Set.fromList [2, 7]),
                (Set.fromList [7], Set.fromList [1, 4, 7])
              ]
          ),
          ( Decide (-4),
            Map.fromList
              [ (Set.empty, Set.empty),
                (Set.fromList [-1], Set.fromList [-1]),
                (Set.fromList [1, -7], Set.fromList [1, 4, 5, -7]),
                (Set.fromList [1, 7], Set.fromList [1, 4, 7]),
                (Set.fromList [2, 7], Set.fromList [2, 7])
              ]
          ),
          ( Decide (-5),
            Map.fromList
              [ (Set.empty, Set.empty),
                (Set.fromList [-4], Set.fromList [-4, 5]),
                (Set.fromList [-1], Set.fromList [-1]),
                (Set.fromList [1, 4, -7], Set.fromList [1, 4, 5, -7]),
                (Set.fromList [1, 4, 7], Set.fromList [1, 4, 7]),
                (Set.fromList [2, 7], Set.fromList [2, 7])
              ]
          ),
          ( NOP,
            toExtendedFormula [[-5, 3], [-4, 5], [-1], [1, 4, 5, -7], [1, 4, 7], [2, 7], []]
          )
        ]
        ( backtrackToUnitClause
            (toClause [5, 4, 1, -7])
            [ ( Pure 7,
                Map.fromList [(Set.empty, Set.empty)]
              ),
              ( Pure 2,
                Map.fromList
                  [ (Set.empty, Set.empty),
                    (Set.fromList [7], Set.fromList [1, 4, 7])
                  ]
              ),
              ( Decide (-1),
                Map.fromList
                  [ (Set.empty, Set.empty),
                    (Set.fromList [7], Set.fromList [1, 4, 7]),
                    (Set.fromList [2, 7], Set.fromList [2, 7])
                  ]
              ),
              ( Decide (-4),
                Map.fromList
                  [ (Set.empty, Set.empty),
                    (Set.fromList [-1], Set.fromList [-1]),
                    (Set.fromList [1, 7], Set.fromList [1, 4, 7]),
                    (Set.fromList [2, 7], Set.fromList [2, 7])
                  ]
              ),
              ( Decide (-5),
                Map.fromList
                  [ (Set.empty, Set.empty),
                    (Set.fromList [-4], Set.fromList [-4, 5]),
                    (Set.fromList [-1], Set.fromList [-1]),
                    (Set.fromList [1, 4, 7], Set.fromList [1, 4, 7]),
                    (Set.fromList [2, 7], Set.fromList [2, 7])
                  ]
              ),
              ( NOP,
                toExtendedFormula [[-5, 3], [-4, 5], [-1], [1, 4, 7], [2, 7], []]
              )
            ]
        )
    ]

main :: IO ()
main =
    vmCheck
        [ testPromote
        , testEliminate
        , testFirstPureLiteral
        , testFirstUnitClause
        , testDecide
        , testProcessPureLiterals
        , testProcessUnitClauses
        , testBacktrackToUnitClause
        ]
