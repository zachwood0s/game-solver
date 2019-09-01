import Test.Tasty
import Test.Tasty.HUnit
import TicTacToe (emptyBoard, BoardSize(..), Board(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [boardTests]

boardTests = testGroup "Board tests"
  [ testCase "Creates an empty board correctly" $
    emptyBoard (BoardSize 2 3) @?= Board [[ Nothing, Nothing, Nothing ], [ Nothing, Nothing, Nothing ]] 
  ]
