module Test.Suites
  ( testRubyDsl
  ) where

import System.IO         (openFile, IOMode(ReadMode, WriteMode), hGetContents, hClose, hPutStr, )
import Control.Exception (evaluate)
import Control.DeepSeq   (rnf)
import System.Directory  (removeFile)
import Data.Functor ((<&>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Ruby.Formatter
import Ruby.Reader
import Ruby.Interpreter
import Ruby.Dsl

data Suite = Suite
  { code :: FilePath
  , formatted :: FilePath
  , output :: FilePath
  , exprIndex :: Int
  }

getCode :: Suite -> IO String
getCode = readFile . code

actualExpr :: Suite -> IO String
actualExpr = actualFormatted

expectExpr :: Suite -> IO String
expectExpr = return . format_ . (expressions !!) . exprIndex

expectFormatted :: Suite -> IO String
expectFormatted = readFile . formatted

expectOutput :: Suite -> IO String
expectOutput = readFile . output

actualFormatted :: Suite -> IO String
actualFormatted = (format_ . readRuby <$>) . getCode

actualOutput :: Suite -> IO String
actualOutput s = do
  let tmpFile = output s ++ ".tmp"
  file <- openFile tmpFile WriteMode
  () <- hPutStr file ""
  hClose file
  expr <- readRuby <$> getCode s
  () <- interpret expr "" tmpFile
  file <- openFile tmpFile ReadMode
  r <- hGetContents file
  evaluate (rnf r)
  hClose file
  removeFile tmpFile
  return r

expressions :: Ruby expr => [expr ()]
expressions =
  [ file_
      [ def_ "max" ["x", "y"]
          [ if_ (gt_ (read_ "x") (read_ "y"))
              [ return_ $ read_ "x" ]
            $ Just
              [ return_ $ read_ "y"]
          ]
      , assoc_ "x" $ int_ 1
      , assoc_ "y" $ int_ 2
      , onTop_ $ call_ "puts" [ call_ "max" [ read_ "x", read_ "y" ] ]
      ]
  , file_
      [ onTop_ $ call_ "puts"
          [ plus_
              (plus_
                (int_ 1)
                (div_
                  (mult_
                    (int_ 2)
                    (int_ 3)
                  )
                  (int_ 2)
                )
              )
              (int_ 1)
          ]
      ]
  , file_
      [ for_ "i" (int_ 1, int_ 10)
          [ onTop_ $ call_ "puts" [ read_ "i" ]
          , onTop_ $ call_ "puts" [ string_ "!!" ]
          ]
      ]
  , file_
      [ assoc_ "a" $ string_ "1"
      , assoc_ "b" $ string_ "2"
      , onTop_ $ call_ "puts" [ plus_ (read_ "a") (read_ "b") ]
      ]
  , file_
      [ assoc_ "x" $ int_ 1
      , assoc_ "y" $ float_ 0.1
      , onTop_ $ call_ "puts" [ plus_ (read_ "x") (read_ "y") ]
      ]
  , file_
      [ if_ (bool_ True)
          [ onTop_ $ call_ "puts" [ int_ 1 ] ]
        Nothing
      ]
  ]

suites :: [Suite]
suites = ([1..6] :: [Int]) <&>
  \i -> Suite
    { code = "./res/tests/code/test_good_" ++ show i ++ ".rb"
    , formatted = "./res/tests/formatted/test_good_" ++ show i ++ ".rb"
    , output = "./res/tests/result/test_good_" ++ show i ++ ".out"
    , exprIndex = pred i
    }

testRubyDsl :: TestTree
testRubyDsl = testGroup "Testing Ruby DSL"
  [ testGroup "Testing parsing" $
      suites <&> buildTestCase actualExpr expectExpr
  , testGroup "Testing formatting" $
      suites <&> buildTestCase actualFormatted expectFormatted
  , testGroup "Testing interpreting" $
      suites <&> buildTestCase actualOutput expectOutput
  ]
  where
    buildTestCase
      :: (Suite -> IO String)
      -> (Suite -> IO String)
      -> Suite -> TestTree
    buildTestCase actualIO expectedIO suite =
      testCase (code suite) $ do
        actual <- actualIO suite
        expected <- expectedIO suite
        actual @?= expected