module Main
    ( main
    ) where

import qualified Spec.ModelClosable
import qualified Spec.TraceClosable
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "homework token sale"
    [ Spec.TraceClosable.tests
    , Spec.ModelClosable.tests
    ]
