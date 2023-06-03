module Test.Tasty.DocumentationTest where

import Test.Tasty
import Test.Tasty.Providers
import GHC.Stack.Types

import Test.Tasty.DocumentationTest.Internal

-- reportAsTable :: TestTree -> TestTree
-- reportAsTable test = if anyTable tree then go tree else tree
--   where
--     go TestTree -> TestTree
--     go (PlusTestOptions f t) = case lookupOption (f empty) of
--         TableRender True -> TestGroup 

  -- TestGroup name tree -> testGroup name [PlusTestOptions (setOption (TableRender True) tree)]

report :: TestName -> DocumentationTest -> TestTree
report name test = singleTest name test

prependLocation :: Maybe SrcLoc -> String -> String
prependLocation mbloc s =
  case mbloc of
    Nothing -> s
    Just loc -> srcLocFile loc ++ ":" ++ show (srcLocStartLine loc) ++ ":\n" ++ s

-- anyTable :: TestTree -> Bool
-- anyTable = getAny . foldTestTree tfold mempty
--   where
--     tfold = trivialFold {foldSingle = \opts _ _ -> Any (focusedOpts opts)}
--     focusedOpts opts = case lookupOption opts of
--       TableRender -> b
