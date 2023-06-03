{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Test.Tasty.DocumentationTest.Internal where

import Data.Data (Typeable, Proxy(Proxy))
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (OsPath, osp)
import System.OsPath qualified as OsPath
import Prettyprinter
import Text.Read (readMaybe)

import Test.Tasty.HUnit
import Test.Tasty.Options
import Test.Tasty.Providers

data DocumentationTest = DocumentationTest
  { runTest :: IO ResultData
  , format :: ReportFormat
  }

newtype ResultData = ResultData { resultData :: Map Text Text}
  deriving newtype (Show, Eq, Ord, Typeable)

newtype ReportFile = ReportFile OsPath
  deriving newtype (Show, Eq, Ord, Typeable)

instance IsOption ReportFile where
  defaultValue = ReportFile [osp| "./test/report.adoc" |]
  parseValue = parseReportPath
  optionName = pure "report-file"
  optionHelp = pure "The file name in which the report will be written"

data ReportFormat = TableRender
  deriving stock (Show, Eq, Ord, Typeable)

instance IsOption ReportFormat where
  defaultValue = TableRender
  parseValue = readMaybe
  optionName = pure "table render"
  optionHelp = pure ""

parseReportPath :: FilePath -> Maybe ReportFile
parseReportPath param = Just $ ReportFile $ unsafePerformIO $ OsPath.encodeFS param

instance IsTest DocumentationTest where
  run = runTest
  testOptions = pure
    [ Option (Proxy :: Proxy ReportFile) ]

runActualTest :: OptionSet -> DocumentationTest -> (Progress -> IO ()) -> IO Result
runActualTest options (DocumentationTest testFunction format) = const $ do
  let ReportFile filePath = lookupOption options
  result <- try testFunction 
  case result of 
    Left (HUnitFailure mbloc message) -> pure $ testFailed $ prependLocation mbloc message
    Right (ResultData resultData) -> do
      render format resultData filePath

render :: ReportFormat -> ResultData -> OsPath -> IO ()
render TableRender = do
  
