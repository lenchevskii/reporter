module Main where

import           Demo        (project)
import           PrettyPrint (prettyProject, prettyReport)
import qualified Reporting   as R

-- main :: IO ()
-- main = do
--   report <- calculateProjectReport project
--   putStrLn (prettyReport report)
--
main :: IO ()
main = do
  report <- R.calculateProjectReports project
  putStrLn (prettyProject prettyReport prettyReport report)
--
-- $> main
