module Main where

import           Demo        (_project)
import           PrettyPrint (prettyResult)
import           Reporting   (calculateProjectReports)

-- main :: IO ()
-- main = do
--   report <- calculateProjectReport project
--   putStrLn (prettyReport report)
--
-- | FIXME: add drawTreeWith fnc, fix reports types error
main :: IO ()
main = do
  reports <- calculateProjectReports _project
  drawTreeWith (prettyResult reports)
--
-- $> main
