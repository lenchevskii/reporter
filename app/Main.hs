module Main where

import           Demo        (project)
import           PrettyPrint (prettyProject, prettyReport)
import           Reporting   (Report, calculateProjectReport)

main :: IO ()
main = do 
  report <- calculateProjectReport project
  putStrLn (prettyReport report)
--
-- $> main
