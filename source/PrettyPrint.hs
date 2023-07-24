module PrettyPrint where

import           Data.Bool                   (bool)
import           Data.Decimal                (roundTo)
import           Data.Generics.Fixplate.Base (Ann (Ann))
import           Data.Tree                   (Tree (Node), drawTree)
import           Project                     (Money (Money), ProjectF (..),
                                              ProjectId (ProjectId))
import           Reporting                   (Report (budgetProfit, difference, netProfit))
import           Text.Printf                 (printf)

-- asTree :: P.Project -> Tree String
-- asTree project =
--   case project of
--     P.Project (P.ProjectId p) name -> Node (printf "%s (%d)" name p) []
--     P.ProjectGroup name projects   -> Node (unpack name) (map asTree projects)
--
prettyResult :: Ann ProjectF Report a -> String
prettyResult (Ann report project') =
  case project' of
    Project (ProjectId p) name ->
      printf "%s (%d): %s" name p (prettyReport report)
    ProjectGroup name _ -> printf "%s: %s" name (prettyReport report)

-- asTree :: (g -> String) -> (a -> String) -> Project g a -> Tree String
-- asTree prettyGroup prettyValue project =
--   case project of
--     Project name x -> Node (printf "%s: %s" name (prettyValue x)) []
--     ProjectGroup name x projects ->
--       Node
--         (printf "%s: %s" name (prettyGroup x))
--         (map (asTree prettyGroup prettyValue) projects)
-- --
-- prettyProject :: (g -> String) -> (a -> String) -> Project g a -> String
-- prettyProject prettyGroup prettyValue =
--   drawTree . asTree prettyGroup prettyValue
--
prettyMoney :: Money -> String
prettyMoney (Money d) = bool "" "+" (d > 0) ++ show (roundTo 2 d)

prettyReport :: Report -> String
prettyReport r =
  printf
    "Budget: %s, Net: %s, Difference: %s"
    (prettyMoney (budgetProfit r))
    (prettyMoney (netProfit r))
    (prettyMoney (difference r))
