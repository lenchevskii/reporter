module PrettyPrint where

import           Data.Bool    (bool)
import           Data.Decimal (roundTo)
import           Data.Text    (unpack)
import           Data.Tree    (Tree (Node), drawTree)
import           Project      (Money)
import qualified Project      as P
import qualified Reporting    as R
import           Text.Printf  (printf)

-- asTree :: P.Project -> Tree String
-- asTree project =
--   case project of
--     P.Project (P.ProjectId p) name -> Node (printf "%s (%d)" name p) []
--     P.ProjectGroup name projects   -> Node (unpack name) (map asTree projects)
--
asTree :: (g -> String) -> (a -> String) -> P.Project g a -> Tree String
asTree prettyGroup prettyValue project =
  case project of
    P.Project name x -> Node (printf "%s: %s" name (prettyValue x)) []
    P.ProjectGroup name x projects ->
      Node
        (printf "%s: %s" name (prettyGroup x))
        (map (asTree prettyGroup prettyValue) projects)

prettyProject :: (g -> String) -> (a -> String) -> P.Project g a -> String
prettyProject prettyGroup prettyValue =
  drawTree . asTree prettyGroup prettyValue

prettyMoney :: Money -> String
prettyMoney (P.Money d) = bool "" "+" (d > 0) ++ show (roundTo 2 d)

prettyReport :: R.Report -> String
prettyReport r =
  printf
    "Budget: %s, Net: %s, Difference: %s"
    (prettyMoney (R.budgetProfit r))
    (prettyMoney (R.netProfit r))
    (prettyMoney (R.difference r))
