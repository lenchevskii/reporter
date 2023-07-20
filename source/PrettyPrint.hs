module PrettyPrint where

import           Data.Text   (unpack)
import           Data.Tree   (Tree (Node), drawTree)
import qualified Project     as P
import qualified Reporting   as R
import           Text.Printf (printf)

asTree :: P.Project -> Tree String
asTree project =
  case project of
    P.Project (P.ProjectId p) name -> Node (printf "%s (%d)" name p) []
    P.ProjectGroup name projects   -> Node (unpack name) (map asTree projects)

prettyProject :: P.Project -> String
prettyProject = drawTree . asTree

prettyReport :: R.Report -> String
prettyReport r =
  printf
    "Budget: %.2f, Net: %.2f, Difference: %.2f"
    (P.unMoney (R.budgetProfit r))
    (P.unMoney (R.netProfit r))
    (P.unMoney (R.difference r))
