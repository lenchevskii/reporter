{-# LANGUAGE InstanceSigs #-}

module Reporting where

import           Data.Foldable (fold)
import           Data.Monoid   (getSum)
import qualified Database      as DB
import qualified Project       as P

data Report =
  Report
    { budgetProfit :: P.Money
    , netProfit    :: P.Money
    , difference   :: P.Money
    }
  deriving (Show, Eq)

instance Semigroup Report where
  (<>) :: Report -> Report -> Report
  (<>) (Report b1 n1 d1) (Report b2 n2 d2) =
    Report (b1 + b2) (n1 + n2) (d1 + d2)

instance Monoid Report where
  mempty :: Report
  mempty = Report 0 0 0

calculateReport :: P.Budget -> [P.Transaction] -> Report
calculateReport budget transactions =
  Report
    { budgetProfit = budgetProfit'
    , netProfit = netProfit'
    , difference = netProfit' - budgetProfit'
    }
  where
    budgetProfit' = P.budgetIncome budget - P.budgetExpenditure budget
    netProfit' = getSum (foldMap asProfit transactions)
    asProfit (P.Sale m)     = pure m
    asProfit (P.Purchase m) = pure (negate m)

-- calculateProjectReport :: Project -> IO Report
-- calculateProjectReport project =
--   case project of
--     (Project p _) -> calculateReport <$> DB.getBuget p <*> DB.getTransactions p
--     (ProjectGroup _ projects) -> foldMap calculateProjectReport projects
--
calculateProjectReport :: P.Project P.ProjectId -> IO (P.Project Report)
calculateProjectReport =
  traverse (\p -> calculateReport <$> DB.getBuget p <*> DB.getTransactions p)

accumulateProjectReport :: P.Project Report -> Report
accumulateProjectReport = fold
