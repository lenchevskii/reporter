{-# LANGUAGE InstanceSigs #-}

module Reporting where

import           Data.Monoid (getSum)
import qualified Database    as DB
import           Project     (Budget (budgetExpenditure, budgetIncome), Money,
                              Project (Project, ProjectGroup),
                              Transaction (Purchase, Sale))

data Report =
  Report
    { budgetProfit :: Money
    , netProfit    :: Money
    , difference   :: Money
    }
  deriving (Show, Eq)

instance Semigroup Report where
  (<>) :: Report -> Report -> Report
  (<>) (Report b1 n1 d1) (Report b2 n2 d2) =
    Report (b1 + b2) (n1 + n2) (d1 + d2)

instance Monoid Report where
  mempty :: Report
  mempty = Report 0 0 0

calculateReport :: Budget -> [Transaction] -> Report
calculateReport budget transactions =
  Report
    { budgetProfit = budgetProfit'
    , netProfit = netProfit'
    , difference = netProfit' - budgetProfit'
    }
  where
    budgetProfit' = budgetIncome budget - budgetExpenditure budget
    netProfit' = getSum (foldMap asProfit transactions)
    asProfit (Sale m)     = pure m
    asProfit (Purchase m) = pure (negate m)

calculateProjectReport :: Project -> IO Report
calculateProjectReport project =
  case project of
    (Project p _) -> calculateReport <$> DB.getBuget p <*> DB.getTransactions p
    (ProjectGroup _ projects) -> foldMap calculateProjectReport projects
