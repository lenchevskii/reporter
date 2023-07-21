{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}

module Reporting where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Writer   (MonadWriter (tell), listen, runWriterT)
import           Data.Foldable          (fold)
import           Data.Monoid            (getSum)
import qualified Database               as DB
import qualified Project                as P

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
calculateProjectReports ::
     P.Project g P.ProjectId -> IO (P.Project Report Report)
calculateProjectReports project = fst <$> runWriterT (calc project)
  where
    calc (P.Project name p) = do
      report <-
        liftIO (calculateReport <$> DB.getBuget p <*> DB.getTransactions p)
      tell report
      pure (P.Project name report)
    calc (P.ProjectGroup name _ projects) = do
      (projects', report) <- listen (mapM calc projects)
      pure (P.ProjectGroup name report projects')
--
-- There is no need for the `accumulateProjectReport` function,
-- because `calculateProjectReport` returns Reports on all levels.
