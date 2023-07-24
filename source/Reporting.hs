{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}

module Reporting where

import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Writer              (MonadWriter (tell), listen,
                                                    runWriterT)
import           Data.Foldable                     (fold)
import           Data.Generics.Fixplate.Attributes (synthetiseM)
import           Data.Generics.Fixplate.Base       (Attr)
import           Data.Monoid                       (getSum)
import qualified Database                          as DB
import           Project                           (Budget (budgetExpenditure, budgetIncome),
                                                    Money, Project,
                                                    ProjectF (..),
                                                    Transaction (..))

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

-- calculateProjectReport :: Project -> IO Report
-- calculateProjectReport project =
--   case project of
--     (Project p _) -> calculateReport <$> DB.getBuget p <*> DB.getTransactions p
--     (ProjectGroup _ projects) -> foldMap calculateProjectReport projects
--
type ProjectReport = Attr ProjectF Report

calculateProjectReports :: Project -> IO ProjectReport
calculateProjectReports = synthetiseM calc
  where
    calc (Project p _) =
      calculateReport <$> DB.getBuget p <*> DB.getTransactions p
    calc (ProjectGroup _ reports) = pure (fold reports)
--
-- calculateProjectReports ::
--      Project g ProjectId -> IO (Project Report Report)
-- calculateProjectReports project = fst <$> runWriterT (calc project)
--   where
--     calc (Project name p) = do
--       report <-
--         liftIO (calculateReport <$> DB.getBuget p <*> DB.getTransactions p)
--       tell report
--       pure (Project name report)
--     calc (ProjectGroup name _ projects) = do
--       (projects', report) <- listen (mapM calc projects)
--       pure (ProjectGroup name report projects')
--
-- There is no need for the `accumulateProjectReport` function,
-- because `calculateProjectReport` returns Reports on all levels.
