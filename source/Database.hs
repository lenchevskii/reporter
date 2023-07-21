{-# LANGUAGE NamedFieldPuns #-}

module Database where

import           Data.Decimal  (realFracToDecimal)
import           Project       (Budget (Budget, budgetExpenditure, budgetIncome),
                                Money (Money), ProjectId,
                                Transaction (Purchase, Sale))
import           System.Random (getStdRandom, randomR)

randomMoney :: (Double, Double) -> IO Money
randomMoney range = Money . realFracToDecimal 2 <$> getStdRandom (randomR range)

getBuget :: ProjectId -> IO Budget
getBuget _ = do
  budgetIncome <- randomMoney (0, 10000)
  budgetExpenditure <- randomMoney (0, 10000)
  pure Budget {budgetIncome, budgetExpenditure}

getTransactions :: ProjectId -> IO [Transaction]
getTransactions _ = do
  sale <- Sale <$> randomMoney (0, 10000)
  purechase <- Purchase <$> randomMoney (0, 10000)
  pure [sale, purechase]
