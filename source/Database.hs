{-# LANGUAGE NamedFieldPuns #-}

module Database where

import           Project       (Budget (Budget, budgetExpenditure, budgetIncome),
                                Money (Money), ProjectId,
                                Transaction (Purchase, Sale))
import           System.Random (getStdRandom, randomR)

getBuget :: ProjectId -> IO Budget
getBuget _ = do
  budgetIncome <- Money <$> getStdRandom (randomR (0, 10000))
  budgetExpenditure <- Money <$> getStdRandom (randomR (0, 10000))
  pure Budget {budgetIncome, budgetExpenditure}

getTransactions :: ProjectId -> IO [Transaction]
getTransactions _ = do
  sale <- Sale . Money <$> getStdRandom (randomR (0, 10000))
  purechase <- Purchase . Money <$> getStdRandom (randomR (0, 10000))
  pure [sale, purechase]
