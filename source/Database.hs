module Database where

import           Project       (Budget (Budget, budgetExpenditure, budgetIncome),
                                Money (Money), ProjectId,
                                Transaction (Purchase, Sale))
import           System.Random (getStdRandom, randomR)

getBuget :: ProjectId -> IO Budget
getBuget _ = do
  income <- Money <$> getStdRandom (randomR (0, 10000))
  expenditure <- Money <$> getStdRandom (randomR (0, 10000))
  pure Budget {budgetIncome = income, budgetExpenditure = expenditure}

getTransactions :: ProjectId -> IO [Transaction]
getTransactions _ = do
  sale <- Sale . Money <$> getStdRandom (randomR (0, 10000))
  purechase <- Purchase . Money <$> getStdRandom (randomR (0, 10000))
  pure [sale, purechase]
