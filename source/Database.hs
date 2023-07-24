{-# LANGUAGE NamedFieldPuns #-}

module Database where

import           Data.Decimal  (realFracToDecimal)
import qualified Project       as P
import           System.Random (getStdRandom, randomR)

randomMoney :: (Double, Double) -> IO P.Money
randomMoney range =
  P.Money . realFracToDecimal 2 <$> getStdRandom (randomR range)

getBuget :: P.ProjectId -> IO P.Budget
getBuget _ = do
  budgetIncome <- randomMoney (0, 10000)
  budgetExpenditure <- randomMoney (0, 10000)
  pure P.Budget {P.budgetIncome, P.budgetExpenditure}

getTransactions :: P.ProjectId -> IO [P.Transaction]
getTransactions _ = do
  sale <- P.Sale <$> randomMoney (0, 10000)
  purechase <- P.Purchase <$> randomMoney (0, 10000)
  pure [sale, purechase]
