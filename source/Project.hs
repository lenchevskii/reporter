{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Project where

import           Data.Decimal                (Decimal)
import           Data.Generics.Fixplate.Base (Mu (Fix))
import           Data.Text                   (Text)

newtype Money =
  Money
    { unMoney :: Decimal
    }
  deriving (Show, Eq, Num)

newtype ProjectId =
  ProjectId
    { unProjectId :: Int
    }
  deriving (Show, Eq, Num)

-- | The field `g` is a slot for project group level reports.
-- data Project g a
--   = Project Text a
--   | ProjectGroup Text g [Project g a]
--   deriving (Show, Eq, Functor, Foldable, Traversable)
--
data ProjectF f
  = Project ProjectId Text
  | ProjectGroup Text [f]
  deriving (Show, Eq, Functor, Foldable, Traversable)

type Project = Mu ProjectF

project :: ProjectId -> Text -> Project
project p = Fix . Project p

projectGroup :: Text -> [Project] -> Project
projectGroup name = Fix . ProjectGroup name

data Budget =
  Budget
    { budgetIncome      :: Money
    , budgetExpenditure :: Money
    }
  deriving (Show, Eq)

data Transaction
  = Sale Money
  | Purchase Money
  deriving (Show, Eq)
