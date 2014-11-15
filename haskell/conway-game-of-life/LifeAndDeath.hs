module LifeAndDeath where

data Cell = DeadCell | LivingCell deriving (Eq, Show)

nextStateOf _ _ = DeadCell
