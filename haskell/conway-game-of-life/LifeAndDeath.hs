module LifeAndDeath where

data Cell = DeadCell | LivingCell deriving (Eq, Show)

nextStateOf LivingCell 2 = LivingCell
nextStateOf _ 3 = LivingCell
nextStateOf _ _ = DeadCell
