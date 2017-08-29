{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

module HAH (
           ) where

import Data.Maybe
import Data.Finite
import CLaSH.Sized.Vector
import Control.Lens hiding ((:>))
import Control.Monad.State.Lazy
import Control.Arrow
import Data.Singletons.TypeLits
import Data.Finite

data Black n = Black String 
data AnyBlack = forall (n :: Nat). AnyBlack (Black n) (SNat n)

data Player = Player { hand :: [String], points :: Int }

data HAH = forall v n.
  HAH { _black :: [AnyBlack]
      , _white :: [String]
      , _players :: Vec n Player
      , _czar :: Finite n
      }

withGame :: Monad m => [AnyBlack] -> [String] -> StateT HAH m a -> m a
withGame black white game = evalStateT game (HAH black white (singleton $ Player [] 0) (fromJust . packFinite $ 0))
-- makeGame :: [AnyBlack] -> [String] -> HAH
-- makeGame black white = HAH black white (singleton $ Player [] 0) (fromJust . packFinite $ 0)

addPlayer :: MonadState HAH m => m ()
addPlayer = modify $
  \(HAH black white players czar) -> HAH black white (Player [] 0 :> players) (weaken czar)

