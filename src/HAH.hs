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
import Data.Vector.Sized
import Control.Lens hiding (cons)
import Control.Monad.State.Lazy
import Control.Arrow
import Data.Singletons.TypeLits
import Data.Finite
import Data.Dependent.Sum
import Prelude hiding (cons)

data Black (n :: Nat) = Black String 
type AnyBlack = DSum Sing Black

data Player = Player { hand :: [String], points :: Int }

data HAH = forall n.
  HAH { _black :: [AnyBlack]
      , _white :: [String]
      , _players :: Vector n Player
      , _czar :: Finite n
      }

withGame :: Monad m => [AnyBlack] -> [String] -> StateT HAH m a -> m a
withGame black white game = evalStateT game state
  where
    state = HAH
      black
      white
      (singleton $ Player [] 0)
      (fromJust . packFinite $ 0)

-- addPlayer :: MonadState HAH m => m ()
-- addPlayer = modify $ \(HAH black white players czar) ->
--   HAH black white (Player [] 0 `cons` players) (weaken czar)

