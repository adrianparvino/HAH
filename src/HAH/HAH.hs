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

module HAH.HAH where

import CLaSH.Sized.Vector
import Control.Lens hiding ((:>))
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
