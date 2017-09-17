-- | This file is part of HAH
-- |
-- | wakemeeup is free software: you can redistribute it and/or modify
-- | it under the terms of the GNU General Public License as published by
-- | the Free Software Foundation, either version 3 of the License, or
-- | (at your option) any later version.
-- |
-- | HAH is distributed in the hope that it will be useful,
-- | but WITHOUT ANY WARRANTY; without even the implied warranty of
-- | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- | GNU General Public License for more details.
-- |
-- | You should have received a copy of the GNU General Public License
-- | along with HAH. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MAH.Send where

import Data.Aeson
import Data.Aeson.TH
import Data.Void
import qualified Data.Text as T

import MAH.Common

data Send = Send
  { send_recipient :: Recipient
  , send_message :: Maybe Message
  , send_sender_action :: Maybe T.Text
  , send_notification_type :: Maybe T.Text
  , send_tag :: Maybe T.Text
  } deriving Show

$(deriveJSON defaultOptions{fieldLabelModifier = drop 5, omitNothingFields = True} ''Send)
