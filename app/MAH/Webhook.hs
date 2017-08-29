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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MAH.Webhook where

import Data.Aeson
import Data.Aeson.TH
import Data.Void
import qualified Data.Text as T

import MAH.Common

data Webhook = Webhook
  { webhook_object :: T.Text
  , webhook_entry :: [Entry]
  } deriving Show

data Entry = Entry
  { entry_id :: T.Text
  , entry_time :: Integer
  , entry_messaging :: [Messaging]
  } deriving Show

data Messaging =
  MessagingMessage Sender Recipient Integer Message
  deriving Show

$(deriveJSON defaultOptions{fieldLabelModifier = drop 8, omitNothingFields = True} ''Webhook)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 6, omitNothingFields = True} ''Entry)

instance ToJSON Messaging where
  toJSON (MessagingMessage sender recipient timestamp message) =
    object [ "sender" .= sender
           , "recipient" .= recipient
           , "timestamp" .= timestamp
           , "message" .= message
           ]

instance FromJSON Messaging where
  parseJSON =
    withObject "message" $ \o ->
      MessagingMessage
      <$> o .: "sender"
      <*> o .: "recipient"
      <*> o .: "timestamp"
      <*> o .: "message"

