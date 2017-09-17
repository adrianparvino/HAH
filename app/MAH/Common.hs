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

module MAH.Common where

import Data.Aeson
import Data.Aeson.TH
import Data.Void
import qualified Data.Text as T

data QuickReply = QuickReply
  { quickreply_payload :: Payload
  } deriving Show

data Payload =
  Payload
  deriving Show

data Attachment =
  AttachmentFallback (Maybe T.Text) (Maybe T.Text) (Maybe Void)
  deriving Show

$(deriveJSON defaultOptions{fieldLabelModifier = drop 11, omitNothingFields = True} ''QuickReply)

instance ToJSON Attachment where
  toJSON (AttachmentFallback title url payload) =
    object $ [ "title" .= title
             , "url" .= url
             , "payload" .= Null
             , "type" .= ("fallback" :: T.Text)
             ]

instance FromJSON Attachment where
  parseJSON =
    withObject "message" $ \o ->
      AttachmentFallback
      <$> o .: "title"
      <*> o .: "url"
      <*> pure Nothing

data Sender = Sender
  { sender_id :: T.Text
  } deriving Show

data Recipient = Recipient
  { recipient_id :: T.Text
  , recipient_phone_number :: Maybe T.Text
  , recipient_name :: Maybe Name
  } deriving Show

data Message = Message
  { message_mid :: Maybe T.Text
  , message_text :: Maybe T.Text
  , message_quick_reply :: Maybe QuickReply
  , message_attachments :: Maybe [Attachment]
  , message_metadata :: Maybe T.Text
  } deriving Show

data Name = Name
  { name_first_name :: Maybe T.Text
  , name_last_name :: Maybe T.Text
  } deriving Show

fromSender :: Sender -> Recipient
fromSender (Sender id) = Recipient id Nothing Nothing

fromRecipient :: Recipient -> Sender
fromRecipient (Recipient id _ _) = Sender id

$(deriveJSON defaultOptions{fieldLabelModifier = drop 7, omitNothingFields = True} ''Sender)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 10, omitNothingFields = True} ''Recipient)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 8, omitNothingFields = True} ''Message)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 5, omitNothingFields = True} ''Name)

instance ToJSON Payload where
  toJSON Payload = object []

instance FromJSON Payload where
  parseJSON = withObject "Payload" $ \o ->
    pure Payload
