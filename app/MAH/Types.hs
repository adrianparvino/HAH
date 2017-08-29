{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MAH.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Void
import qualified Data.Text.Lazy as T

data Common = Common
  { common_object :: T.Text
  , common_entry :: [Entry]
  } deriving Show

data Entry = Entry
  { entry_id :: T.Text
  , entry_time :: Integer
  , entry_messaging :: [Messaging]
  } deriving Show

data Messaging =
  MessagingMessage Sender Recipient Integer Message
  deriving Show

data Sender = Sender
  { sender_id :: T.Text
  } deriving Show

data Recipient = Recipient
  { recipient_id :: T.Text
  } deriving Show

data Message = Message
  { message_mid :: T.Text
  , message_text :: Maybe T.Text
  , message_quick_reply :: Maybe QuickReply
  , message_attachments :: Maybe [Attachments]
  } deriving Show

data QuickReply = QuickReply
  { quickreply_payload :: Payload
  } deriving Show
data Payload =
  Payload
  deriving Show
data Attachments =
  AttachmentsFallback (Maybe T.Text) (Maybe T.Text) (Maybe Void)
  deriving Show

$(deriveJSON defaultOptions{fieldLabelModifier = drop 7, omitNothingFields = True} ''Common)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 6, omitNothingFields = True} ''Entry)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 7, omitNothingFields = True} ''Sender)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 10, omitNothingFields = True} ''Recipient)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 8, omitNothingFields = True} ''Message)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 11, omitNothingFields = True} ''QuickReply)

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

instance ToJSON Attachments where
  toJSON (AttachmentsFallback title url payload) =
    object $ [ "title" .= title
             , "url" .= url
             , "payload" .= Null
             , "type" .= ("fallback" :: T.Text)
             ]
    

instance FromJSON Attachments where
  parseJSON =
    withObject "message" $ \o ->
      AttachmentsFallback
      <$> o .: "title"
      <*> o .: "url"
      <*> pure Nothing
      
instance ToJSON Payload where
  toJSON Payload = object []

instance FromJSON Payload where
  parseJSON = withObject "Payload" $ \o ->
    pure Payload
