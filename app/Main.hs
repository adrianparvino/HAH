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

module Main where

import HAH

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

import Data.Aeson
import Data.Aeson.TH
import Data.List
import Data.Monoid
import Data.Traversable
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import Web.Spock
import Web.Spock.Config
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import qualified Network.Wreq as Wreq

import MAH.Common
import qualified MAH.Webhook as Webhook
import qualified MAH.Send as Send

secretKey = "EAADE4GTC9ZBwBANMtZAw8gbqLwGtiRZClUBvNW2gSbQyYjRgISaq45wWRNBXJJeKjtrDjnBK2uFmVoXhhrtldOZCt9wY0jC3gDXmZAC7c2A2GVxumdPzW0EBRlZB6PrI3ZBIRtXlSgYgFrl0rN2wGHLNd6SZBSqKjquxUzRaTKe8WiJYyH2Q3e6W"
tlsCert = "/etc/letsencrypt/live/mah.sadale.net/cert.pem"
tlsKey  = "/etc/letsencrypt/live/mah.sadale.net/privkey.pem"
  
main = do
  xs <- atomically $ newTVar []
  spockCfg <- defaultSpockCfg Nothing PCNoDatabase xs
  runSpock 80 $ spock spockCfg system

system :: SpockM conn sess (TVar [Message]) ()
system = do
  get "/messages" $ do
    xs <- getState
    requests <- liftIO . atomically $ readTVar xs
    text . T.pack . show $ requests
    return ()
  get "/webhook" $ do
    mode <- lookup "hub.mode" <$> params
    case mode of
      Just "subscribe" -> do
        runMaybeT $ do
          challenge <- MaybeT $ lookup "hub.challenge" <$> params
          lift $ text challenge
        return ()
      Nothing ->
        return ()
  post "/webhook" $ do
    body >>= liftIO . putStrLn . BS.unpack
    runMaybeT $ do
      (request :: Webhook.Webhook)  <- MaybeT jsonBody
      liftIO $ print "Success"
      case request of
        (Webhook.Webhook "page" entries) -> for entries $
          \(Webhook.Entry _ _ messaging) -> for messaging $
            \(Webhook.MessagingMessage sender _ _ message) -> do
              tvar <- lift getState
              liftIO . atomically $ modifyTVar tvar (message:)
              let payload = Send.Send (fromSender sender) (Just message{message_mid=Nothing}) Nothing Nothing Nothing
              liftIO . putStrLn . LBS.unpack $ "payload: " <> encode payload
              liftIO . Wreq.post ("https://graph.facebook.com/me/messages?access_token=" <> secretKey)
                $ toJSON payload
              return ()
    return ()

