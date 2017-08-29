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

import Web.Spock
import Web.Spock.Config
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import qualified Network.Wreq as Wreq

import MAH.Common
import qualified MAH.Webhook as Webhook
import qualified MAH.Send as Send

secretKey = "abcd"
tlsCert = "/etc/letsencrypt/live/mah.sadale.net/cert.pem"
tlsKey  = "/etc/letsencrypt/live/mah.sadale.net/privkey.pem"
  
main = do
  xs <- atomically $ newTVar []
  spockCfg <- defaultSpockCfg Nothing PCNoDatabase xs
  app <- spockAsApp $ spock spockCfg system
  runTLS (tlsSettings tlsCert tlsKey) (setPort 443 $ defaultSettings) app

system :: SpockM conn sess (TVar [Webhook.Webhook]) ()
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
    body >>= liftIO . print . show
    runMaybeT $ do
      (request :: Webhook.Webhook)  <- MaybeT $ jsonBody
      xs <- lift getState
      liftIO . atomically $ modifyTVar xs (request:)
      case request of
        (Webhook.Webhook "page" entries) -> for entries $
          \(Webhook.Entry _ _ messaging) -> do
            for messaging $
              \(Webhook.MessagingMessage sender _ _ message) -> do
                liftIO $ Wreq.post ("https://graph.facebook.com/me/messages?access_token=" <> secretKey)
                  $ toJSON $ Send.Send (fromSender sender) (Just message) Nothing Nothing Nothing
                return ()
    return ()

