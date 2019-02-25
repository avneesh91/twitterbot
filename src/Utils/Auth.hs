{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils.Auth where

import qualified Web.Authenticate.OAuth as OA
import qualified Web.Twitter.Conduit.Types as TP
import qualified System.Environment as E
import qualified Control.Monad.IO.Class as IO


data Authentication = Auth OA.OAuth OA.Credential deriving (Show)


--getCredentials :: IO Authentication
--getCredentials = do
--              oauthInfo <-  TP.twitterOAuth { OA.oauthConsumerKey = oauthConsumerKey, OA.oauthConsumerSecret = oauthConsumerSecret}
--              credentials <-  OA.Credential [("oauth_token", oauthToken), ("oauth_token_secret", oauthTokenSecret)]
--              return $ Auth oauthInfo credentials
--              where oauthConsumerKey = E.getEnv "oauthConsumerKey"
--                    oauthConsumerSecret = E.getEnv "oauthConsumerSecret"
--                    oauthToken = E.getEnv "oauthToken"
--                    oauthTokenSecret = E.getEnv "oauthTokenSecret"


--getTwitterOAuth :: IO OA.OAuth
--getTwitterOAuth = do
--              oauthInfo <-  TP.twitterOAuth { OA.oauthConsumerKey =  oauthConsumerKey, OA.oauthConsumerSecret =  oauthConsumerSecret}
--              return oauthInfo
--              where oauthConsumerKey = E.getEnv "oauthConsumerKey"
--                    oauthConsumerSecret = E.getEnv "oauthConsumerSecret"
            
getTwitterCredentials :: IO OA.Credential
getTwitterCredentials = do
              oauthToken <- E.getEnv "oauthToken"
              oauthTokenSecret <- E.getEnv "oauthTokenSecret"
              credentials <-  OA.Credential [("oauth_token", oauthToken), ("oauth_token_secret",  oauthTokenSecret)]
              return credentials
