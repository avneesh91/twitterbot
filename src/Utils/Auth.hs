{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils.Auth where

import qualified Web.Authenticate.OAuth as OA
import qualified Web.Twitter.Conduit.Types as TP
import qualified System.Environment as E
import qualified Control.Monad.IO.Class as IO
import Data.ByteString.Char8 as BC

data Authentication = Auth { consumerAuth ::OA.OAuth ,
                             userCredentials:: OA.Credential
                           } deriving (Show)

getCredentials :: IO Authentication
getCredentials = do
               twitterOAuth <- getTwitterOAuth
               twitterCredentials <- getTwitterCredentials
               let authObj = Auth twitterOAuth twitterCredentials
               return authObj


dataGetter :: String -> IO BC.ByteString
dataGetter = fmap BC.pack . E.getEnv

getTwitterOAuth :: IO OA.OAuth
getTwitterOAuth = do
              oauthConsumerKey <- dataGetter "oauthConsumerKey"
              oauthConsumerSecret <- dataGetter "oauthConsumerSecret"
              let oauthInfo  = TP.twitterOAuth { OA.oauthConsumerKey =  oauthConsumerKey, 
                                                 OA.oauthConsumerSecret =  oauthConsumerSecret}
              return oauthInfo
                          
getTwitterCredentials :: IO OA.Credential
getTwitterCredentials = do
              oauthToken <- dataGetter "oauthToken"
              oauthTokenSecret <- dataGetter "oauthTokenSecret"
              let credentials =  OA.Credential [("oauth_token", oauthToken), ("oauth_token_secret",  oauthTokenSecret)]
              return credentials



