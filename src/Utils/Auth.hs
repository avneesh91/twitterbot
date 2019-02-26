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

-- getConsumerAuth ::  IO Authentication -> OA.OAuth
-- getConsumerAuth a  = a >>= \x -> return $ consumerAuth x

-- getUserCredentials :: IO Authentication -> IO OA.Credential
-- getUserCredentials a = a >>= \x -> return $ userCredentials x

getCredentials :: IO Authentication
getCredentials = do
               twitterOAuth <- getTwitterOAuth
               twitterCredentials <- getTwitterCredentials
               let authObj = Auth twitterOAuth twitterCredentials
               return authObj

getByteString :: IO String -> IO BC.ByteString
getByteString a = a >>= \x -> return $ BC.pack x

dataGetter :: String -> IO BC.ByteString
dataGetter = getByteString . E.getEnv

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



