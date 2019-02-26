{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Conduit
import Control.Monad.IO.Class
import Web.Twitter.Conduit
import Control.Monad.Trans.Resource
import qualified Web.Twitter.Conduit.Stream as ST
import qualified Web.Twitter.Conduit.Parameters as P
import Web.Twitter.Types.Lens

import Control.Lens
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad.Trans.Resource
import Control.Monad.IO.Class


import qualified Utils.Auth as UA



twInfo :: UA.Authentication -> TWInfo
twInfo credentials = twitterLoginAuth
        where tokens = UA.consumerAuth credentials
              oauthCredentials = UA.userCredentials credentials
              twitterLoginAuth = TWInfo{ twToken = TWToken tokens oauthCredentials, twProxy = Nothing}

getManager :: IO Manager
getManager = newManager tlsManagerSettings

getStream =  do
               mgr <- getManager
               impureTWInfo <- fmap twInfo UA.getCredentials
               runResourceT $ do
                   src <- stream impureTWInfo mgr $ statusesFilter [Track ["python", "Node.js"]]
                   C.runConduit $ src C..| CL.mapM_ (lift . printTL)

showStatus :: Status -> T.Text
showStatus s = T.concat [ s ^. user . userScreenName
                        , ":"
                        , s ^. text]

printTL :: StreamingAPI -> IO ()
printTL (SStatus s) = T.putStrLn . showStatus $ s
printTL (SRetweetedStatus s) = T.putStrLn $ T.concat [ s ^. user . userScreenName
                                                     , ": RT @"
                                                     , showStatus (s ^. rsRetweetedStatus)
                                                     ]
printTL x = print x

main :: IO()
main = getStream
