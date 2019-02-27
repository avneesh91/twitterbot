{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where


import Conduit
import Control.Lens
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.String as DT

import qualified Data.Text as T
import qualified Utils.Auth as UA
import qualified Data.Conduit as C
import qualified Data.Text.IO as T
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as B8
import qualified Web.Twitter.Conduit.Stream as ST
import qualified Web.Twitter.Conduit.Parameters as P
import qualified Web.Twitter.Conduit.Api as AP
import qualified Web.Twitter.Conduit.Request as RQ
import qualified Web.Twitter.Types as TP

twInfo :: UA.Authentication -> TWInfo
twInfo credentials = twitterLoginAuth
        where tokens = UA.consumerAuth credentials
              oauthCredentials = UA.userCredentials credentials
              twitterLoginAuth = TWInfo{ twToken = TWToken tokens oauthCredentials, twProxy = Nothing}

getManager :: IO Manager
getManager = newManager tlsManagerSettings

getStream :: IO ()
getStream =  do
               mgr <- getManager
               impureTWInfo <- fmap twInfo UA.getCredentials
               runResourceT $ do
                   src <- stream impureTWInfo mgr $ statusesFilter [Track ["#remote-work", "#remote-jobs", "#remotework", "#TechJobRemote", "remotework"]]
                   C.runConduit $ src C..| CL.mapM_ (lift . processTweet)

getUserName :: Status -> T.Text
getUserName currStatus = currStatus ^. user . userScreenName

followUser :: StreamingAPI -> T.Text
followUser (SStatus s) =  getUserName s
followUser (SRetweetedStatus s) = getUserName $ s ^. rsRetweetedStatus

processTweet :: StreamingAPI -> IO ()
processTweet tweet =  do
                         mgr <- getManager
                         impureTWInfo <- fmap twInfo UA.getCredentials
                         let origUser = followUser tweet
                         T.putStrLn origUser
                         let userScreenName = P.ScreenNameParam $ T.unpack origUser
                         let followUserIntent =  AP.friendshipsCreate userScreenName
                         res <- call impureTWInfo mgr followUserIntent
                         return ()

main :: IO()
main = getStream
