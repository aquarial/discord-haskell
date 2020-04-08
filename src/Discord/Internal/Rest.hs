{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | Provides a higher level interface to the rest functions.
--   Preperly writes to the rate-limit loop. Creates separate
--   MVars for each call
module Discord.Internal.Rest
  ( module Discord.Internal.Types
  , DiscordHandleRestChan
  , Request(..)
  , writeRestCall
  , startRestThread
  , RestCallInternalException(..)
  ) where

import Prelude hiding (log)
import Data.Aeson (FromJSON, eitherDecode)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent (forkIO, ThreadId)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T


import Discord.Internal.Types
import Discord.Internal.Rest.HTTP

type DiscordHandleRestChan = Chan (String, JsonRequest, MVar (Either RestCallInternalException BL.ByteString))

-- | Starts the http request thread. Please only call this once
startRestThread :: Auth -> Chan T.Text -> IO (DiscordHandleRestChan, ThreadId)
startRestThread auth log = do
  c <- newChan
  tid <- forkIO $ restLoop auth c log
  pure (c, tid)

-- | Execute a request blocking until a response is received
writeRestCall :: (Request r , FromJSON (Response r)) => DiscordHandleRestChan -> r  -> IO (Either RestCallInternalException (Response r))
writeRestCall c req = do
  m <- newEmptyMVar
  writeChan c (majorRoute req, jsonRequest req, m)
  r <- readMVar m
  pure $ case eitherDecode <$> r of
    Right (Right o) -> Right o
    Right (Left er) -> Left (RestCallInternalNoParse er (case r of Right x -> x
                                                                   Left _ -> ""))
    Left e -> Left e


