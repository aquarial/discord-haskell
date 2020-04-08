{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Utility and base types and functions for the Discord Rest API
module Discord.Internal.Rest.Prelude where

import Prelude hiding (log)
import Control.Exception.Safe (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Network.HTTP.Req as R

import Discord.Internal.Types
import Discord.Internal.Gateway (Cache)

-- | Discord requires HTTP headers for authentication.
authHeader :: Auth -> R.Option 'R.Https
authHeader auth =
          R.header "Authorization" (TE.encodeUtf8 (authToken auth))
       <> R.header "User-Agent" agent
  where
  -- | https://discordapp.com/developers/docs/reference#user-agent
  -- Second place where the library version is noted
  agent = "DiscordBot (https://github.com/aquarial/discord-haskell, 1.5.0)"

-- Append to an URL
infixl 5 //
(//) :: Show a => R.Url scheme -> a -> R.Url scheme
(//) url part = url R./: T.pack (show part)


-- | Represtents a HTTP request made to an API that supplies a Json response
data JsonRequest where
  Delete ::                 R.Url 'R.Https ->      R.Option 'R.Https -> JsonRequest
  Get    ::                 R.Url 'R.Https ->      R.Option 'R.Https -> JsonRequest
  Put    :: R.HttpBody a => R.Url 'R.Https -> a -> R.Option 'R.Https -> JsonRequest
  Patch  :: R.HttpBody a => R.Url 'R.Https -> RestIO a -> R.Option 'R.Https -> JsonRequest
  Post   :: R.HttpBody a => R.Url 'R.Https -> RestIO a -> R.Option 'R.Https -> JsonRequest

class Request a where
  type Response a :: *
  majorRoute :: a -> String
  jsonRequest :: a -> JsonRequest
  updateCache :: Cache -> a -> Response a -> Cache

-- | Same Monad as IO. Overwrite Req settings
newtype RestIO a = RestIO { restIOtoIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance R.MonadHttp RestIO where
  -- | Throw actual exceptions
  handleHttpException = liftIO . throwIO
  -- | Don't throw exceptions on http error codes like 404
  getHttpConfig = pure $ R.defaultHttpConfig { R.httpConfigCheckResponse = \_ _ _ -> Nothing }
