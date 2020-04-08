{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Voice API interactions
module Discord.Internal.Rest.Voice
  ( VoiceRequest(..)
  ) where


import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R

import Discord.Internal.Rest.Prelude
import Discord.Internal.Types
import Discord.Internal.Gateway.Cache

instance Request (VoiceRequest a) where
  type Response (VoiceRequest a) = a
  majorRoute = voiceMajorRoute
  jsonRequest = voiceJsonRequest
  updateCache = voiceUpdateCache

-- | Data constructor for requests. See <https://discordapp.com/developers/docs/resources/ API>
data VoiceRequest a where
  ListVoiceRegions :: VoiceRequest [VoiceRegion]

voiceMajorRoute :: VoiceRequest a -> String
voiceMajorRoute c = case c of
  (ListVoiceRegions) -> "whatever "

-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
  where apiVersion = "v6"

voices :: R.Url 'R.Https
voices = baseUrl /: "voice"

voiceJsonRequest :: VoiceRequest r -> JsonRequest
voiceJsonRequest c = case c of
  (ListVoiceRegions) -> Get (voices /: "regions") mempty

voiceUpdateCache :: Cache -> VoiceRequest r -> Response (VoiceRequest r) -> Cache
voiceUpdateCache c _ _ = c
