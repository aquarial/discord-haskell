{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Channel API interactions
module Discord.Internal.Rest.Emoji
  ( EmojiRequest(..)
  , ModifyGuildEmojiOpts(..)
  , parseEmojiImage
  ) where

import Data.Aeson
import Data.Monoid (mempty, (<>))
import Codec.Picture
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

import Discord.Internal.Rest.Prelude
import Discord.Internal.Types
import Discord.Internal.Gateway.Cache

instance Request (EmojiRequest a) where
  type Response (EmojiRequest a) = a
  majorRoute = emojiMajorRoute
  jsonRequest = emojiJsonRequest
  updateCache = emojiUpdateCache


-- | Data constructor for requests. See <https://discordapp.com/developers/docs/resources/ API>
data EmojiRequest a where
  -- | List of emoji objects for the given guild. Requires MANAGE_EMOJIS permission.
  ListGuildEmojis :: GuildId -> EmojiRequest [Emoji]
  -- | Emoji object for the given guild and emoji ID
  GetGuildEmoji :: GuildId -> EmojiId -> EmojiRequest Emoji
  -- | Create a new guild emoji (static&animated). Requires MANAGE_EMOJIS permission.
  CreateGuildEmoji :: GuildId -> T.Text -> EmojiImageParsed -> EmojiRequest Emoji
  -- | Requires MANAGE_EMOJIS permission
  ModifyGuildEmoji :: GuildId -> EmojiId -> ModifyGuildEmojiOpts -> EmojiRequest Emoji
  -- | Requires MANAGE_EMOJIS permission
  DeleteGuildEmoji :: GuildId -> EmojiId -> EmojiRequest ()

data ModifyGuildEmojiOpts = ModifyGuildEmojiOpts
     { modifyGuildEmojiName  :: T.Text
     , modifyGuildEmojiRoles :: [RoleId]
     }

instance ToJSON ModifyGuildEmojiOpts where
  toJSON (ModifyGuildEmojiOpts name roles) =
    object [ "name" .= name, "roles" .= roles ]


data EmojiImageParsed = EmojiImageParsed T.Text

parseEmojiImage :: B.ByteString -> Either T.Text EmojiImageParsed
parseEmojiImage bs =
  if B.length bs > 256000
  then Left "Cannot create emoji - File is larger than 256kb"
  else case (decodeGifImages bs, decodeImage bs) of
         (Left e1, Left e2) -> Left ("Could not parse image or gif: " <> T.pack e1
                                                           <> " and " <> T.pack e2)
         (Right ims, _) -> if all is128 ims
                           then Right (EmojiImageParsed ("data:text/plain;"
                                                      <> "base64,"
                                                      <> TE.decodeUtf8 (B64.encode bs)))
                           else Left "The frames are not all 128x128"
         (_, Right im) -> if is128 im
                          then Right (EmojiImageParsed ("data:text/plain;"
                                                     <> "base64,"
                                                     <> TE.decodeUtf8 (B64.encode bs)))
                          else Left "Image is not 128x128"
  where
    is128 im = let i = convertRGB8 im
               in imageWidth i == 128 && imageHeight i == 128


emojiMajorRoute :: EmojiRequest a -> String
emojiMajorRoute c = case c of
  (ListGuildEmojis g) ->      "emoji " <> show g
  (GetGuildEmoji g _) ->      "emoji " <> show g
  (CreateGuildEmoji g _ _) -> "emoji " <> show g
  (ModifyGuildEmoji g _ _) -> "emoji " <> show g
  (DeleteGuildEmoji g _)   -> "emoji " <> show g

-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
  where apiVersion = "v6"

guilds :: R.Url 'R.Https
guilds = baseUrl /: "guilds"

emojiJsonRequest :: EmojiRequest r -> JsonRequest
emojiJsonRequest c = case c of
  (ListGuildEmojis g) -> Get (guilds // g /: "emojis") mempty
  (GetGuildEmoji g e) -> Get (guilds // g /: "emojis" // e) mempty
  (CreateGuildEmoji g name (EmojiImageParsed im)) ->
                   Post (guilds // g /: "emojis")
                        (pure (R.ReqBodyJson (object [ "name" .= name
                                                     , "image" .= im
                                                     -- todo , "roles" .= ...
                                                     ])))
                        mempty
  (ModifyGuildEmoji g e o) -> Patch (guilds // g /: "emojis" // e)
                                    (pure (R.ReqBodyJson o))
                                    mempty
  (DeleteGuildEmoji g e) -> Delete (guilds // g /: "emojis" // e) mempty

emojiUpdateCache :: Cache -> EmojiRequest r -> Response (EmojiRequest r) -> Cache
emojiUpdateCache c _ _ = c
