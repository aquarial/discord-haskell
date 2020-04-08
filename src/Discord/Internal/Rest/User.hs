{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Channel API interactions
module Discord.Internal.Rest.User
  ( UserRequest(..)
  , parseCurrentUserAvatar
  , CurrentUserAvatar
  ) where


import Data.Aeson
import Codec.Picture
import Data.Monoid (mempty, (<>))
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64

import Discord.Internal.Rest.Prelude
import Discord.Internal.Types
import Discord.Internal.Gateway

instance Request (UserRequest a) where
  type Response (UserRequest a) = a
  majorRoute = userMajorRoute
  jsonRequest = userJsonRequest
  updateCache = userUpdateCache


-- | Data constructor for requests. See <https://discordapp.com/developers/docs/resources/ API>
data UserRequest a where
  -- | Returns the 'User' object of the requester's account. For OAuth2, this requires
  --   the identify scope, which will return the object without an email, and optionally
  --   the email scope, which returns the object with an email.
  GetCurrentUser       :: UserRequest User
  -- | Returns a 'User' for a given user ID
  GetUser              :: UserId -> UserRequest User
  -- | Modify user's username & avatar pic
  ModifyCurrentUser    :: T.Text -> CurrentUserAvatar -> UserRequest User
  -- | Returns a list of user 'Guild' objects the current user is a member of.
  --   Requires the guilds OAuth2 scope.
  GetCurrentUserGuilds :: UserRequest [PartialGuild]
  -- | Leave a guild.
  LeaveGuild           :: GuildId -> UserRequest ()
  -- | Returns a list of DM 'Channel' objects
  GetUserDMs           :: UserRequest [Channel]
  -- | Create a new DM channel with a user. Returns a DM 'Channel' object.
  CreateDM             :: UserId -> UserRequest Channel

  GetUserConnections   :: UserRequest [ConnectionObject]

-- | Formatted avatar data https://discordapp.com/developers/docs/resources/user#avatar-data
data CurrentUserAvatar = CurrentUserAvatar T.Text

parseCurrentUserAvatar :: B.ByteString -> Either T.Text CurrentUserAvatar
parseCurrentUserAvatar bs =
  case decodeImage bs of
    Left e -> Left (T.pack e)
    Right im -> Right $ CurrentUserAvatar $ "data:image/png;base64,"
                <> TE.decodeUtf8 (B64.encode (BL.toStrict (encodePng (convertRGBA8 im))))


userMajorRoute :: UserRequest a -> String
userMajorRoute c = case c of
  (GetCurrentUser) ->                        "me "
  (GetUser _) ->                           "user "
  (ModifyCurrentUser _ _) ->        "modify_user "
  (GetCurrentUserGuilds) ->     "get_user_guilds "
  (LeaveGuild g) ->                 "leave_guild " <> show g
  (GetUserDMs) ->                       "get_dms "
  (CreateDM _) ->                       "make_dm "
  (GetUserConnections) ->           "connections "

-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
  where apiVersion = "v6"

users :: R.Url 'R.Https
users = baseUrl /: "users"

userJsonRequest :: UserRequest r -> JsonRequest
userJsonRequest c = case c of
  (GetCurrentUser) -> Get (users /: "@me") mempty

  (GetUser user) -> Get (users // user ) mempty

  (ModifyCurrentUser name (CurrentUserAvatar im)) ->
      Patch (users /: "@me")  (pure (R.ReqBodyJson (object [ "username" .= name
                                                           , "avatar" .= im ]))) mempty

  (GetCurrentUserGuilds) -> Get (users /: "@me" /: "guilds") mempty

  (LeaveGuild guild) -> Delete (users /: "@me" /: "guilds" // guild) mempty

  (GetUserDMs) -> Get (users /: "@me" /: "channels") mempty

  (CreateDM user) ->
      let body = R.ReqBodyJson $ object ["recipient_id" .= user]
      in Post (users /: "@me" /: "channels") (pure body) mempty

  (GetUserConnections) ->
    Get (users /: "@me" /: "connections") mempty

userUpdateCache :: Cache -> UserRequest r -> Response (UserRequest r) -> Cache
userUpdateCache c _ _ = c
