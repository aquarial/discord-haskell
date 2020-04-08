{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Channel API interactions
module Discord.Internal.Rest.Channel
  ( ChannelRequest(..)
  , ReactionTiming(..)
  , MessageTiming(..)
  , ChannelInviteOpts(..)
  , ModifyChannelOpts(..)
  , ChannelPermissionsOpts(..)
  , GroupDMAddRecipientOpts(..)
  , ChannelPermissionsOptsType(..)
  ) where


import Data.Aeson
import Data.Emoji (unicodeByName)
import Data.Monoid (mempty, (<>))
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client (RequestBody (RequestBodyBS))
import Network.HTTP.Client.MultipartFormData (partFileRequestBody, partBS, PartM)
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import qualified Data.Map as M

import Discord.Internal.Rest.Prelude
import Discord.Internal.Types
import Discord.Internal.Gateway

instance Request (ChannelRequest a) where
  type Response (ChannelRequest a) = a
  majorRoute = channelMajorRoute
  jsonRequest = channelJsonRequest
  updateCache = channelUpdateCache

-- | Data constructor for requests. See <https://discordapp.com/developers/docs/resources/ API>
data ChannelRequest a where
  -- | Gets a channel by its id.
  GetChannel              :: ChannelId -> ChannelRequest Channel
  -- | Edits channels options.
  ModifyChannel           :: ChannelId -> ModifyChannelOpts -> ChannelRequest Channel
  -- | Deletes a channel if its id doesn't equal to the id of guild.
  DeleteChannel           :: ChannelId -> ChannelRequest Channel
  -- | Gets a messages from a channel with limit of 100 per request.
  GetChannelMessages      :: ChannelId -> (Int, MessageTiming) -> ChannelRequest [Message]
  -- | Gets a message in a channel by its id.
  GetChannelMessage       :: (ChannelId, MessageId) -> ChannelRequest Message
  -- | Sends a message to a channel.
  CreateMessage           :: ChannelId -> T.Text -> ChannelRequest Message
  -- | Sends a message with an Embed to a channel.
  CreateMessageEmbed      :: ChannelId -> T.Text -> CreateEmbed -> ChannelRequest Message
  -- | Sends a message with a file to a channel.
  CreateMessageUploadFile :: ChannelId -> T.Text -> B.ByteString -> ChannelRequest Message
  -- | Add an emoji reaction to a message. ID must be present for custom emoji
  CreateReaction          :: (ChannelId, MessageId) -> T.Text -> ChannelRequest ()
  -- | Remove a Reaction this bot added
  DeleteOwnReaction       :: (ChannelId, MessageId) -> T.Text -> ChannelRequest ()
  -- | Remove a Reaction someone else added
  DeleteUserReaction      :: (ChannelId, MessageId) -> UserId -> T.Text -> ChannelRequest ()
  -- | Deletes all reactions of a single emoji on a message
  DeleteSingleReaction    :: (ChannelId, MessageId) -> T.Text -> ChannelRequest ()
  -- | List of users that reacted with this emoji
  GetReactions            :: (ChannelId, MessageId) -> T.Text -> (Int, ReactionTiming) -> ChannelRequest [User]
  -- | Delete all reactions on a message
  DeleteAllReactions      :: (ChannelId, MessageId) -> ChannelRequest ()
  -- | Edits a message content.
  EditMessage             :: (ChannelId, MessageId) -> T.Text -> Maybe CreateEmbed
                                                    -> ChannelRequest Message
  -- | Deletes a message.
  DeleteMessage           :: (ChannelId, MessageId) -> ChannelRequest ()
  -- | Deletes a group of messages.
  BulkDeleteMessage       :: (ChannelId, [MessageId]) -> ChannelRequest ()
  -- | Edits a permission overrides for a channel.
  EditChannelPermissions  :: ChannelId -> OverwriteId -> ChannelPermissionsOpts -> ChannelRequest ()
  -- | Gets all instant invites to a channel.
  GetChannelInvites       :: ChannelId -> ChannelRequest Object
  -- | Creates an instant invite to a channel.
  CreateChannelInvite     :: ChannelId -> ChannelInviteOpts -> ChannelRequest Invite
  -- | Deletes a permission override from a channel.
  DeleteChannelPermission :: ChannelId -> OverwriteId -> ChannelRequest ()
  -- | Sends a typing indicator a channel which lasts 10 seconds.
  TriggerTypingIndicator  :: ChannelId -> ChannelRequest ()
  -- | Gets all pinned messages of a channel.
  GetPinnedMessages       :: ChannelId -> ChannelRequest [Message]
  -- | Pins a message.
  AddPinnedMessage        :: (ChannelId, MessageId) -> ChannelRequest ()
  -- | Unpins a message.
  DeletePinnedMessage     :: (ChannelId, MessageId) -> ChannelRequest ()
  -- | Adds a recipient to a Group DM using their access token
  GroupDMAddRecipient     :: ChannelId -> GroupDMAddRecipientOpts -> ChannelRequest ()
  -- | Removes a recipient from a Group DM
  GroupDMRemoveRecipient  :: ChannelId -> UserId -> ChannelRequest ()


-- | Data constructor for GetReaction requests
data ReactionTiming = BeforeReaction MessageId
                    | AfterReaction MessageId
                    | LatestReaction

reactionTimingToQuery :: ReactionTiming -> R.Option 'R.Https
reactionTimingToQuery t = case t of
  (BeforeReaction snow) -> "before" R.=: show snow
  (AfterReaction snow) -> "after"  R.=: show snow
  (LatestReaction) -> mempty

-- | Data constructor for GetChannelMessages requests. See <https://discordapp.com/developers/docs/resources/channel#get-channel-messages>
data MessageTiming = AroundMessage MessageId
                   | BeforeMessage MessageId
                   | AfterMessage MessageId
                   | LatestMessages

messageTimingToQuery :: MessageTiming -> R.Option 'R.Https
messageTimingToQuery t = case t of
  (AroundMessage snow) -> "around" R.=: show snow
  (BeforeMessage snow) -> "before" R.=: show snow
  (AfterMessage snow) -> "after"  R.=: show snow
  (LatestMessages) -> mempty

data ChannelInviteOpts = ChannelInviteOpts
  { channelInviteOptsMaxAgeSeconds          :: Maybe Integer
  , channelInviteOptsMaxUsages              :: Maybe Integer
  , channelInviteOptsIsTemporary            :: Maybe Bool
  , channelInviteOptsDontReuseSimilarInvite :: Maybe Bool
  }

instance ToJSON ChannelInviteOpts where
  toJSON ChannelInviteOpts{..} = object [(name, val) | (name, Just val) <-
                         [("max_age",   toJSON <$> channelInviteOptsMaxAgeSeconds),
                          ("max_uses",  toJSON <$> channelInviteOptsMaxUsages),
                          ("temporary", toJSON <$> channelInviteOptsIsTemporary),
                          ("unique",    toJSON <$> channelInviteOptsDontReuseSimilarInvite) ] ]

data ModifyChannelOpts = ModifyChannelOpts
  { modifyChannelName                 :: Maybe T.Text
  , modifyChannelPosition             :: Maybe Integer
  , modifyChannelTopic                :: Maybe T.Text
  , modifyChannelNSFW                 :: Maybe Bool
  , modifyChannelBitrate              :: Maybe Integer
  , modifyChannelUserRateLimit        :: Maybe Integer
  , modifyChannelPermissionOverwrites :: Maybe [Overwrite]
  , modifyChannelParentId             :: Maybe ChannelId
  }

instance ToJSON ModifyChannelOpts where
  toJSON ModifyChannelOpts{..} = object [(name, val) | (name, Just val) <-
               [("name",       toJSON <$> modifyChannelName),
                ("position",   toJSON <$> modifyChannelPosition),
                ("topic",      toJSON <$> modifyChannelTopic),
                ("nsfw",       toJSON <$> modifyChannelNSFW),
                ("bitrate",    toJSON <$> modifyChannelBitrate),
                ("user_limit", toJSON <$> modifyChannelUserRateLimit),
                ("permission_overwrites",  toJSON <$> modifyChannelPermissionOverwrites),
                ("parent_id",  toJSON <$> modifyChannelParentId) ] ]

data ChannelPermissionsOpts = ChannelPermissionsOpts
  { channelPermissionsOptsAllow :: Integer
  , channelPermissionsOptsDeny :: Integer
  , channelPermissionsOptsType :: ChannelPermissionsOptsType}

data ChannelPermissionsOptsType = ChannelPermissionsOptsUser
                                | ChannelPermissionsOptsRole

instance ToJSON ChannelPermissionsOptsType where
  toJSON t = case t of ChannelPermissionsOptsUser -> String "member"
                       ChannelPermissionsOptsRole -> String "role"

instance ToJSON ChannelPermissionsOpts where
  toJSON (ChannelPermissionsOpts a d t) = object [ ("allow", toJSON a )
                                                 , ("deny", toJSON d)
                                                 , ("type", toJSON t)]

-- | https://discordapp.com/developers/docs/resources/channel#group-dm-add-recipient
data GroupDMAddRecipientOpts = GroupDMAddRecipientOpts
  { groupDMAddRecipientUserToAdd :: UserId
  , groupDMAddRecipientUserToAddNickName :: T.Text
  , groupDMAddRecipientGDMJoinAccessToken :: T.Text
  }

channelMajorRoute :: ChannelRequest a -> String
channelMajorRoute c = case c of
  (GetChannel chan) ->                 "get_chan " <> show chan
  (ModifyChannel chan _) ->            "mod_chan " <> show chan
  (DeleteChannel chan) ->              "mod_chan " <> show chan
  (GetChannelMessages chan _) ->            "msg " <> show chan
  (GetChannelMessage (chan, _)) ->      "get_msg " <> show chan
  (CreateMessage chan _) ->                 "msg " <> show chan
  (CreateMessageEmbed chan _ _) ->          "msg " <> show chan
  (CreateMessageUploadFile chan _ _) ->     "msg " <> show chan
  (CreateReaction (chan, _) _) ->     "add_react " <> show chan
  (DeleteOwnReaction (chan, _) _) ->      "react " <> show chan
  (DeleteUserReaction (chan, _) _ _) ->   "react " <> show chan
  (DeleteSingleReaction (chan, _) _) ->   "react " <> show chan
  (GetReactions (chan, _) _ _) ->         "react " <> show chan
  (DeleteAllReactions (chan, _)) ->       "react " <> show chan
  (EditMessage (chan, _) _ _) ->        "get_msg " <> show chan
  (DeleteMessage (chan, _)) ->          "get_msg " <> show chan
  (BulkDeleteMessage (chan, _)) ->     "del_msgs " <> show chan
  (EditChannelPermissions chan _ _) ->    "perms " <> show chan
  (GetChannelInvites chan) ->           "invites " <> show chan
  (CreateChannelInvite chan _) ->       "invites " <> show chan
  (DeleteChannelPermission chan _) ->     "perms " <> show chan
  (TriggerTypingIndicator chan) ->          "tti " <> show chan
  (GetPinnedMessages chan) ->              "pins " <> show chan
  (AddPinnedMessage (chan, _)) ->           "pin " <> show chan
  (DeletePinnedMessage (chan, _)) ->        "pin " <> show chan
  (GroupDMAddRecipient chan _) ->       "groupdm " <> show chan
  (GroupDMRemoveRecipient chan _) ->    "groupdm " <> show chan

cleanupEmoji :: T.Text -> T.Text
cleanupEmoji emoji =
  let noAngles = T.replace "<" "" (T.replace ">" "" emoji)
      byName = T.pack <$> unicodeByName (T.unpack (T.replace ":" "" emoji))
  in case (byName, T.stripPrefix ":" noAngles) of
    (Just e, _) -> e
    (_, Just a) -> "custom:" <> a
    (_, Nothing) -> noAngles

maybeEmbed :: Maybe CreateEmbed -> [PartM IO]
maybeEmbed = --maybe [] $ \embed -> ["embed" .= createEmbed embed]
      let mkPart (name,content) = partFileRequestBody name (T.unpack name) (RequestBodyBS content)
          uploads CreateEmbed{..} = [(n,c) | (n, Just (CreateEmbedImageUpload c)) <-
                                          [ ("author.png", createEmbedAuthorIcon)
                                          , ("thumbnail.png", createEmbedThumbnail)
                                          , ("image.png", createEmbedImage)
                                          , ("footer.png", createEmbedFooterIcon) ]]
      in maybe [] (map mkPart . uploads)

-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
  where apiVersion = "v6"

channels :: R.Url 'R.Https
channels = baseUrl /: "channels"

channelJsonRequest :: ChannelRequest r -> JsonRequest
channelJsonRequest c = case c of
  (GetChannel chan) ->
      Get (channels // chan) mempty

  (ModifyChannel chan patch) ->
      Patch (channels // chan) (pure (R.ReqBodyJson patch)) mempty

  (DeleteChannel chan) ->
      Delete (channels // chan) mempty

  (GetChannelMessages chan (n,timing)) ->
      let n' = if n < 1 then 1 else (if n > 100 then 100 else n)
          options = "limit" R.=: n' <> messageTimingToQuery timing
      in Get (channels // chan /: "messages") options

  (GetChannelMessage (chan, msg)) ->
      Get (channels // chan /: "messages" // msg) mempty

  (CreateMessage chan msg) ->
      let content = ["content" .= msg]
          body = pure $ R.ReqBodyJson $ object content
      in Post (channels // chan /: "messages") body mempty

  (CreateMessageEmbed chan msg embed) ->
      let partJson = partBS "payload_json" $ BL.toStrict $ encode $ toJSON $ object ["content" .= msg, "embed" .= createEmbed embed]
          body = R.reqBodyMultipart (partJson : maybeEmbed (Just embed))
      in Post (channels // chan /: "messages") body mempty

  (CreateMessageUploadFile chan fileName file) ->
      let part = partFileRequestBody "file" (T.unpack fileName) $ RequestBodyBS file
          body = R.reqBodyMultipart [part]
      in Post (channels // chan /: "messages") body mempty

  (CreateReaction (chan, msgid) emoji) ->
      let e = cleanupEmoji emoji
      in Put (channels // chan /: "messages" // msgid /: "reactions" /: e /: "@me" )
             R.NoReqBody mempty

  (DeleteOwnReaction (chan, msgid) emoji) ->
      let e = cleanupEmoji emoji
      in Delete (channels // chan /: "messages" // msgid /: "reactions" /: e /: "@me" ) mempty

  (DeleteUserReaction (chan, msgid) uID emoji) ->
      let e = cleanupEmoji emoji
      in Delete (channels // chan /: "messages" // msgid /: "reactions" /: e // uID ) mempty

  (DeleteSingleReaction (chan, msgid) emoji) ->
    let e = cleanupEmoji emoji
    in Delete (channels // chan /: "messages" // msgid /: "reactions" /: e) mempty

  (GetReactions (chan, msgid) emoji (n, timing)) ->
      let e = cleanupEmoji emoji
          n' = if n < 1 then 1 else (if n > 100 then 100 else n)
          options = "limit" R.=: n' <> reactionTimingToQuery timing
      in Get (channels // chan /: "messages" // msgid /: "reactions" /: e) options

  (DeleteAllReactions (chan, msgid)) ->
      Delete (channels // chan /: "messages" // msgid /: "reactions" ) mempty

  (EditMessage (chan, msg) new embed) ->
      let partJson = partBS "payload_json" $ BL.toStrict $ encode $ toJSON $ object ["content" .= new]
          body = R.reqBodyMultipart (partJson : maybeEmbed embed)
      in Patch (channels // chan /: "messages" // msg) body mempty

  (DeleteMessage (chan, msg)) ->
      Delete (channels // chan /: "messages" // msg) mempty

  (BulkDeleteMessage (chan, msgs)) ->
      let body = pure . R.ReqBodyJson $ object ["messages" .= msgs]
      in Post (channels // chan /: "messages" /: "bulk-delete") body mempty

  (EditChannelPermissions chan perm patch) ->
      Put (channels // chan /: "permissions" // perm) (R.ReqBodyJson patch) mempty

  (GetChannelInvites chan) ->
      Get (channels // chan /: "invites") mempty

  (CreateChannelInvite chan patch) ->
      Post (channels // chan /: "invites") (pure (R.ReqBodyJson patch)) mempty

  (DeleteChannelPermission chan perm) ->
      Delete (channels // chan /: "permissions" // perm) mempty

  (TriggerTypingIndicator chan) ->
      Post (channels // chan /: "typing") (pure R.NoReqBody) mempty

  (GetPinnedMessages chan) ->
      Get (channels // chan /: "pins") mempty

  (AddPinnedMessage (chan, msg)) ->
      Put (channels // chan /: "pins" // msg) R.NoReqBody mempty

  (DeletePinnedMessage (chan, msg)) ->
      Delete (channels // chan /: "pins" // msg) mempty

  (GroupDMAddRecipient chan (GroupDMAddRecipientOpts uid nick tok)) ->
      Put (channels // chan // chan /: "recipients" // uid)
          (R.ReqBodyJson (object [ ("access_token", toJSON tok)
                                 , ("nick", toJSON nick)]))
          mempty

  (GroupDMRemoveRecipient chan userid) ->
      Delete (channels // chan // chan /: "recipients" // userid) mempty

channelUpdateCache :: Cache -> ChannelRequest a -> Response (ChannelRequest a) -> Cache
channelUpdateCache c (GetChannel cid) channel =
  c { _dmChannels = if isDMChannel channel then M.insert cid channel (_dmChannels c) else _dmChannels c, _channels = M.insert cid channel (_channels c)}
  where
    isDMChannel (ChannelDirectMessage {..}) = True
    isDMChannel _ = False
channelUpdateCache c _ _ = c
