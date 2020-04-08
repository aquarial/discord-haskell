{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies#-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Channel API interactions
module Discord.Internal.Rest.Guild
  ( GuildRequest(..)
  , CreateGuildChannelOpts(..)
  , CreateGuildOpts(..)
  , ModifyGuildOpts(..)
  , AddGuildMemberOpts(..)
  , ModifyGuildMemberOpts(..)
  , GuildMembersTiming(..)
  , CreateGuildBanOpts(..)
  , ModifyGuildRoleOpts(..)
  , CreateGuildIntegrationOpts(..)
  , ModifyGuildIntegrationOpts(..)
  ) where


import Data.Aeson
import Data.Monoid (mempty, (<>))
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import qualified Data.Text as T

import Discord.Internal.Rest.Prelude
import Discord.Internal.Types
import Discord.Internal.Gateway.Cache

instance Request (GuildRequest a) where
  type Response (GuildRequest a) = a
  majorRoute = guildMajorRoute
  jsonRequest = guildJsonRequest
  updateCache = guildUpdateCache

-- | Data constructor for requests. See <https://discordapp.com/developers/docs/resources/ API>
data GuildRequest a where
  CreateGuild              :: CreateGuildOpts -> GuildRequest Guild
  -- | Returns the new 'Guild' object for the given id
  GetGuild                 :: GuildId -> GuildRequest Guild
  -- | Modify a guild's settings. Returns the updated 'Guild' object on success. Fires a
  --   Guild Update 'Event'.
  ModifyGuild              :: GuildId -> ModifyGuildOpts -> GuildRequest Guild
  -- | Delete a guild permanently. User must be owner. Fires a Guild Delete 'Event'.
  DeleteGuild              :: GuildId -> GuildRequest ()
  -- | Returns a list of guild 'Channel' objects
  GetGuildChannels         :: GuildId -> GuildRequest [Channel]
  -- | Create a new 'Channel' object for the guild. Requires 'MANAGE_CHANNELS'
  --   permission. Returns the new 'Channel' object on success. Fires a Channel Create
  --   'Event'
  CreateGuildChannel       :: GuildId -> T.Text -> [Overwrite] -> CreateGuildChannelOpts -> GuildRequest Channel
  -- | Modify the positions of a set of channel objects for the guild. Requires
  --   'MANAGE_CHANNELS' permission. Returns a list of all of the guild's 'Channel'
  --   objects on success. Fires multiple Channel Update 'Event's.
  ModifyGuildChannelPositions      :: GuildId -> [(ChannelId,Int)] -> GuildRequest [Channel]
  -- | Returns a guild 'Member' object for the specified user
  GetGuildMember           :: GuildId -> UserId -> GuildRequest GuildMember
  -- | Returns a list of guild 'Member' objects that are members of the guild.
  ListGuildMembers         :: GuildId -> GuildMembersTiming -> GuildRequest [GuildMember]
  -- | Adds a user to the guild, provided you have a valid oauth2 access token
  --   for the user with the guilds.join scope. Returns the guild 'Member' as the body.
  --   Fires a Guild Member Add 'Event'. Requires the bot to have the
  --   CREATE_INSTANT_INVITE permission.
  AddGuildMember           :: GuildId -> UserId -> AddGuildMemberOpts
                                      -> GuildRequest ()
  -- | Modify attributes of a guild 'Member'. Fires a Guild Member Update 'Event'.
  ModifyGuildMember        :: GuildId -> UserId -> ModifyGuildMemberOpts -> GuildRequest ()
  -- | Modify the nickname of the current user
  ModifyCurrentUserNick    :: GuildId -> T.Text -> GuildRequest ()
  -- | Add a member to a guild role. Requires 'MANAGE_ROLES' permission.
  AddGuildMemberRole    :: GuildId -> UserId -> RoleId -> GuildRequest ()
  -- | Remove a member from a guild role. Requires 'MANAGE_ROLES' permission.
  RemoveGuildMemberRole    :: GuildId -> UserId -> RoleId -> GuildRequest ()
  -- | Remove a member from a guild. Requires 'KICK_MEMBER' permission. Fires a
  --   Guild Member Remove 'Event'.
  RemoveGuildMember        :: GuildId -> UserId -> GuildRequest ()
  -- | Returns a list of 'Ban' objects for users that are banned from this guild. Requires the
  --   'BAN_MEMBERS' permission
  GetGuildBans             :: GuildId -> GuildRequest [GuildBan]
  -- | Returns a 'Ban' object for the user banned from this guild. Requires the
  --   'BAN_MEMBERS' permission
  GetGuildBan              :: GuildId -> UserId -> GuildRequest GuildBan
  -- | Create a guild ban, and optionally Delete previous messages sent by the banned
  --   user. Requires the 'BAN_MEMBERS' permission. Fires a Guild Ban Add 'Event'.
  CreateGuildBan           :: GuildId -> UserId -> CreateGuildBanOpts -> GuildRequest ()
  -- | Remove the ban for a user. Requires the 'BAN_MEMBERS' permissions.
  --   Fires a Guild Ban Remove 'Event'.
  RemoveGuildBan           :: GuildId -> UserId -> GuildRequest ()
  -- | Returns a list of 'Role' objects for the guild. Requires the 'MANAGE_ROLES'
  --   permission
  GetGuildRoles            :: GuildId -> GuildRequest [Role]
  -- | Create a new 'Role' for the guild. Requires the 'MANAGE_ROLES' permission.
  --   Returns the new role object on success. Fires a Guild Role Create 'Event'.
  CreateGuildRole          :: GuildId -> ModifyGuildRoleOpts -> GuildRequest Role
  -- | Modify the positions of a set of role objects for the guild. Requires the
  --   'MANAGE_ROLES' permission. Returns a list of all of the guild's 'Role' objects
  --   on success. Fires multiple Guild Role Update 'Event's.
  ModifyGuildRolePositions :: GuildId -> [(RoleId, Integer)] -> GuildRequest [Role]
  -- | Modify a guild role. Requires the 'MANAGE_ROLES' permission. Returns the
  --   updated 'Role' on success. Fires a Guild Role Update 'Event's.
  ModifyGuildRole          :: GuildId -> RoleId -> ModifyGuildRoleOpts -> GuildRequest Role
  -- | Delete a guild role. Requires the 'MANAGE_ROLES' permission. Fires a Guild Role
  --   Delete 'Event'.
  DeleteGuildRole          :: GuildId -> RoleId -> GuildRequest ()
  -- | Returns an object with one 'pruned' key indicating the number of members
  --   that would be removed in a prune operation. Requires the 'KICK_MEMBERS'
  --   permission.
  GetGuildPruneCount       :: GuildId -> Integer -> GuildRequest Object
  -- | Begin a prune operation. Requires the 'KICK_MEMBERS' permission. Returns an
  --   object with one 'pruned' key indicating the number of members that were removed
  --   in the prune operation. Fires multiple Guild Member Remove 'Events'.
  BeginGuildPrune          :: GuildId -> Integer -> GuildRequest Object
  -- | Returns a list of 'VoiceRegion' objects for the guild. Unlike the similar /voice
  --   route, this returns VIP servers when the guild is VIP-enabled.
  GetGuildVoiceRegions     :: GuildId -> GuildRequest [VoiceRegion]
  -- | Returns a list of 'Invite' objects for the guild. Requires the 'MANAGE_GUILD'
  --   permission.
  GetGuildInvites          :: GuildId -> GuildRequest [Invite]
  -- | Return a list of 'Integration' objects for the guild. Requires the 'MANAGE_GUILD'
  --   permission.
  GetGuildIntegrations     :: GuildId -> GuildRequest [Integration]
  -- | Attach an 'Integration' object from the current user to the guild. Requires the
  --   'MANAGE_GUILD' permission. Fires a Guild Integrations Update 'Event'.
  CreateGuildIntegration   :: GuildId -> IntegrationId -> CreateGuildIntegrationOpts -> GuildRequest ()
  -- | Modify the behavior and settings of a 'Integration' object for the guild.
  --   Requires the 'MANAGE_GUILD' permission. Fires a Guild Integrations Update 'Event'.
  ModifyGuildIntegration   :: GuildId -> IntegrationId -> ModifyGuildIntegrationOpts
                                      -> GuildRequest ()
  -- | Delete the attached 'Integration' object for the guild. Requires the
  --   'MANAGE_GUILD' permission. Fires a Guild Integrations Update 'Event'.
  DeleteGuildIntegration   :: GuildId -> IntegrationId -> GuildRequest ()
  -- | Sync an 'Integration'. Requires the 'MANAGE_GUILD' permission.
  SyncGuildIntegration     :: GuildId -> IntegrationId -> GuildRequest ()
  -- | Returns the 'GuildEmbed' object. Requires the 'MANAGE_GUILD' permission.
  GetGuildEmbed            :: GuildId -> GuildRequest GuildEmbed
  -- | Modify a 'GuildEmbed' object for the guild. All attributes may be passed in with
  --   JSON and modified. Requires the 'MANAGE_GUILD' permission. Returns the updated
  --   'GuildEmbed' object.
  ModifyGuildEmbed         :: GuildId -> GuildEmbed -> GuildRequest GuildEmbed
  -- | Vanity URL
  GetGuildVanityURL        :: GuildId -> GuildRequest T.Text

data CreateGuildOpts = CreateGuildOpts
  { createGuildOptsName :: T.Text
  , createGuildOptsChannels :: [Channel]
  } deriving (Show, Eq, Ord)

instance ToJSON CreateGuildOpts where
  toJSON CreateGuildOpts{..} =  object [(name, val) | (name, Just val) <-
         [ ("name", toJSON <$> pure createGuildOptsName )
         , ("channels", toJSON <$> pure createGuildOptsChannels ) ]]

data ModifyGuildIntegrationOpts = ModifyGuildIntegrationOpts
  { modifyGuildIntegrationOptsExpireBehavior :: Integer
  , modifyGuildIntegrationOptsExpireGraceSeconds :: Integer
  , modifyGuildIntegrationOptsEmoticonsEnabled :: Bool
  } deriving (Show, Eq, Ord)

instance ToJSON ModifyGuildIntegrationOpts where
  toJSON ModifyGuildIntegrationOpts{..} =  object [(name, val) | (name, Just val) <-
         [ ("expire_grace_period", toJSON <$> pure modifyGuildIntegrationOptsExpireGraceSeconds )
         , ("expire_behavior", toJSON <$> pure modifyGuildIntegrationOptsExpireBehavior )
         , ("enable_emoticons", toJSON <$> pure modifyGuildIntegrationOptsEmoticonsEnabled ) ]]

data CreateGuildIntegrationOpts = CreateGuildIntegrationOpts
  { createGuildIntegrationOptsType :: T.Text
  } deriving (Show, Eq, Ord)

instance ToJSON CreateGuildIntegrationOpts where
  toJSON CreateGuildIntegrationOpts{..} =  object [(name, val) | (name, Just val) <-
                       [("type", toJSON <$> pure createGuildIntegrationOptsType ) ]]

data CreateGuildBanOpts = CreateGuildBanOpts
  { createGuildBanOptsDeleteLastNMessages :: Maybe Int
  , createGuildBanOptsReason              :: Maybe T.Text
  } deriving (Show, Eq, Ord)

instance ToJSON CreateGuildBanOpts where
  toJSON CreateGuildBanOpts{..} =  object [(name, val) | (name, Just val) <-
                       [("delete-message-days",
                             toJSON <$> createGuildBanOptsDeleteLastNMessages ),
                        ("reason", toJSON <$> createGuildBanOptsReason )]]

data ModifyGuildRoleOpts = ModifyGuildRoleOpts
  { modifyGuildRoleOptsName            :: Maybe T.Text
  , modifyGuildRoleOptsPermissions     :: Maybe Integer
  , modifyGuildRoleOptsColor           :: Maybe Integer
  , modifyGuildRoleOptsSeparateSidebar :: Maybe Bool
  , modifyGuildRoleOptsMentionable     :: Maybe Bool
  } deriving (Show, Eq, Ord)

instance ToJSON ModifyGuildRoleOpts where
  toJSON ModifyGuildRoleOpts{..} =  object [(name, val) | (name, Just val) <-
                       [("name",        toJSON <$> modifyGuildRoleOptsName ),
                        ("permissions", toJSON <$> modifyGuildRoleOptsPermissions ),
                        ("color",       toJSON <$> modifyGuildRoleOptsColor ),
                        ("hoist",       toJSON <$> modifyGuildRoleOptsSeparateSidebar ),
                        ("mentionable", toJSON <$> modifyGuildRoleOptsMentionable )]]

data AddGuildMemberOpts = AddGuildMemberOpts
  { addGuildMemberOptsAccessToken :: T.Text
  , addGuildMemberOptsNickname    :: Maybe T.Text
  , addGuildMemberOptsRoles       :: Maybe [RoleId]
  , addGuildMemberOptsIsMuted     :: Maybe Bool
  , addGuildMemberOptsIsDeafened  :: Maybe Bool
  } deriving (Show, Eq, Ord)

instance ToJSON AddGuildMemberOpts where
  toJSON AddGuildMemberOpts{..} =  object [(name, val) | (name, Just val) <-
                                  [("access_token", toJSON <$> Just addGuildMemberOptsAccessToken ),
                                   ("nick",   toJSON <$> addGuildMemberOptsNickname ),
                                   ("roles",  toJSON <$> addGuildMemberOptsRoles ),
                                   ("mute",   toJSON <$> addGuildMemberOptsIsMuted ),
                                   ("deaf",   toJSON <$> addGuildMemberOptsIsDeafened )]]

data ModifyGuildMemberOpts = ModifyGuildMemberOpts
  { modifyGuildMemberOptsNickname      :: Maybe T.Text
  , modifyGuildMemberOptsRoles         :: Maybe [RoleId]
  , modifyGuildMemberOptsIsMuted       :: Maybe Bool
  , modifyGuildMemberOptsIsDeafened    :: Maybe Bool
  , modifyGuildMemberOptsMoveToChannel :: Maybe ChannelId
  } deriving (Show, Eq, Ord)

instance ToJSON ModifyGuildMemberOpts where
  toJSON ModifyGuildMemberOpts{..} =  object [(name, val) | (name, Just val) <-
                                  [("nick",  toJSON <$> modifyGuildMemberOptsNickname ),
                                   ("roles", toJSON <$> modifyGuildMemberOptsRoles ),
                                   ("mute",  toJSON <$> modifyGuildMemberOptsIsMuted ),
                                   ("deaf",  toJSON <$> modifyGuildMemberOptsIsDeafened ),
                                   ("channel_id", toJSON <$> modifyGuildMemberOptsMoveToChannel)]]
data CreateGuildChannelOpts
  = CreateGuildChannelOptsText {
    createGuildChannelOptsTopic :: Maybe T.Text
  , createGuildChannelOptsUserMessageRateDelay :: Maybe Integer
  , createGuildChannelOptsIsNSFW :: Maybe Bool
  , createGuildChannelOptsCategoryId :: Maybe ChannelId }
  | CreateGuildChannelOptsVoice {
    createGuildChannelOptsBitrate :: Maybe Integer
  , createGuildChannelOptsMaxUsers :: Maybe Integer
  , createGuildChannelOptsCategoryId :: Maybe ChannelId }
  | CreateGuildChannelOptsCategory
  deriving (Show, Eq, Ord)

createChannelOptsToJSON :: T.Text -> [Overwrite] -> CreateGuildChannelOpts -> Value
createChannelOptsToJSON name perms opts = object [(key, val) | (key, Just val) <- optsJSON]
  where
  optsJSON = case opts of
    CreateGuildChannelOptsText{..} ->
                          [("name",                  Just (String name))
                          ,("type",                  Just (Number 0))
                          ,("permission_overwrites", toJSON <$> Just perms)
                          ,("topic",                 toJSON <$> createGuildChannelOptsTopic)
                          ,("rate_limit_per_user",   toJSON <$> createGuildChannelOptsUserMessageRateDelay)
                          ,("nsfw",                  toJSON <$> createGuildChannelOptsIsNSFW)
                          ,("parent_id",             toJSON <$> createGuildChannelOptsCategoryId)]
    CreateGuildChannelOptsVoice{..} ->
                          [("name",                  Just (String name))
                          ,("type",                  Just (Number 2))
                          ,("permission_overwrites", toJSON <$> Just perms)
                          ,("bitrate",               toJSON <$> createGuildChannelOptsBitrate)
                          ,("user_limit",            toJSON <$> createGuildChannelOptsMaxUsers)
                          ,("parent_id",             toJSON <$> createGuildChannelOptsCategoryId)]
    CreateGuildChannelOptsCategory ->
                          [("name",                  Just (String name))
                          ,("type",                  Just (Number 4))
                          ,("permission_overwrites", toJSON <$> Just perms)]


-- | https://discordapp.com/developers/docs/resources/guild#modify-guild
data ModifyGuildOpts = ModifyGuildOpts
  { modifyGuildOptsName         :: Maybe T.Text
  , modifyGuildOptsAFKChannelId :: Maybe ChannelId
  , modifyGuildOptsIcon         :: Maybe T.Text
  , modifyGuildOptsOwnerId      :: Maybe UserId
   -- Region
   -- VerificationLevel
   -- DefaultMessageNotification
   -- ExplicitContentFilter
  } deriving (Show, Eq, Ord)

instance ToJSON ModifyGuildOpts where
  toJSON ModifyGuildOpts{..} =  object [(name, val) | (name, Just val) <-
                                  [("name",            toJSON <$>  modifyGuildOptsName ),
                                   ("afk_channel_id",  toJSON <$>  modifyGuildOptsAFKChannelId ),
                                   ("icon",            toJSON <$>  modifyGuildOptsIcon ),
                                   ("owner_id",        toJSON <$>  modifyGuildOptsOwnerId )] ]

data GuildMembersTiming = GuildMembersTiming
                          { guildMembersTimingLimit :: Maybe Int
                          , guildMembersTimingAfter :: Maybe UserId
                          }

guildMembersTimingToQuery :: GuildMembersTiming -> R.Option 'R.Https
guildMembersTimingToQuery (GuildMembersTiming mLimit mAfter) =
  let limit = case mLimit of
              Nothing -> mempty
              Just lim -> "limit" R.=: lim
      after = case mAfter of
              Nothing -> mempty
              Just aft -> "after" R.=: show aft
  in limit <> after

guildMajorRoute :: GuildRequest a -> String
guildMajorRoute c = case c of
  (CreateGuild _) ->                      "guild "
  (GetGuild g) ->                         "guild " <> show g
  (ModifyGuild g _) ->                    "guild " <> show g
  (DeleteGuild g) ->                      "guild " <> show g
  (GetGuildChannels g) ->            "guild_chan " <> show g
  (CreateGuildChannel g _ _ _) ->    "guild_chan " <> show g
  (ModifyGuildChannelPositions g _) -> "guild_chan " <> show g
  (GetGuildMember g _) ->            "guild_memb " <> show g
  (ListGuildMembers g _) ->         "guild_membs " <> show g
  (AddGuildMember g _ _) ->         "guild_membs " <> show g
  (ModifyGuildMember g _ _) ->      "guild_membs " <> show g
  (ModifyCurrentUserNick g _) ->    "guild_membs " <> show g
  (AddGuildMemberRole g _ _) ->     "guild_membs " <> show g
  (RemoveGuildMemberRole g _ _) ->  "guild_membs " <> show g
  (RemoveGuildMember g _) ->        "guild_membs " <> show g
  (GetGuildBan g _) ->               "guild_bans " <> show g
  (GetGuildBans g) ->                "guild_bans " <> show g
  (CreateGuildBan g _ _) ->           "guild_ban " <> show g
  (RemoveGuildBan g _) ->             "guild_ban " <> show g
  (GetGuildRoles g) ->              "guild_roles " <> show g
  (CreateGuildRole g _) ->          "guild_roles " <> show g
  (ModifyGuildRolePositions g _) -> "guild_roles " <> show g
  (ModifyGuildRole g _ _) ->         "guild_role " <> show g
  (DeleteGuildRole g _ ) ->          "guild_role " <> show g
  (GetGuildPruneCount g _) ->       "guild_prune " <> show g
  (BeginGuildPrune g _) ->          "guild_prune " <> show g
  (GetGuildVoiceRegions g) ->       "guild_voice " <> show g
  (GetGuildInvites g) ->            "guild_invit " <> show g
  (GetGuildIntegrations g) ->       "guild_integ " <> show g
  (CreateGuildIntegration g _ _) -> "guild_integ " <> show g
  (ModifyGuildIntegration g _ _) -> "guild_intgr " <> show g
  (DeleteGuildIntegration g _) ->   "guild_intgr " <> show g
  (SyncGuildIntegration g _) ->      "guild_sync " <> show g
  (GetGuildEmbed g) ->              "guild_embed " <> show g
  (ModifyGuildEmbed g _) ->         "guild_embed " <> show g
  (GetGuildVanityURL g) ->                "guild " <> show g


-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
  where apiVersion = "v6"

guilds :: R.Url 'R.Https
guilds = baseUrl /: "guilds"

guildJsonRequest :: GuildRequest r -> JsonRequest
guildJsonRequest c = case c of
  (CreateGuild opts) ->
      Post (guilds) (pure (R.ReqBodyJson opts)) mempty

  (GetGuild guild) ->
      Get (guilds // guild) mempty

  (ModifyGuild guild patch) ->
      Patch (guilds // guild) (pure (R.ReqBodyJson patch)) mempty

  (DeleteGuild guild) ->
      Delete (guilds // guild) mempty

  (GetGuildChannels guild) ->
      Get (guilds // guild /: "channels") mempty

  (CreateGuildChannel guild name perms patch) ->
      Post (guilds // guild /: "channels")
           (pure (R.ReqBodyJson (createChannelOptsToJSON name perms patch))) mempty

  (ModifyGuildChannelPositions guild newlocs) ->
      let patch = map (\(a, b) -> object [("id", toJSON a)
                                         ,("position", toJSON b)]) newlocs
      in Patch (guilds // guild /: "channels") (pure (R.ReqBodyJson patch)) mempty

  (GetGuildMember guild member) ->
      Get (guilds // guild /: "members" // member) mempty

  (ListGuildMembers guild range) ->
      Get (guilds // guild /: "members") (guildMembersTimingToQuery range)

  (AddGuildMember guild user patch) ->
      Put (guilds // guild /: "members" // user) (R.ReqBodyJson patch) mempty

  (ModifyGuildMember guild member patch) ->
      Patch (guilds // guild /: "members" // member) (pure (R.ReqBodyJson patch)) mempty

  (ModifyCurrentUserNick guild name) ->
      let patch = object ["nick" .= name]
      in Patch (guilds // guild /: "members/@me/nick") (pure (R.ReqBodyJson patch)) mempty

  (AddGuildMemberRole guild user role) ->
      let body = R.ReqBodyJson (object [])
      in Put (guilds // guild /: "members" // user /: "roles" // role) body mempty

  (RemoveGuildMemberRole guild user role) ->
      Delete (guilds // guild /: "members" // user /: "roles" // role) mempty

  (RemoveGuildMember guild user) ->
      Delete (guilds // guild /: "members" // user) mempty

  (GetGuildBan guild user) -> Get (guilds // guild /: "bans" // user) mempty

  (GetGuildBans guild) -> Get (guilds // guild /: "bans") mempty

  (CreateGuildBan guild user patch) ->
      Put (guilds // guild /: "bans" // user) (R.ReqBodyJson patch) mempty

  (RemoveGuildBan guild ban) ->
      Delete (guilds // guild /: "bans" // ban) mempty

  (GetGuildRoles guild) ->
      Get (guilds // guild /: "roles") mempty

  (CreateGuildRole guild patch) ->
      Post (guilds // guild /: "roles") (pure (R.ReqBodyJson patch)) mempty

  (ModifyGuildRolePositions guild patch) ->
      let body = map (\(role, pos) -> object ["id".=role, "position".=pos]) patch
      in Patch (guilds // guild /: "roles") (pure (R.ReqBodyJson body)) mempty

  (ModifyGuildRole guild role patch) ->
      Patch (guilds // guild /: "roles" // role) (pure (R.ReqBodyJson patch)) mempty

  (DeleteGuildRole guild role) ->
      Delete (guilds // guild /: "roles" // role) mempty

  (GetGuildPruneCount guild days) ->
      Get (guilds // guild /: "prune") ("days" R.=: days)

  (BeginGuildPrune guild days) ->
      Post (guilds // guild /: "prune") (pure R.NoReqBody) ("days" R.=: days)

  (GetGuildVoiceRegions guild) ->
      Get (guilds // guild /: "regions") mempty

  (GetGuildInvites guild) ->
      Get (guilds // guild /: "invites") mempty

  (GetGuildIntegrations guild) ->
      Get (guilds // guild /: "integrations") mempty

  (CreateGuildIntegration guild iid opts) ->
      let patch = object [("type" .= createGuildIntegrationOptsType opts) ,("id"   .= iid)]
      in Post (guilds // guild /: "integrations") (pure (R.ReqBodyJson patch)) mempty

  (ModifyGuildIntegration guild iid patch) ->
      let body = pure (R.ReqBodyJson patch)
      in Patch (guilds // guild /: "integrations" // iid) body mempty

  (DeleteGuildIntegration guild integ) ->
      Delete (guilds // guild /: "integrations" // integ) mempty

  (SyncGuildIntegration guild integ) ->
      Post (guilds // guild /: "integrations" // integ) (pure R.NoReqBody) mempty

  (GetGuildEmbed guild) ->
      Get (guilds // guild /: "integrations") mempty

  (ModifyGuildEmbed guild patch) ->
      Patch (guilds // guild /: "embed") (pure (R.ReqBodyJson patch)) mempty

  (GetGuildVanityURL guild) ->
      Get (guilds // guild /: "vanity-url") mempty

guildUpdateCache :: Cache -> GuildRequest a -> Response (GuildRequest a) -> Cache
guildUpdateCache c _ _ = c
