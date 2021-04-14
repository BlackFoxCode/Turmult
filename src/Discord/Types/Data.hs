{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{- |
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>

-}

module Discord.Types.Data
  ( Sendable
  , Receivable
  , Snowflake
  , DiscordState(..)
  , Activity(..)
  , ActivityType(..)
  , Timestamps(..)
  , ActivityEmoji(..)
  , ActivityParty(..)
  , PartySize(..)
  , ActivityAssets(..)
  , ActivitySecrets(..)
  , Channel(..)
  , ClientStatus(..)
  , Emoji(..)
  , Guild(..)
  , GuildMember(..)
  , Invite(..)
  , Overwrite(..)
  , OverwriteType(..)
  , PresenceUpdate(..)
  , Role(..)
  , RoleTag(..)
  , Template(..)
  , User(..)
  , UserFlags(..)
  , VoiceState(..)
  , Webhook(..)
  , WebhookType(..)
  , WelcomeScreen(..)
  , WelcomeScreenChannel(..)
  )
where

import           Discord.Types.Classes

import           Data.Aeson                     ( FromJSON
                                                , Object
                                                , Options
                                                , ToJSON
                                                , Value(..)
                                                , camelTo2
                                                , defaultOptions
                                                , fieldLabelModifier
                                                , genericParseJSON
                                                , genericToJSON
                                                , omitNothingFields
                                                , parseJSON
                                                , toJSON
                                                )
import           Data.Bits                      ( testBit )
import           Data.Scientific                ( toBoundedInteger )

-- | Function to provide default JSON conversion options
defaultJSONOptions :: Int -- ^ Number of characters to drop from the lable (e.x. '_ClassNameField' would be 10)
                   -> Options
defaultJSONOptions x = defaultOptions { omitNothingFields = True , fieldLabelModifier = camelTo2 '_' . drop x }


{- | Discord utilizes Twitter's snowflake format for uniquely identifiable
 -  descriptors (IDs). These IDs are guaranteed to be unique across all of
 -  Discord, except in some unique scenarios in which child objects share their
 -  parent's ID.
 -}
type Snowflake = Text

data DiscordState = DiscordState
  { _discordUser :: User
  , _guilds      :: [Guild]
  } deriving stock (Generic, Show)

data Activity = Activity
  { _activityName           :: Text                  -- ^ the activity's name
  , _activityType           :: ActivityType          -- ^ activity type
  , _activityUrl            :: Maybe Text            -- ^ stream url, is validated when type is 1
  , _activityCreatedAt      :: Int                   -- ^ unix timestamp of when the activity was added to the user's session
  , _activityTimestamps     :: Maybe Timestamps      -- ^ timestamps for start and/or end of the game
  , _activityApplicationId  :: Maybe Snowflake       -- ^ application id for the game
  , _activityDetails        :: Maybe Text            -- ^ what the player is currently doing
  , _activityState          :: Maybe Text            -- ^ the user's current party status
  , _activityEmoji          :: Maybe ActivityEmoji   -- ^ the emoji used for a custom status
  , _activityParty          :: Maybe ActivityParty   -- ^ information for the current party of the player
  , _activityAssets         :: Maybe ActivityAssets  -- ^ for the presence and their hover texts
  , _activitySecrets        :: Maybe ActivitySecrets -- ^ for Rich Presence joining and spectating
  , _activityInstance       :: Maybe Bool            -- ^ whether or not the activity is an instanced game session
  , _activityFlags          :: Maybe Int             -- ^ activity flags ORd together, describes what the payload includes
  }
  deriving stock (Generic, Show)
instance FromJSON Activity where
  parseJSON = genericParseJSON $ defaultJSONOptions 9
instance ToJSON Activity where
  toJSON = genericToJSON $ defaultJSONOptions 9
instance Receivable Activity

data ActivityAssets = ActivityAssets
  { _activityAssetsLargeImage :: Maybe Text -- ^ the id for a large asset of the activity, usually a snowflake
  , _activityAssetsLargeText  :: Maybe Text -- ^ text displayed when hovering over the large image of the activity
  , _activityAssetsSmallImage :: Maybe Text -- ^ the id for a small asset of the activity, usually a snowflake
  , _activityAssetsSmallText  :: Maybe Text -- ^ text displayed when hovering over the small image of the activity
  }
  deriving stock (Generic, Show)
instance FromJSON ActivityAssets where
  parseJSON = genericParseJSON $ defaultJSONOptions 15
instance ToJSON ActivityAssets where
  toJSON = genericToJSON $ defaultJSONOptions 15

data ActivityEmoji = ActivityEmoji
  { _activityEmojiName     :: Text            -- ^ the name of the emoji
  , _activityEmojiId       :: Maybe Snowflake -- ^ the id of the emoji
  , _activityEmojiAnimated :: Maybe Bool      -- ^ whether this emoji is animated
  }
  deriving stock (Generic, Show)
instance FromJSON ActivityEmoji where
  parseJSON = genericParseJSON $ defaultJSONOptions 14
instance ToJSON ActivityEmoji where
  toJSON = genericToJSON $ defaultJSONOptions 14

data ActivityParty = ActivityParty
  { _activityPartyId   :: Maybe Text      -- ^ the id of the party
  , _activityPartySize :: Maybe PartySize -- ^ used to show the party's current and maximum size
  }
  deriving stock (Generic, Show)
instance FromJSON ActivityParty where
  parseJSON = genericParseJSON $ defaultJSONOptions 14
instance ToJSON ActivityParty where
  toJSON = genericToJSON $ defaultJSONOptions 14

data ActivitySecrets = ActivitySecrets
  { _activitySecretsJoin     :: Maybe Text -- ^ the secret for joining a party
  , _activitySecretsSpectate :: Maybe Text -- ^ the secret for spectating a game
  , _activitySecretsMatch    :: Maybe Text -- ^ the secret for a specific instanced match
  }
  deriving stock (Generic, Show)
instance FromJSON ActivitySecrets where
  parseJSON = genericParseJSON $ defaultJSONOptions 16
instance ToJSON ActivitySecrets where
  toJSON = genericToJSON $ defaultJSONOptions 16

                               --   Format              | Example
data ActivityType = Game       -- ^ Playing {name}      | "Playing Rocket League"
                  | Streaming  -- ^ Streaming {details} | "Streaming Rocket League"
                  | Listening  -- ^ Listening to {name} | "Listening to Spotify"
                  | Custom     -- ^ {emoji}{name}       | ":smiley: I am cool"
                  | Competing  -- ^ Competing in {name} | "Competing in Arena World Champions"
    deriving stock (Generic, Show)
instance FromJSON ActivityType where
  parseJSON (Number x) = case x of
    0 -> pure Game
    1 -> pure Streaming
    2 -> pure Listening
    3 -> pure Custom
    4 -> pure Competing
    _ -> mzero
  parseJSON _ = mzero
instance ToJSON ActivityType where
  toJSON Game      = Number 0
  toJSON Streaming = Number 1
  toJSON Listening = Number 2
  toJSON Custom    = Number 3
  toJSON Competing = Number 4

data Channel = Channel
  { _channelId                    :: Snowflake         -- ^ the id of this channel
  , _channelType                  :: Int               -- ^ the type of channel
  , _channelGuildId               :: Maybe Snowflake   -- ^ the id of the guild (may be missing for some channel objects received over gateway guild dispatches)
  , _channelPosition              :: Maybe Int         -- ^ sorting position of the channel
  , _channelPermissionOverwrites  :: Maybe [Overwrite] -- ^ explicit permission overwrites for members and roles
  , _channelName                  :: Maybe Text        -- ^ the name of the channel (2-100 characters)
  , _channelTopic                 :: Maybe Text        -- ^ the channel topic (0-1024 characters)
  , _channelNsfw                  :: Maybe Bool        -- ^ whether the channel is nsfw
  , _channelLastMessageId         :: Maybe Snowflake   -- ^ the id of the last message sent in this channel (may not point to an existing or valid message)
  , _channelBitrate               :: Maybe Int         -- ^ the bitrate (in bits) of the voice channel
  , _channelUserLimit             :: Maybe Int         -- ^ the user limit of the voice channel
  , _channelRateLimitPerUser      :: Maybe Int         -- ^ amount of seconds a user has to wait before sending another message (0-21600); bots, as well as users with the permission manage_channelMessages or manage_channelChannel, are unaffected
  , _channelRecipients            :: Maybe [User]      -- ^ the recipients of the DM
  , _channelIcon                  :: Maybe Text        -- ^ icon hash
  , _channelOwnerId               :: Maybe Snowflake   -- ^ id of the DM creator
  , _channelApplicationId         :: Maybe Snowflake   -- ^ application id of the group DM creator if it is bot-created
  , _channelParentId              :: Maybe Snowflake   -- ^ id of the parent category for a channel (each parent category can contain up to 50 channels)
  , _channelLastPinTimestamp      :: Maybe Text        -- ^ when the last pinned message was pinned. This may be null in events such as GUILD_channelCREATE when a message is not pinned.
  , _channelRtcRegion             :: Maybe Text        -- ^ voice region id for the voice channel, automatic when set to null
  , _channelVideoQualityMode      :: Maybe Int         -- ^ the camera video quality mode of the voice channel, 1 when not present
  } deriving stock (Generic, Show)
instance FromJSON Channel where
  parseJSON = genericParseJSON $ defaultJSONOptions 8

data ClientStatus = ClientStatus
  { _clientStatusDesktop :: Maybe Text -- ^ the user's status set for an active desktop (Windows, Linux, Mac) application session
  , _clientStatusMobile  :: Maybe Text -- ^ the user's status set for an active mobile (iOS, Android) application session
  , _clientStatusWeb     :: Maybe Text -- ^ the user's status set for an active web (browser, bot account) application session
  } deriving stock (Generic, Show)
instance FromJSON ClientStatus where
  parseJSON = genericParseJSON $ defaultJSONOptions 13

data Emoji = Emoji
  { _emojiId            :: Maybe Snowflake -- ^ emoji id
  , _emojiName          :: Text            -- ^ emoji name
  , _emojiRoles         :: Maybe [Role]    -- ^ roles allowed to use this emoji
  , _emojiUser          :: Maybe User      -- ^ user that created this emoji
  , _emojiRequireColons :: Maybe Bool      -- ^ whether this emoji must be wrapped in colons
  , _emojiManaged       :: Maybe Bool      -- ^ whether this emoji is managed
  , _emojiAnimated      :: Maybe Bool      -- ^ whether this emoji is animated
  , _emojiAvailable     :: Maybe Bool      -- ^ whether this emoji can be used, may be false due to loss of Server Boosts
  } deriving stock (Generic, Show)
instance FromJSON Emoji where
  parseJSON = genericParseJSON $ defaultJSONOptions 6

data Guild = Guild
  { _guildId                          :: Snowflake        -- ^ guild id
  , _guildName                        :: Text             -- ^ guild name (2-100 characters, excluding trailing and leading whitespace)
  , _guildIcon                        :: Maybe Text       -- ^ icon hash
  , _guildIconHash                    :: Maybe Text       -- ^ icon hash, returned when in the template object
  , _guildSplash                      :: Maybe Text       -- ^ splash hash
  , _guildDiscoverySplash             :: Maybe Text       -- ^ discovery splash hash; only present for guilds with the "DISCOVERABLE" feature
  , _guildOwner                       :: Bool             -- ^ true if the user is the owner of the guild
  , _guildOwnerId                     :: Snowflake        -- ^ id of owner
  , _guildPermissions                 :: Text             -- ^ total permissions for the user in the guild (excludes overrides)
  , _guildRegion                      :: Text             -- ^ voice region id for the guild
  , _guildAfkChannelId                :: Maybe Snowflake  -- ^ id of afk channel
  , _guildAfkTimeout                  :: Int              -- ^ afk timeout in seconds
  , _guildWidgetEnabled               :: Bool             -- ^ true if the server widget is enabled
  , _guildWidgetChannelId             :: Maybe Snowflake  -- ^ the channel id that the widget will generate an invite to, or null if set to no invite
  , _guildVerificationLevel           :: Int              -- ^ verification level required for the guild
  , _guildDefaultMessageNotifications :: Int              -- ^ default message notifications level
  , _guildExplicitContentFilter       :: Int              -- ^ explicit content filter level
  , _guildRoles                       :: [Role]           -- ^ roles in the guild
  , _guildEmojis                      :: [Emoji]          -- ^ custom guild emojis
  , _guildFeatures                    :: [Text]           -- ^ enabled guild features
  , _guildMfaLevel                    :: Int              -- ^ required MFA level for the guild
  , _guildApplicationId               :: Maybe Snowflake  -- ^ application id of the guild creator if it is bot-created
  , _guildSystemChannelId             :: Maybe Snowflake  -- ^ the id of the channel where guild notices such as welcome messages and boost events are posted
  , _guildSystemChannelFlags          :: Int              -- ^ system channel flags
  , _guildRulesChannelId              :: Maybe Snowflake  -- ^ the id of the channel where Community guilds can display rules and/or guidelines
  , _guildJoinedAt                    :: Text             -- ^ when this guild was joined at
  , _guildLarge                       :: Bool             -- ^ true if this is considered a large guild
  , _guildUnavailable                 :: Bool             -- ^ true if this guild is unavailable due to an outage
  , _guildMemberCount                 :: Int              -- ^ total number of members in this guild
  , _guildVoiceStates                 :: [VoiceState]     -- ^ states of members currently in voice channels; lacks the guild_guildId key
  , _guildMembers                     :: [User]           -- ^ users in the guild
  , _guildChannels                    :: [Channel]        -- ^ channels in the guild
  , _guildPresences                   :: [PresenceUpdate] -- ^ presences of the members in the guild, will only include non-offline members if the size is greater than large threshold
  , _guildMaxPresences                :: Maybe Int        -- ^ the maximum number of presences for the guild (the default value, currently 25000, is in effect when null is returned)
  , _guildMaxMembers                  :: Int              -- ^ the maximum number of members for the guild
  , _guildVanityUrlCode               :: Maybe Text       -- ^ the vanity url code for the guild
  , _guildDescription                 :: Maybe Text       -- ^ the description for the guild, if the guild is discoverable
  , _guildBanner                      :: Maybe Text       -- ^ banner hash
  , _guildPremiumTier                 :: Int              -- ^ premium tier (Server Boost level)
  , _guildPremiumSubscriptionCount    :: Int              -- ^ the number of boosts this guild currently has
  , _guildPreferredLocale             :: Text             -- ^ the preferred locale of a Community guild; used in server discovery and notices from Discord; defaults to "en-US"
  , _guildPublicUpdatesChannelId      :: Maybe Snowflake  -- ^ the id of the channel where admins and moderators of Community guilds receive notices from Discord
  , _guildMaxVideoChannelUsers        :: Int              -- ^ the maximum amount of users in a video channel
  , _guildApproximateMemberCount      :: Int              -- ^ approximate number of members in this guild, returned from the GET /guilds/<id> endpoint when with_guildCounts is true
  , _guildApproximatePresenceCount    :: Int              -- ^ approximate number of non-offline members in this guild, returned from the GET /guilds/<id> endpoint when with_guildCounts is true
  , _guildWelcomeScreen               :: WelcomeScreen    -- ^ the welcome screen of a Community guild, shown to new members, returned when in the invite object
  } deriving stock (Generic, Show)
instance FromJSON Guild where
  parseJSON = genericParseJSON $ defaultJSONOptions 6

data GuildMember = GuildMember
  { _guildMemberUser         :: Maybe User  -- ^ the user this guild member represents
  , _guildMemberNick         :: Maybe Text  -- ^ this users guild nickname
  , _guildMemberRoles        :: [Snowflake] -- ^ array of role object ids
  , _guildMemberJoinedAt     :: Text        -- ^ when the user joined the guild
  , _guildMemberPremiumSince :: Maybe Text  -- ^ when the user started boosting the guild
  , _guildMemberDeaf         :: Bool        -- ^ whether the user is deafened in voice channels
  , _guildMemberMute         :: Bool        -- ^ whether the user is muted in voice channels
  , _guildMemberPending      :: Maybe Bool  -- ^ whether the user has not yet passed the guild's Membership Screening requirements
  , _guildMemberPermissions  :: Maybe Text  -- ^ total permissions of the member in the channel, including overrides, returned when in the interaction object
  } deriving stock (Generic, Show)
instance FromJSON GuildMember where
  parseJSON = genericParseJSON $ defaultJSONOptions 12

data Invite = Invite
  { _inviteCode                     :: Text         -- ^ the invite code (unique ID)
  , _inviteGuild                    :: Maybe Guild  -- ^ the guild this invite is for
  , _inviteChannel                  :: Channel      -- ^ the channel this invite is for
  , _inviteInviter                  :: Maybe User   -- ^ the user who created the invite
  , _inviteTargetType               :: Maybe Int    -- ^ the type of target for this voice channel invite
  , _inviteTargetUser               :: Maybe User   -- ^ the user whose stream to display for this voice channel stream invite
  , _inviteTargetApplication        :: Maybe Object -- ^ the embedded application to open for this voice channel embedded application invite
  , _inviteApproximatePresenceCount :: Maybe Int    -- ^ approximate count of online members, returned from the GET /invites/<code> endpoint when withCounts is true
  , _inviteApproximateMemberCount   :: Maybe Int    -- ^ approximate count of total members, returned from the GET /invites/<code> endpoint when withCounts is true
  } deriving stock (Generic, Show)
instance FromJSON Invite where
  parseJSON = genericParseJSON $ defaultJSONOptions 10

data Overwrite = Overwrite
  { _overwriteId    :: Snowflake     -- ^ role or user id
  , _overwriteType  :: OverwriteType -- ^ either 0 (role) or 1 (member)
  , _overwriteAllow :: Text          -- ^ permission bit set
  , _overwriteDeny  :: Text          -- ^ permission bit set
  } deriving stock (Generic, Show)
instance FromJSON Overwrite where
  parseJSON = genericParseJSON $ defaultJSONOptions 10

data OverwriteType = ForRole | ForMember deriving stock (Generic, Show, Enum)
instance FromJSON OverwriteType where
  parseJSON (Number 0) = pure ForRole
  parseJSON (Number 1) = pure ForMember
  parseJSON _          = mzero

data PartySize = PartySize
  { _partySizeSizeCurrent :: Int -- ^ current size
  , _partySizeSizeMax     :: Int -- ^ max size
  }
  deriving stock (Generic, Show)
instance FromJSON PartySize where
  parseJSON = genericParseJSON $ defaultJSONOptions 10
instance ToJSON PartySize where
  toJSON = genericToJSON $ defaultJSONOptions 10

data PresenceUpdate = PresenceUpdate
  { _presenceUpdateUser         :: User         -- ^ the user presence is being updated for
  , _presenceUpdateGuildId      :: Snowflake    -- ^ id of the guild
  , _presenceUpdateStatus       :: Text         -- ^ either "idle", "dnd", "online", or "offline"
  , _presenceUpdateActivities   :: [Activity]   -- ^ user's current activities
  , _presenceUpdateClientStatus :: ClientStatus -- ^ user's platform-dependent status
  } deriving stock (Generic, Show)
instance FromJSON PresenceUpdate where
  parseJSON = genericParseJSON $ defaultJSONOptions 15

data Role = Role
  { _roleId          :: Snowflake -- ^ role id
  , _roleName        :: Text      -- ^ role name
  , _roleColor       :: Int       -- ^ Int representation of hexadecimal color code
  , _roleHoist       :: Bool      -- ^ if this role is pinned in the user listing
  , _rolePosition    :: Int       -- ^ position of this role
  , _rolePermissions :: Text      -- ^ permission bit set
  , _roleManaged     :: Bool      -- ^ whether this role is managed by an integration
  , _roleMentionable :: Bool      -- ^ whether this role is mentionable
  , _roleTags        :: [RoleTag] -- ^ the tags this role has
  } deriving stock (Generic, Show)
instance FromJSON Role where
  parseJSON = genericParseJSON $ defaultJSONOptions 5

data RoleTag = RoleTag
  { _roleTagBotId :: Maybe Snowflake -- ^ the id of the bot this role belongs to
  , _roleTagIntegrationId :: Maybe Snowflake -- ^ the id of the integration this role belongs to
  , _roleTagPremiumSubscriber :: Maybe Bool -- ^ whether this is the guild's premium subscriber role
  } deriving stock (Generic, Show)
instance FromJSON RoleTag where
  parseJSON = genericParseJSON $ defaultJSONOptions 8

data Template = Template
  { _templateCode                  :: Text       -- ^ the template code (unique ID)
  , _templateName                  :: Text       -- ^ template name
  , _templateDescription           :: Maybe Text -- ^ the description for the template
  , _templateUsageCount            :: Int        -- ^ number of times this template has been used
  , _templateCreatorId             :: Snowflake  -- ^ the ID of the user who created the template
  , _templateCreator               :: User       -- ^ the user who created the template
  , _templateCreatedAt             :: Text       -- ^ when this template was created
  , _templateUpdatedAt             :: Text       -- ^ when this template was last synced to the source guild
  , _templateSourceGuildId         :: Snowflake  -- ^ the ID of the guild this template is based on
  , _templateSerializedSourceGuild :: Guild      -- ^ the guild snapshot this template contains
  , _templateIsDirty               :: Maybe Bool -- ^ whether the template has unsynced changes
  } deriving stock (Generic, Show)
instance FromJSON Template where
  parseJSON = genericParseJSON $ defaultJSONOptions 9

data Timestamps = Timestamps
  { _timestampsStart :: Maybe Int -- ^ unix time (in milliseconds) of when the activity started
  , _timestampsEnd   :: Maybe Int -- ^ unix time (in milliseconds) of when the activity ends
  } deriving stock (Generic, Show)
instance FromJSON Timestamps where
  parseJSON = genericParseJSON $ defaultJSONOptions 11
instance ToJSON Timestamps where
  toJSON = genericToJSON $ defaultJSONOptions 11

data User = User
  { _userId            :: Text       -- ^ the user's id
  , _userUsername      :: Text       -- ^ the user's username, not unique across the platform
  , _userDiscriminator :: Text       -- ^ the user's 4-digit discord-tag
  , _userAvatar        :: Maybe Text -- ^ the user's avatar hash
  , _userBot           :: Bool       -- ^ whether the user belongs to an OAuth2 application
  , _userSystem        :: Bool       -- ^ whether the user is an Official Discord System user (part of the urgent message system)
  , _userMfaEnabled    :: Bool       -- ^ whether the user has two factor enabled on their account
  , _userLocale        :: Text       -- ^ the user's chosen language option
  , _userVerified      :: Bool       -- ^ whether the email on this account has been verified
  , _userEmail         :: Text       -- ^ the user's email
  , _userFlags         :: UserFlags  -- ^ the flags on a user's account
  , _userPremiumType   :: Int        -- ^ the type of Nitro subscription on a user's account
  , _userPublicFlags   :: Int        -- ^ the public flags on a user's account
  } deriving stock (Generic, Show)
instance FromJSON User where
  parseJSON = genericParseJSON $ defaultJSONOptions 5

data UserFlags = UserFlags
  { _userFlagsDiscordEmployee           :: Bool
  , _userFlagsPartneredServerOwner      :: Bool
  , _userFlagsHypeSquadEvents           :: Bool
  , _userFlagsBugHunterLevelOne         :: Bool
  , _userFlagsHouseBravery              :: Bool
  , _userFlagsHouseBrilliance           :: Bool
  , _userFlagsHouseBalance              :: Bool
  , _userFlagsEarlySupporter            :: Bool
  , _userFlagsTeamUser                  :: Bool
  , _userFlagsBugHunterLevelTwo         :: Bool
  , _userFlagsVerifiedBot               :: Bool
  , _userFlagsEarlyVerifiedBotDeveloper :: Bool
  } deriving stock (Generic, Show)
instance FromJSON UserFlags where
  parseJSON (Number bits) = do
    let mask = fromMaybe 0 (toBoundedInteger bits :: Maybe Int)
    pure
      (UserFlags (mask `testBit` 0)
                 (mask `testBit` 1)
                 (mask `testBit` 2)
                 (mask `testBit` 3)
                 (mask `testBit` 6)
                 (mask `testBit` 7)
                 (mask `testBit` 8)
                 (mask `testBit` 9)
                 (mask `testBit` 10)
                 (mask `testBit` 14)
                 (mask `testBit` 16)
                 (mask `testBit` 17)
      )
  parseJSON _ = mzero

data VoiceState = VoiceState
  { _voiceStateGuildId                 :: Maybe Snowflake     -- ^ the guild id this voice state is for
  , _voiceStateChannelId               :: Maybe Snowflake     -- ^ the channel id this user is connected to
  , _voiceStateUserId                  :: Snowflake           -- ^ the user id this voice state is for
  , _voiceStateMember                  :: Maybe [GuildMember] -- ^ the guild member this voice state is for
  , _voiceStateSessionId               :: Text                -- ^ the session id for this voice state
  , _voiceStateDeaf                    :: Bool                -- ^ whether this user is deafened by the server
  , _voiceStateMute                    :: Bool                -- ^ whether this user is muted by the server
  , _voiceStateSelfDeaf                :: Bool                -- ^ whether this user is locally deafened
  , _voiceStateSelfMute                :: Bool                -- ^ whether this user is locally muted
  , _voiceStateSelfStream              :: Maybe Bool          -- ^ whether this user is streaming using "Go Live"
  , _voiceStateSelfVideo               :: Bool                -- ^ whether this user's camera is enabled
  , _voiceStateSuppress                :: Bool                -- ^ whether this user is muted by the current user
  , _voiceStateRequestToSpeakTimestamp :: Maybe Text          -- ^ the time at which the user requested to speak
  } deriving stock (Generic, Show)
instance FromJSON VoiceState where
  parseJSON = genericParseJSON $ defaultJSONOptions 11

data Webhook = Webhook
  { _webhookId            :: Snowflake       -- ^ the id of the webhook
  , _webhookType          :: WebhookType     -- ^ the type of the webhook
  , _webhookGuildId       :: Maybe Snowflake -- ^ the guild id this webhook is for
  , _webhookChannelId     :: Snowflake       -- ^ the channel id this webhook is for
  , _webhookUser          :: Maybe User      -- ^ the user this webhook was created by (not returned when getting a webhook with its token)
  , _webhookName          :: Maybe Text      -- ^ the default name of the webhook
  , _webhookAvatar        :: Maybe Text      -- ^ the default user avatar hash of the webhook
  , _webhookToken         :: Maybe Text      -- ^ the secure token of the webhook (returned for Incoming Webhooks)
  , _webhookApplicationId :: Maybe Snowflake -- ^ the bot/OAuth2 application that created this webhook
  , _webhookSourceGuild   :: Maybe Guild     -- ^ the guild of the channel that this webhook is following (returned for Channel Follower Webhooks)
  , _webhookSourceChannel :: Maybe Channel   -- ^ the channel that this webhook is following (returned for Channel Follower Webhooks)
  , _webhookUrl           :: Maybe Text      -- ^ the url used for executing the webhook (returned by the webhooks OAuth2 flow)
  } deriving stock (Generic, Show)
instance FromJSON Webhook where
  parseJSON = genericParseJSON $ defaultJSONOptions 8

data WebhookType = Incoming | ChannelFollower deriving stock (Generic, Show, Enum)
instance FromJSON WebhookType where
  parseJSON (Number 0) = pure Incoming
  parseJSON (Number 1) = pure ChannelFollower
  parseJSON _          = mzero

data WelcomeScreen = WelcomeScreen
  { _welcomeScreenDescription     :: Maybe Text             -- ^ the server description shown in the welcome screen
  , _welcomeScreenWelcomeChannels :: [WelcomeScreenChannel] -- ^ the channels shown in the welcome screen, up to 5
  } deriving stock (Generic, Show)
instance FromJSON WelcomeScreen where
  parseJSON = genericParseJSON $ defaultJSONOptions 14

data WelcomeScreenChannel = WelcomeScreenChannel
  { _welcomeScreenChannelChannelId   :: Snowflake       -- ^ the channel's id
  , _welcomeScreenChannelDescription :: Text            -- ^ the description shown for the channel
  , _welcomeScreenChannelEmojiId     :: Maybe Snowflake -- ^ the emoji id, if the emoji is custom
  , _welcomeScreenChannelEmojiName   :: Maybe Text      -- ^ the emoji name if custom, the unicode character if standard, or null if no emoji is set
  } deriving stock (Generic, Show)
instance FromJSON WelcomeScreenChannel where
  parseJSON = genericParseJSON $ defaultJSONOptions 21
