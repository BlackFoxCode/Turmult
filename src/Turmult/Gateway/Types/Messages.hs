{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}
{- |
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>

-}

module Turmult.Gateway.Types.Messages
  ( -- | Gateway Commands
    Payload(..)
  , payload_op
  , payload_d
  , payload_s
  , payload_t
  , Identify(..)
  , Snowflake
  , Heartbeat
  , Presence(..)
  , VoiceStateUpdate(..)
  , Resume(..)
  , Reconnect
  , RequestGuildMembers(..)
  , InvalidSession
  , Hello(..)
  , Properties(..)
  , Shard(..)
  , StatusType(..)
  , GatewayIntents(..)
  , Ready(..)
  ) where

import           Turmult.Types
import           Turmult.Gateway.Exceptions     ( GatewayException(..) )

import           Control.Exception.Safe         ( impureThrow )
import           Control.Lens
import           Data.Aeson
import           Data.Bits                      ( shift, testBit )
import Data.Default ( Default, def )
import           Data.Maybe                     ( fromJust )
import           Data.Scientific ( toBoundedInteger )
import           Network.WebSockets

myDefaultOptions :: Options
myDefaultOptions = defaultOptions { omitNothingFields = True }

{- | Payload format for all messages sent/received over Gateway socket
 -   https://discord.com/developers/docs/topics/gateway#payloads
 -}
data Payload = Payload
  { _payload_op :: Integer         -- ^ opcode for the payload
  , _payload_d  :: Maybe Value     -- ^ event data
  , _payload_s  :: Maybe Integer   -- ^ sequence number, used for resuming sessions and heartbeats
  , _payload_t  :: Maybe Text      -- ^ the event name for this payload
  }
  deriving stock (Generic, Show)
instance FromJSON Payload where
  parseJSON = withObject "payload" $ \o -> Payload <$> o .: "op" <*> o .:? "d" <*> o .:? "s" <*> o .:? "t"
instance ToJSON Payload where
  toJSON = genericToJSON myDefaultOptions { fieldLabelModifier = drop 9 }
instance WebSocketsData Payload where
  fromDataMessage (Text msg _) = fromJust $ decode msg
  fromDataMessage (Binary _  ) = impureThrow UnsupportedEncoding
  fromLazyByteString = fromJust . decode
  toLazyByteString   = encode
$(makeLenses ''Payload)

{- | Opcode 0 Dispatch: Receive - An event was dispatched.
 - This is encoded as one of the event payloads defined further down
 -}

-- | Opcode 1 Heartbeat: Send/Receive - Fired periodically by the client to keep the connection alive.
type Heartbeat = Maybe Int

-- | Opcode 2 Identify: Send - Starts a new session during the initial handshake.
data Identify = Identify
  { _identify_token               :: Text            -- ^ authentication token
  , _identify_properties          :: Properties      -- ^ connection properties
  , _identify_compress            :: Maybe Bool      -- ^ whether this connection supports compression of packets - Default: False
  , _identify_large_threshold     :: Maybe Int       -- ^ value between 50 and 250, total number of members where the gateway will stop sending offline members in the guild member list
  , _identify_shard               :: Maybe Shard     -- ^ used for Guild Sharding
  , _identify_presence            :: Maybe Presence  -- ^ presence structure for initial presence information
  , _identify_guild_subscriptions :: Maybe Bool      -- ^ enables dispatching of guild subscription events (presence and typing events)
  , _identify_intents             :: GatewayIntents  -- ^ the Gateway Intents you wish to receive
  }
  deriving stock (Generic, Show)
instance FromJSON Identify where
  parseJSON = genericParseJSON myDefaultOptions { fieldLabelModifier = drop 10 }
instance ToJSON Identify where
  toJSON = genericToJSON myDefaultOptions { fieldLabelModifier = drop 10, omitNothingFields = True }

-- | Opcode 3 Presence Update: Send - Update the client's presence.
data Presence = Presence
  { _presence_since      :: Maybe Int  -- ^ unix time (in milliseconds) of when the client went idle, or null if the client is not idle
  , _presence_activities :: [Activity] -- ^ null, or the user's activities
  , _presence_status     :: StatusType -- ^ the user's new status
  , _presence_afk        :: Bool       -- ^ whether or not the client is afk
  }
  deriving stock (Generic, Show)
instance FromJSON Presence where
  parseJSON = genericParseJSON myDefaultOptions { fieldLabelModifier = drop 9 }
instance ToJSON Presence where
  toJSON = genericToJSON myDefaultOptions { fieldLabelModifier = drop 9 }

-- | Opcode 4 Voice State Update: Send - Used to join/leave or move between voice channels.
data VoiceStateUpdate = VoiceStateUpdate
  { _voice_state_update_token    :: Text      -- ^ voice connection token
  , _voice_state_update_guild_id :: Snowflake -- ^ the guild this voice server update is for
  , _voice_state_update_endpoint :: Text      -- ^ the voice server host
  }
  deriving stock (Generic, Show)
instance FromJSON VoiceStateUpdate where
  parseJSON = genericParseJSON myDefaultOptions { fieldLabelModifier = drop 20 }
instance ToJSON VoiceStateUpdate where
  toJSON = genericToJSON myDefaultOptions { fieldLabelModifier = drop 20 }

-- | Opcode 6 Resume: Send - Resume a previous session that was disconnected.
data Resume = Resume
  { _resume_token      :: Text -- ^ session token
  , _resume_session_id :: Text -- ^ session id
  , _resume_seq        :: Int  -- ^ last sequence number received
  }
  deriving stock (Generic, Show)
instance FromJSON Resume where
  parseJSON = genericParseJSON myDefaultOptions { fieldLabelModifier = drop 8 }
instance ToJSON Resume where
  toJSON = genericToJSON myDefaultOptions { fieldLabelModifier = drop 8 }

{- | Opcode 7 Reconnect: Receive - You should attempt to reconnect and resume immediately.
 -   indicates whether the session may be resumable.
 -}
type Reconnect = Bool

-- | Opcode 8 Request Guild Members: Send - Request information about offline guild members in a large guild.
data RequestGuildMembers = RequestGuildMembers
  { _request_guild_members_guild_id  :: Snowflake         -- ^ id of the guild to get members for
  , _request_guild_members_query     :: Maybe Text        -- ^ Text that username starts with, or an empty Text to return all members
  , _request_guild_members_limit     :: Int               -- ^ maximum number of members to send matching the query; a limit of 0 can be used with an empty Text query to return all members
  , _request_guild_members_presences :: Maybe Bool        -- ^ used to specify if we want the presences of the matched members
  , _request_guild_members_user_ids  :: Maybe [Snowflake] -- ^ used to specify which users you wish to fetch
  , _request_guild_members_nonce     :: Maybe Text        -- ^ nonce to identify the Guild Members Chunk response
  }
  deriving stock (Generic, Show)
instance FromJSON RequestGuildMembers where
  parseJSON = genericParseJSON myDefaultOptions { fieldLabelModifier = drop 23 }
instance ToJSON RequestGuildMembers where
  toJSON = genericToJSON myDefaultOptions { fieldLabelModifier = drop 23 }

{- | Opcode 9 Invalid Session: Receive - The session has been invalidated. You should reconnect and identify/resume accordingly.
 -   indicates whether the session may be resumable.
 -}
type InvalidSession = Bool

-- | Opcode 10 Hello: Receive - Sent immediately after connecting, contains the heartbeat_interval to use.
newtype Hello = Hello
    { _hello_heartbeat_interval :: Int -- ^ the interval (in milliseconds) the client should heartbeat with
    } deriving stock (Generic, Show)
instance FromJSON Hello where
  parseJSON = genericParseJSON myDefaultOptions { fieldLabelModifier = drop 7 }
instance ToJSON Hello where
  toJSON = genericToJSON myDefaultOptions { fieldLabelModifier = drop 7 }

-- Opcode 11 Heartbeat ACK: Receive - Sent in response to receiving a heartbeat to acknowledge that it has been received.
-- This Opcode actually doesn't have a data section. Will need to be handled separately.

-- | Connection Properties
data Properties = Properties
  { _properties_os      :: Text -- ^ your operating system
  , _properties_browser :: Text -- ^ your library name
  , _properties_device  :: Text -- ^ your library name
  }
  deriving stock (Generic, Show)
instance FromJSON Properties where
  parseJSON = genericParseJSON myDefaultOptions { fieldLabelModifier = drop 12 }
instance ToJSON Properties where
  toJSON = genericToJSON myDefaultOptions { fieldLabelModifier = drop 12 }

-- | Shard Identifier
data Shard = Shard
  { _shard_id  :: Int -- ^ Shard ID
  , _shard_num :: Int -- ^ Number of shards
  }
  deriving stock (Generic, Show)
instance FromJSON Shard where
  parseJSON = genericParseJSON myDefaultOptions { fieldLabelModifier = drop 6 }
instance ToJSON Shard where
  toJSON = genericToJSON myDefaultOptions { fieldLabelModifier = drop 6 }

data StatusType = Online    -- ^ Online
                | DND       -- ^ Do Not Disturb
                | Idle      -- ^ AFK
                | Invisible -- ^ Invisible and shown as offline
                | Offline   -- ^ Offline
    deriving stock (Generic, Show)
instance FromJSON StatusType where
  parseJSON (Number x) = case x of
    0 -> pure Online
    1 -> pure DND
    2 -> pure Idle
    3 -> pure Invisible
    4 -> pure Offline
    _ -> mzero
  parseJSON _ = mzero
instance ToJSON StatusType where
  toJSON Online    = Number 0
  toJSON DND       = Number 1
  toJSON Idle      = Number 2
  toJSON Invisible = Number 3
  toJSON Offline   = Number 4

-- | Bit shifting object representing gateway intents that can be subscribed to
data GatewayIntents = Intents
  { _gateway_intents_guilds                   :: Bool
  , _gateway_intents_guild_members            :: Bool
  , _gateway_intents_guild_bans               :: Bool
  , _gateway_intents_guild_emojis             :: Bool
  , _gateway_intents_guild_integrations       :: Bool
  , _gateway_intents_guild_webhooks           :: Bool
  , _gateway_intents_guild_invites            :: Bool
  , _gateway_intents_guild_voice_status       :: Bool
  , _gateway_intents_guild_presences          :: Bool
  , _gateway_intents_guild_messages           :: Bool
  , _gateway_intents_guild_message_reactions  :: Bool
  , _gateway_intents_guild_message_typing     :: Bool
  , _gateway_intents_direct_messages          :: Bool
  , _gateway_intents_direct_message_reactions :: Bool
  , _gateway_intents_direct_message_typing    :: Bool
  }
  deriving stock (Generic, Show)
instance ToJSON GatewayIntents where
  toJSON intent = toJSON
    (sum
      [ if _gateway_intents_guilds intent
        then shift 1 0 -- 1
        else 0
      , if _gateway_intents_guild_members intent
        then shift 1 1 -- 2
        else 0
      , if _gateway_intents_guild_bans intent
        then shift 1 2 -- 4
        else 0
      , if _gateway_intents_guild_emojis intent
        then shift 1 3 -- 8
        else 0
      , if _gateway_intents_guild_integrations intent
        then shift 1 4 -- 16
        else 0
      , if _gateway_intents_guild_webhooks intent
        then shift 1 5 -- 32
        else 0
      , if _gateway_intents_guild_invites intent
        then shift 1 6 -- 64
        else 0
      , if _gateway_intents_guild_voice_status intent
        then shift 1 7 -- 128
        else 0
      , if _gateway_intents_guild_presences intent
        then shift 1 8 -- 256
        else 0
      , if _gateway_intents_guild_messages intent
        then shift 1 9 -- 512
        else 0
      , if _gateway_intents_guild_message_reactions intent
        then shift 1 10 -- 1024
        else 0
      , if _gateway_intents_guild_message_typing intent
        then shift 1 11 -- 2048
        else 0
      , if _gateway_intents_direct_messages intent
        then shift 1 12 -- 4096
        else 0
      , if _gateway_intents_direct_message_reactions intent
        then shift 1 13 -- 8192
        else 0
      , if _gateway_intents_direct_message_typing intent
        then shift 1 14 -- 16384
        else 0
      ] :: Int
    )
instance FromJSON GatewayIntents where
  parseJSON (Number bits) = do
    let mask = fromMaybe 0 (toBoundedInteger bits :: Maybe Int)
    pure
      (Intents (mask `testBit` 0)
               (mask `testBit` 1)
               (mask `testBit` 2)
               (mask `testBit` 3)
               (mask `testBit` 4)
               (mask `testBit` 5)
               (mask `testBit` 6)
               (mask `testBit` 7)
               (mask `testBit` 8)
               (mask `testBit` 9)
               (mask `testBit` 10)
               (mask `testBit` 11)
               (mask `testBit` 12)
               (mask `testBit` 13)
               (mask `testBit` 14)
      )
  parseJSON _ = mzero
instance Default GatewayIntents where
    def = Intents { _gateway_intents_guilds                   = False
                  , _gateway_intents_guild_members            = False
                  , _gateway_intents_guild_bans               = False
                  , _gateway_intents_guild_emojis             = False
                  , _gateway_intents_guild_integrations       = False
                  , _gateway_intents_guild_webhooks           = False
                  , _gateway_intents_guild_invites            = False
                  , _gateway_intents_guild_voice_status       = False
                  , _gateway_intents_guild_presences          = False
                  , _gateway_intents_guild_messages           = True
                  , _gateway_intents_guild_message_reactions  = False
                  , _gateway_intents_guild_message_typing     = False
                  , _gateway_intents_direct_messages          = False
                  , _gateway_intents_direct_message_reactions = False
                  , _gateway_intents_direct_message_typing    = False
                  }

data Ready = Ready
  { _ready_v                :: Int
  , _ready_user             :: User
  , _ready_private_channels :: [Channel]
  , _ready_guilds           :: [Guild]
  , _ready_session_id       :: Text
  , _ready_shard            :: Maybe Shard
  , _ready_application      :: Object
  }
  deriving stock (Generic, Show)
instance FromJSON Ready where
  parseJSON = genericParseJSON myDefaultOptions { fieldLabelModifier = drop 7 }
