{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{- |
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>

-}

module Discord.Gateway.Types
  ( module GM
  , GatewayInfo
  , url
  , shards
  , session_start_limit
  , SessionStartLimit(..)
  , total
  , remaining
  , max_concurrency
  , reset_after
  , GatewayParams
  , version
  , encoding
  , compress
  , GatewayEncoding(..)
  , GatewayCompression(..)
  , defaultGatewayParams
  )
where

import           Discord.Gateway.Types.Messages as GM
import           Discord.Gateway.Exceptions

import           Control.Exception.Safe
import           Control.Lens
import           Data.Aeson
import           Data.Char                      ( toLower )
import qualified Text.Show

{- | Data constructors to discover Gateway connection info
 -   https://discord.com/developers/docs/topics/gateway#get-gateway-bot
 -}
data GatewayInfo = GWInfo
  { _url                 :: Text              -- ^ The WSS URL that can be used for connecting to the gateway
  , _shards              :: Integer           -- ^ The recommended number of shards to use when connecting
  , _session_start_limit :: SessionStartLimit -- ^ Information on the current session start limit
  } deriving stock    (Generic, Show)
instance FromJSON GatewayInfo where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = map toLower . drop 1
    }

data SessionStartLimit = SessionStartLimit
  { _total           :: Integer -- ^ The total number of session starts the current user is allowed
  , _remaining       :: Integer -- ^ The remaining number of session starts the current user is allowed
  , _reset_after     :: Integer -- ^ The number of milliseconds after which the limit resets
  , _max_concurrency :: Integer -- ^ The number of identify requests allowed per 5 seconds
  } deriving stock    (Generic, Show)
instance FromJSON SessionStartLimit where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = map toLower . drop 1
    }

{-
 - | Data constructors for connecting to the Gateway
 -   https://discord.com/developers/docs/topics/gateway#connecting-to-the-gateway
 -}
data GatewayParams = GWParams
  { _version  :: Integer                  -- ^ Gateway Version to use
  , _encoding :: GatewayEncoding          -- ^ The encoding of received gateway packets
  , _compress :: Maybe GatewayCompression -- ^ The (optional) compression of gateway packets
  } deriving stock    (Generic, Show)

{- | The Discord Gateway supports both JSON and ETF encodings.
 -   We, however, only support JSON at the moment.
 -}
data GatewayEncoding = JSON | ETF deriving stock (Generic, Enum)
instance Show GatewayEncoding where
  show JSON = "json"
  show ETF  = impureThrow UnsupportedEncoding

{- | The Discord Gateway supports ZLIB compression on payloads
 -   Unfortunately, we do not.
 -}
data GatewayCompression = ZLIB_STREAM deriving stock (Generic, Enum)
instance Show GatewayCompression where
  show ZLIB_STREAM = impureThrow UnsupportedCompression

defaultGatewayParams :: GatewayParams
defaultGatewayParams = GWParams 8 JSON Nothing

$(makeLenses ''GatewayInfo)
$(makeLenses ''SessionStartLimit)
$(makeLenses ''GatewayParams)
