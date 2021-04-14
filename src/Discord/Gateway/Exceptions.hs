{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}
{- |
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>

-}

module Discord.Gateway.Exceptions
  ( GatewayException(..)
  , GWConnException(..)
  ) where

data GatewayException =
    UnsupportedEncoding
  | UnsupportedCompression
  deriving stock (Show, Eq, Enum)
  deriving anyclass (Exception)

data GWConnException =
    UnexpectedResponse
  | NoResponse
  deriving stock (Show, Eq, Enum)
  deriving anyclass (Exception)
