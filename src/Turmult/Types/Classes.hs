{- |
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>

-}

module Turmult.Types.Classes
    ( Sendable
    , Receivable
    ) where

import Data.Aeson.Types (ToJSON, FromJSON)

class (ToJSON a)   => Sendable   a
class (FromJSON a) => Receivable a
