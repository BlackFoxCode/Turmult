{-# LANGUAGE TemplateHaskell #-}
{- |
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>

-}

module Turmult.Types.Lens where

import           Turmult.Types.Data
import           Control.Lens.TH                ( makeLenses )

$(makeLenses ''Activity)
$(makeLenses ''ActivityAssets)
$(makeLenses ''ActivityEmoji)
$(makeLenses ''ActivityParty)
$(makeLenses ''ActivitySecrets)
$(makeLenses ''ActivityType)
$(makeLenses ''Channel)
$(makeLenses ''ClientStatus)
$(makeLenses ''Emoji)
$(makeLenses ''Guild)
$(makeLenses ''GuildMember)
$(makeLenses ''Invite)
$(makeLenses ''Overwrite)
$(makeLenses ''OverwriteType)
$(makeLenses ''PartySize)
$(makeLenses ''PresenceUpdate)
$(makeLenses ''Role)
$(makeLenses ''RoleTag)
$(makeLenses ''Template)
$(makeLenses ''Timestamps)
$(makeLenses ''User)
$(makeLenses ''UserFlags)
$(makeLenses ''VoiceState)
$(makeLenses ''Webhook)
$(makeLenses ''WebhookType)
$(makeLenses ''WelcomeScreen)
$(makeLenses ''WelcomeScreenChannel)
