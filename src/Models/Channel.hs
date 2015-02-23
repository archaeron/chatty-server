{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Channel where

import GHC.Generics
import Data.Aeson
import Database.Groundhog
import Database.Groundhog.Core ()
import Database.Groundhog.TH
import Database.Groundhog.Generic ()

import Models.Group

data Channel =
    Channel
        {
            channelName :: String,
            channelGroup :: DefaultKey Group
        } deriving (Generic, Show)

instance ToJSON Channel where
    toJSON (Channel name group) = object ["name" .= name, "id" .= show group]

mkPersist defaultCodegenConfig [groundhog|
- entity: Channel
  constructors:
    - name: Channel
      fields:
        - name: channelName
          dbName: name
  uniques:
    - name: NameConstraint
      fields: [channelName]
|]
