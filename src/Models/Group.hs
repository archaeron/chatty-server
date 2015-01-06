{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Group where

import GHC.Generics
import Data.Aeson
import Database.Groundhog ()
import Database.Groundhog.Core ()
import Database.Groundhog.TH
import Database.Groundhog.Generic ()

data Group =
	Group
		{
			groupName :: String
		} deriving (Generic, Show)

instance FromJSON Group

instance ToJSON Group where
	toJSON (Group name) = object [ "name" .= name ]

mkPersist defaultCodegenConfig [groundhog|
- entity: Group			   # Name of the datatype
  constructors:
    - name: Group
      fields:
      - name: groupName
        dbName: name
      uniques:
        - name: NameConstraint
          fields: [groupName]
|]
