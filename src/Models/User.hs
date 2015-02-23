{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.User where

import GHC.Generics
import Data.Aeson
import Database.Groundhog
import Database.Groundhog.Core ()
import Database.Groundhog.TH
import Database.Groundhog.Generic ()

import Models.Group

data User = User
    {
        userName :: String
      , userEmail :: String
    } deriving (Generic, Show)


instance ToJSON User where
  toJSON (User name email) = object [ "name" .= name, "email"  .= email ]

mkPersist defaultCodegenConfig [groundhog|
    - entity: User
      constructors:
        - name: User
          fields:
            - name: userName
              dbName: name
            - name: userEmail
              dbName: email
          uniques:
            - name: NameConstraint
              fields: [userEmail]
|]
