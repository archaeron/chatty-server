{-# LANGUAGE FlexibleContexts #-}

module Helpers.Database where

import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Sqlite
import Data.Proxy

db :: Proxy Sqlite
db = undefined

intToKey :: (PrimitivePersistField (Key a b)) => Int -> Key a b
intToKey p = integralToKey p

integralToKey :: (PrimitivePersistField i, PrimitivePersistField (Key a b)) =>  i -> Key a b
integralToKey = fromPrimitivePersistValue db . toPrimitivePersistValue db
