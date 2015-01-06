{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Monoid
import Data.Pool
import Data.Proxy
import Data.Text
import Database.Groundhog
import Database.Groundhog.Sqlite
import Database.Groundhog.Generic
import GHC.Generics
import Models.Channel
import Models.Group
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


-- * Example
type Conn = (MonadBaseControl IO m, MonadIO m) => DbPersist Sqlite (NoLoggingT m) a -> m a

-- API specification
type ChattyApi =
	"groups" :> Get [Group]


chattyApi :: Proxy ChattyApi
chattyApi =  Proxy


server :: Conn -> Server ChattyApi
server conn = getGroupsH conn

getGroupsH :: (MonadBaseControl IO m, MonadIO m) => Conn -> m [Group]
getGroupsH conn =
	do
		conn $ do
			allGroups <- select CondEmpty
			return allGroups
	--return [ Group "Haskell" ]

runTestServer :: Conn -> Port -> IO ()
runTestServer conn port = run port (serve chattyApi $ server conn)


main :: IO ()
main =
	do
		let
			conn :: Conn
			conn m = withSqliteConn "db.sqlite" $ runDbConn $ m
		conn $ do
			runMigration $ do
				migrate (undefined :: Group)
				migrate (undefined :: Channel)

			_ <- insert $ Group "Haskell"
			return ()

		runTestServer conn 8001
