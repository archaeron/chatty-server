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
import Data.Proxy
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Sqlite
import Models.Channel
import Models.Group
import Models.User
import Network.Wai.Handler.Warp
import Servant
import Helpers.Database

-- * Example
type Conn = (MonadBaseControl IO m, MonadIO m) => DbPersist Sqlite (NoLoggingT m) a -> m a

-- API specification
type ChattyApi =
	"groups" :> Get [Group]
	:<|> "group" :> Capture "groupId" Int :> Get (Maybe Group)
	:<|> "channels" :> Get [Channel]
	:<|> "channel" :> Capture "channelId" Int :> Get (Maybe Channel)


chattyApi :: Proxy ChattyApi
chattyApi =  Proxy

server :: Conn -> Server ChattyApi
server conn =
	getGroupsH conn
	:<|> getGroupH conn
	:<|> getChannelsH conn
	:<|> getChannelH conn

getGroupsH :: (MonadBaseControl IO m, MonadIO m) => Conn -> m [Group]
getGroupsH conn = conn $ select CondEmpty

getGroupH :: (MonadBaseControl IO m, MonadIO m) => Conn -> Int -> m (Maybe Group)
getGroupH conn groupId = conn $ get (intToKey groupId)

getChannelsH :: (MonadBaseControl IO m, MonadIO m) => Conn -> m [Channel]
getChannelsH conn = conn $ select CondEmpty

getChannelH :: (MonadBaseControl IO m, MonadIO m) => Conn -> Int -> m (Maybe Channel)
getChannelH conn channelId = conn $ get (intToKey channelId)

runChattyServer :: Conn -> Port -> IO ()
runChattyServer conn port = run port (serve chattyApi $ server conn)

testData conn =
	conn $ do
		runMigration $ do
			migrate (undefined :: Group)
			migrate (undefined :: Channel)
			migrate (undefined :: User)

		haskellKey <- insert $ Group "Haskell"
		_ <- insert $ Group "Idris"
		_ <- insert $ Group "Purescript"
		_ <- insert $ Group "F#"
		_ <- insert $ Group "Elm"
		_ <- insert $ Channel "Servant" haskellKey
		_ <- insert $ Channel "Groundhog" haskellKey

		_ <- insert $ User "Frank" "frank@fake.com"
		_ <- insert $ User "Hank" "hank@fake.com"
		return ()


main :: IO ()
main =
	do
		let
			conn :: Conn
			conn m = withSqliteConn "db.sqlite" $ runDbConn m

		-- testData conn

		runChattyServer conn 8001
