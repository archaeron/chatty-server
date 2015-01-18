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
import Network.Wai.Handler.Warp
import Servant


-- * Example
type Conn = (MonadBaseControl IO m, MonadIO m) => DbPersist Sqlite (NoLoggingT m) a -> m a

-- API specification
type ChattyApi =
	"groups" :> Get [Group]
	:<|> "group" :> Capture "userId" Int :> Get (Maybe Group)


chattyApi :: Proxy ChattyApi
chattyApi =  Proxy

db :: Proxy Sqlite
db = undefined

server :: Conn -> Server ChattyApi
server conn = getGroupsH conn :<|> getGroupH conn

intToKey :: (PrimitivePersistField (Key a b)) => Int -> Key a b
intToKey p = integralToKey p


integralToKey :: (PrimitivePersistField i, PrimitivePersistField (Key a b)) =>  i -> Key a b
integralToKey = fromPrimitivePersistValue db . toPrimitivePersistValue db

getGroupsH :: (MonadBaseControl IO m, MonadIO m) => Conn -> m [Group]
getGroupsH conn = conn $ select CondEmpty
	--return [ Group "Haskell" ]

getGroupH :: (MonadBaseControl IO m, MonadIO m) => Conn -> Int -> m (Maybe Group)
getGroupH conn groupId = conn $ get (intToKey groupId)

runTestServer :: Conn -> Port -> IO ()
runTestServer conn port = run port (serve chattyApi $ server conn)

testData conn =
	conn $ do
		runMigration $ do
			migrate (undefined :: Group)
			migrate (undefined :: Channel)

		haskellKey <- insert $ Group "Haskell"
		_ <- insert $ Group "Idris"
		_ <- insert $ Group "Purescript"
		_ <- insert $ Group "F#"
		_ <- insert $ Group "Elm"
		_ <- insert $ Channel "Servant" haskellKey
		_ <- insert $ Channel "Groundhog" haskellKey
		return ()


main :: IO ()
main =
	do
		let
			conn :: Conn
			conn m = withSqliteConn "db.sqlite" $ runDbConn m

		-- testData conn

		runTestServer conn 8001
