{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Monoid
import Data.Pool
import Data.Proxy
import Data.Text
import Database.Groundhog
import Database.Groundhog.Sqlite
import GHC.Generics
import Models.Channel
import Models.Group
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


-- * Example
type Conn = (MonadBaseControl IO m, MonadIO m) => (Sqlite -> m a) -> m a



-- API specification
type ChattyApi =
	"groups" :> Get [Group]


chattyApi :: Proxy ChattyApi
chattyApi =  Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'EitherT (Int, String) IO' monad.
server :: Conn -> Server ChattyApi
server conn = getGroupsH conn

getGroupsH :: Monad m => Conn -> m [Group]
getGroupsH conn =
	conn $ runDbConn $ select CondEmpty


-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runTestServer :: Conn -> Port -> IO ()
runTestServer conn port = run port (serve chattyApi $ server conn)

-- Put this all to work!
--main :: IO ()
--main = runTestServer 8001


main :: IO ()
main =
	do
		let
			conn  :: Conn
			conn = withSqliteConn ":memory:"
		conn $ runDbConn $
			runMigration $ do
				migrate (undefined :: Group)
				migrate (undefined :: Channel)
		-- withSqliteConn ":memory:" $ runDbConn $ do
		-- 	runMigration $ do
		-- 		migrate (undefined :: Group)
		-- 		migrate (undefined :: Channel)
		-- 	haskellKey <- insert $ Group "Haskell"
		-- 	get haskellKey >>= liftIO . print
		-- 	_ <- insert $ Channel "Servant" haskellKey
		-- 	_ <- insert $ Channel "Groundhog" haskellKey
		-- 	idrisKey <- insert $ Group "Idris"
		-- 	allGroups <- select CondEmpty
		-- 	liftIO $ putStrLn $ "All Groups: " ++ show (allGroups :: [Group])
		-- 	_ <- insert $ Channel "IdrisJS" idrisKey
		-- 	-- bonus melon for all large melon orders. The values used in expressions should have known type, so literal 5 is annotated.
		-- 	--update [QuantityField =. liftExpr QuantityField + 1] (ProducttNameField ==. ("Melon" :: String) &&. QuantityField >. (4 :: Int))
		-- 	haskellChannels <- select $ (ChannelGroupField ==. haskellKey) `orderBy` [Asc ChannelNameField]
		-- 	liftIO $ putStrLn $ "Haskell Channels: " ++ show haskellChannels

		runTestServer conn 8001
