{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Rest.PostgreSQL.Generic where

import Rest hiding (range)
import qualified Rest.Resource as R

import Database.PostgreSQL.Simple
import Database.PostgreSQL.ORM
import Database.PostgreSQL.ORM.Model

import Control.Monad.Reader
import Control.Applicative
import Control.Monad.Error

import Data.JSON.Schema
import Data.Aeson hiding (Number, Object)
import Data.Typeable
import qualified Data.ByteString.Char8 as B

data ListId a = All

type GenericResource m tr x = Resource (ReaderT Connection m) (ReaderT (GDBRef tr x) (ReaderT Connection m)) (GDBRef tr x) (ListId x) Void

-- | A generic resource.
resource :: forall m x tr. (MonadIO m, Applicative m, Model x, JSONSchema x, ToJSON x, FromJSON x, Typeable x) => GenericResource m tr x
resource = mkResourceReader
  { R.name   = B.unpack . modelTable $ (modelInfo :: ModelInfo x)
  , R.schema = withListing All $ named [("id", singleBy (DBRef . read))]
  , R.list   = list
  , R.get    = Just get
  , R.update = Just update
  , R.remove = Just remove
  , R.create = Just (create (Proxy :: Proxy x))
  }

list :: forall m x. (MonadIO m, Model x, JSONSchema x, ToJSON x, Typeable x) => ListId x -> ListHandler (ReaderT Connection m)
list All = mkListing (jsonO . someO) $ \range -> do
  conn <- ask
  liftIO $ (findAll' conn (Just (offset range, count range)) :: IO [x])

get :: (MonadIO m, Model x, JSONSchema x, ToJSON x, Typeable x) => Handler (ReaderT (GDBRef tr x) (ReaderT Connection m))
get = mkIdHandler (jsonE . jsonO . someO) $ \_ pk -> do
  conn <- lift . lift $ ask
  x <- liftIO $ findRow conn pk
  maybe (throwError NotFound) return x

update :: forall m x tr. (MonadIO m, Model x, JSONSchema x, FromJSON x, Typeable x) => Handler (ReaderT (GDBRef tr x) (ReaderT Connection m))
update = mkInputHandler (jsonE . jsonI . someI) $ \x -> do
  conn <- lift . lift $ ask
  res <- liftIO $ trySave conn (x :: x)
  either (throwError . InputError . UnsupportedFormat . show) (const $ return ()) res

remove :: (MonadIO m, Model x, JSONSchema x, ToJSON x, Typeable x) => Handler (ReaderT (GDBRef tr x) (ReaderT Connection m))
remove = mkIdHandler id $ \_ pk -> do
  conn <- lift . lift $ ask
  liftIO $ destroyByRef conn pk

create :: forall m x. (MonadIO m, Model x, JSONSchema x, FromJSON x, Typeable x) => Proxy x -> Handler (ReaderT Connection m)
create _ = mkInputHandler (jsonI . someI) $ \x -> do
  conn <- ask
  res <- liftIO $ trySave conn (x :: x)
  either (throwError . InputError . UnsupportedFormat . show) (const $ return ()) res

instance JSONSchema DBKey where
  schema _ = Choice [ Object [Field {key = "dBKey", required = True, content = Number unbounded}]
                    , Object [Field {key = "nullKey", required = True, content = Object []}]]

-- | Helper to derive the requires instances
deriveGenericRest :: TypeQ -> DecsQ
deriveGenericRest dt = [d|
  instance Model $dt
  instance JSONSchema $dt where
    schema = gSchema
  instance ToJSON $dt where
  instance FromJSON $dt where
  |]
