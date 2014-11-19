{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Rest.PostgreSQL.Generic where

import Rest hiding (range)
import qualified Rest.Resource as R

import Database.PostgreSQL.ORM
import Database.PostgreSQL.ORM.Model
import Database.PostgreSQL.Simple

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader

import Data.Aeson hiding (Number, Object)
import qualified Data.ByteString.Char8 as B
import Data.JSON.Schema
import Data.Typeable

import Language.Haskell.TH

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
  liftIO $ (findPage conn range :: IO [x])

findPage
  :: (Model x, JSONSchema x, ToJSON x, Typeable x)
  => Connection -> Rest.Range -> IO [x]
findPage conn Range{..} =
  dbSelect conn $ setLimit count
                $ setOffset offset
                $ modelDBSelect

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
  schema = gSchema

instance JSONSchema (GDBRef a b) where
  schema = gSchema

-- | Helper to derive the requires instances
deriveGenericRest :: Name -> DecsQ
deriveGenericRest tyName = do
  let ty = return $ ConT tyName
  [d|
    instance Model $ty
    instance JSONSchema $ty where
      schema = gSchema
    instance ToJSON $ty
    instance FromJSON $ty
    |]
