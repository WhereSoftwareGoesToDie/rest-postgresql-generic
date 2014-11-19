{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Rest.Api
import Rest.Driver.Snap
import Snap.Core (Snap)
import Snap.Http.Server

import System.Process
import Control.Concurrent.Async

import Rest.PostgreSQL.Generic (resource, deriveGenericRest, GenericResource, WithGenericState, defaultState)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.ORM
import Database.PostgreSQL.ORM.CreateTable
import Data.Text
import GHC.Generics

import Control.Monad.Reader
import Control.Applicative
import Data.Typeable

import Rest.Client.Internal hiding (Api)
import qualified Rest.Types.Container
import qualified Rest.Types.Error
import qualified Rest.StringMap.HashMap.Strict

import Test.HUnit

data Post = Post
  { postId :: DBKey
  , postTitle :: Text
  , postBody :: Text
  } deriving (Generic, Typeable, Show, Eq)
deriveGenericRest ''Post

testRouter :: forall m . (Applicative m, MonadIO m) => Router (WithGenericState m) (WithGenericState m)
testRouter = root -/ post
  where
    post = route (resource :: GenericResource m tr Post)

testApi :: Api (WithGenericState Snap)
testApi = [(mkVersion 1 0 0, Some1 testRouter)]

runStack :: Connection -> WithGenericState Snap a -> Snap a
runStack conn m = runReaderT m (defaultState conn)

testSnap :: Connection -> Snap ()
testSnap conn = apiToHandler' (runStack conn) testApi

main :: IO ()
main = do
  let db = "rest-generic-test"
  callProcess "dropdb" ["--if-exists", db]
  callProcess "createdb" [db]
  conn <- connect defaultConnectInfo { connectDatabase = db }
  void $ modelCreate conn (undefined :: Post)
  let snap = testSnap conn
  server <- async $ quickHttpServe snap

  testClient

  cancel server
  void $ waitCatch server
  close conn
  callProcess "dropdb" [db]

testClient :: IO ()
testClient = runWithPort "localhost" 8000 $ do
  ps <- throwErrors <$> list []
  liftIO $ Rest.Types.Container.count ps @?= 0

  let p1 = Post NullKey "First Post!!!" "BLAAAAAAAAA"
  either (error . show) id . responseBody <$> create p1
  p1' <- throwErrors <$> byId "1"
  liftIO $ p1' @?= p1 { postId = DBKey 1 }

  let p2 = Post (DBKey 1) "First Post!!! (redone)" "YEAH..."
  either (error . show) id . responseBody <$> saveById "1" p2
  p2' <- throwErrors <$> byId "1"
  liftIO $ p2' @?= p2

  throwErrors <$> remove "1"
  ps' <- throwErrors <$> list []
  liftIO $ Rest.Types.Container.count ps' @?= 0

  let n = 50
  let posts = [(show i, Post NullKey (pack $ "post" ++ show i) (pack $ "body" ++ show i)) | i <- [1..n]]
  void $ throwErrors <$> saveManyById (Rest.StringMap.HashMap.Strict.fromList posts)
  posts' <- throwErrors <$> list []
  liftIO $ Rest.Types.Container.count posts' @?= fromIntegral n
  let postItems = Rest.Types.Container.items posts'

  posts' <- throwErrors <$> list [("count","5")]
  liftIO $ Rest.Types.Container.count posts' @?= 5
  liftIO $ Rest.Types.Container.items posts' @?= Prelude.take 5 postItems

  posts' <- throwErrors <$> list [("offset","40")]
  liftIO $ Rest.Types.Container.items posts' @?= Prelude.drop 40 postItems

throwErrors :: Show e => ApiResponse e a -> a
throwErrors = either (error . show) id . responseBody

type Identifier = String
 
readId :: Identifier -> [String]
readId x = ["id", showUrl x]
 
list ::
       ApiStateC m =>
       [(String, String)] ->
         m (ApiResponse () (Rest.Types.Container.List (Main.Post)))
list pList
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request = makeReq "GET" "v1.0.0" [["post"]] pList rHeaders ""
      in doRequest fromJSON fromJSON request
 
byId :: ApiStateC m => String -> m (ApiResponse () Main.Post)
byId string
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "GET" "v1.0.0" [["post"], ["id"], [showUrl string]] []
              rHeaders
              ""
      in doRequest fromJSON fromJSON request
 
saveById ::
           ApiStateC m => String -> Main.Post -> m (ApiResponse () ())
saveById string input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/json")]
        request
          = makeReq "PUT" "v1.0.0" [["post"], ["id"], [showUrl string]] []
              rHeaders
              (toJSON input)
      in doRequest fromXML (const ()) request
 
saveManyById ::
               ApiStateC m =>
               Rest.StringMap.HashMap.Strict.StringHashMap ([(Char)]) (Main.Post)
                 ->
                 m (ApiResponse (Rest.Types.Error.Reason (()))
                      (Rest.StringMap.HashMap.Strict.StringHashMap ([(Char)])
                         (Rest.Types.Error.Status (Rest.Types.Error.Reason (())) (()))))
saveManyById input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/json")]
        request
          = makeReq "PUT" "v1.0.0" [["post"], ["id"]] [] rHeaders
              (toJSON input)
      in doRequest fromJSON fromJSON request
 
removeManyId ::
               ApiStateC m =>
               Rest.StringMap.HashMap.Strict.StringHashMap ([(Char)]) (()) ->
                 m (ApiResponse (Rest.Types.Error.Reason (()))
                      (Rest.StringMap.HashMap.Strict.StringHashMap ([(Char)])
                         (Rest.Types.Error.Status (Rest.Types.Error.Reason (())) (()))))
removeManyId input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/json")]
        request
          = makeReq "DELETE" "v1.0.0" [["post"], ["id"]] [] rHeaders
              (toJSON input)
      in doRequest fromJSON fromJSON request
 
create :: ApiStateC m => Main.Post -> m (ApiResponse () ())
create input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/json")]
        request
          = makeReq "POST" "v1.0.0" [["post"]] [] rHeaders (toJSON input)
      in doRequest fromXML (const ()) request
 
remove :: ApiStateC m => Identifier -> m (ApiResponse () ())
remove post
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "DELETE" "v1.0.0" [["post"], readId post] [] rHeaders ""
      in doRequest fromXML (const ()) request
