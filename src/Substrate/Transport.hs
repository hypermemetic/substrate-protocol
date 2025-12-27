-- | Low-level transport for Substrate RPC calls
--
-- Pure IO functions for WebSocket communication with the Substrate backend.
-- All Plexus calls go through 'plexus_call' for routing.
module Substrate.Transport
  ( -- * RPC Calls
    rpcCall
  , rpcCallWith

    -- * Schema Fetching
  , fetchSchemaAt
  , fetchMethodSchemaAt
  , extractSchema
  , extractSchemaResult

    -- * Method Invocation
  , invokeMethod
  , invokeRaw
  ) where

import Control.Exception (SomeException, catch)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Streaming.Prelude as S

import Substrate.Client (SubstrateConfig(..), connect, disconnect, substrateRpc, defaultConfig)
import Plexus.Types (PlexusStreamItem(..))
import Plexus.Schema.Recursive (PluginSchema, MethodSchema, SchemaResult(..), parsePluginSchema, parseSchemaResult)

-- | Low-level RPC call with default localhost config
rpcCall :: Text -> Value -> IO (Either Text [PlexusStreamItem])
rpcCall = rpcCallWith defaultConfig

-- | Low-level RPC call with custom config
rpcCallWith :: SubstrateConfig -> Text -> Value -> IO (Either Text [PlexusStreamItem])
rpcCallWith cfg method params = do
  result <- (Right <$> doCallInner cfg method params)
    `catch` \(e :: SomeException) ->
      pure $ Left $ T.pack $ "Connection error: " <> show e
  pure result

doCallInner :: SubstrateConfig -> Text -> Value -> IO [PlexusStreamItem]
doCallInner cfg method params = do
  conn <- connect cfg
  items <- S.toList_ $ substrateRpc conn method params
  disconnect conn
  pure items

-- | Fetch schema at a specific path
-- Empty path = root (plexus.schema)
-- Non-empty path = child schema (e.g., ["solar", "earth"] -> solar.earth.schema)
fetchSchemaAt :: SubstrateConfig -> [Text] -> IO (Either Text PluginSchema)
fetchSchemaAt cfg path = do
  let schemaMethod = if null path
        then "plexus.schema"
        else T.intercalate "." path <> ".schema"
  result <- rpcCallWith cfg "plexus_call" (object ["method" .= schemaMethod])
  case result of
    Left err -> pure $ Left err
    Right items -> pure $ extractSchema items

-- | Extract PluginSchema from stream items
extractSchema :: [PlexusStreamItem] -> Either Text PluginSchema
extractSchema items =
  case [dat | StreamData _ _ ct dat <- items, ".schema" `T.isSuffixOf` ct] of
    (dat:_) -> parsePluginSchema dat
    [] -> case [err | StreamError _ _ err _ <- items] of
      (err:_) -> Left err
      [] -> Left "No schema in response"

-- | Fetch a specific method's schema
-- Uses the parameter-based query: plugin.schema with {"method": "name"}
fetchMethodSchemaAt :: SubstrateConfig -> [Text] -> Text -> IO (Either Text MethodSchema)
fetchMethodSchemaAt cfg path methodName = do
  let schemaMethod = if null path
        then "plexus.schema"
        else T.intercalate "." path <> ".schema"
  result <- rpcCallWith cfg "plexus_call" (object
    [ "method" .= schemaMethod
    , "params" .= object ["method" .= methodName]
    ])
  case result of
    Left err -> pure $ Left err
    Right items -> case extractSchemaResult items of
      Left err -> pure $ Left err
      Right (SchemaMethod m) -> pure $ Right m
      Right (SchemaPlugin _) -> pure $ Left "Expected method schema, got plugin schema"

-- | Extract SchemaResult (plugin or method) from stream items
extractSchemaResult :: [PlexusStreamItem] -> Either Text SchemaResult
extractSchemaResult items =
  case [dat | StreamData _ _ ct dat <- items, ".schema" `T.isSuffixOf` ct] of
    (dat:_) -> parseSchemaResult dat
    [] -> case [err | StreamError _ _ err _ <- items] of
      (err:_) -> Left err
      [] -> Left "No schema in response"

-- | Invoke a method and return stream items
invokeMethod :: SubstrateConfig -> [Text] -> Text -> Value -> IO (Either Text [PlexusStreamItem])
invokeMethod cfg namespacePath method params = do
  let fullPath = if null namespacePath then ["plexus"] else namespacePath
  let dotPath = T.intercalate "." (fullPath ++ [method])
  let callParams = object ["method" .= dotPath, "params" .= params]
  rpcCallWith cfg "plexus_call" callParams

-- | Invoke with raw method path
invokeRaw :: SubstrateConfig -> Text -> Value -> IO (Either Text [PlexusStreamItem])
invokeRaw cfg method params = do
  let callParams = object ["method" .= method, "params" .= params]
  rpcCallWith cfg "plexus_call" callParams
