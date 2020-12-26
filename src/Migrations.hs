{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Migrations where

import Control.Monad (unless)
import Control.Monad.IO.Unlift
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Data.Maybe
import qualified Database.SQLite.Simple as DB
import Numeric.Natural
import System.Exit

runSchemaMigrations :: MonadUnliftIO m => DB.Connection -> NonEmpty (Migration m) -> Maybe Natural -> m ()
runSchemaMigrations conn migrations toVersion = do
  changes <- getSchemaChanges conn migrations toVersion
  mapM_ ($ conn) changes

data Migration m = Migration
  { version :: Natural,
    up :: SchemaChange m,
    down :: SchemaChange m
  }

type SchemaChange m = DB.Connection -> m ()

getSchemaChanges :: MonadUnliftIO m => DB.Connection -> NonEmpty (Migration m) -> Maybe Natural -> m [SchemaChange m]
getSchemaChanges conn migrations toVersion = do
  let ms = L.toList migrations
  validateVersions ms toVersion
  initMigrations conn
  dbVersions <- getDbVersions conn
  diffMigrations dbVersions ms toVersion

validateVersions :: forall m. MonadUnliftIO m => [Migration m] -> Maybe Natural -> m ()
validateVersions = _validateVersions 0
  where
    _validateVersions :: Natural -> [Migration m] -> Maybe Natural -> m ()
    _validateVersions _ [] _ = return ()
    _validateVersions lastVersion (Migration {version} : ms) toVersion
      | lastVersion >= version =
        invalidMigrations
      | null ms =
        unless (isNothing toVersion || current) invalidMigrations
      | otherwise =
        _validateVersions version ms $ if current then Nothing else toVersion
      where
        current = toVersion == Just version
        invalidMigrations = liftIO $ do
          putStrLn "Invalid migration versions"
          exitWith $ ExitFailure 2

initMigrations :: MonadUnliftIO m => DB.Connection -> m ()
initMigrations _ = return ()

getDbVersions :: MonadUnliftIO m => DB.Connection -> m [Natural]
getDbVersions _ = return []

diffMigrations :: forall m. MonadUnliftIO m => [Natural] -> [Migration m] -> Maybe Natural -> m [SchemaChange m]
diffMigrations dbVersions migrations toVersion = do
  let v = fromMaybe (version . last $ migrations) toVersion
  _diffMigrations dbVersions migrations v
  where
    _diffMigrations :: [Natural] -> [Migration m] -> Natural -> m [SchemaChange m]
    _diffMigrations [] ms toV =
      confirmMigrations (map up $ takeWhile ((<= toV) . version) ms) "Upgrade DB?"
    _diffMigrations vs [] _ =
      invalidMigrations $ "down migrations not available, current DB version is " <> show (last vs)
    _diffMigrations (v : vs) (Migration {version} : ms) toV
      | v /= version =
        invalidMigrations $ "version in DB " <> show v <> " is different from version in migration " <> show version
      | v == toV =
        if length vs > length ms
          then invalidMigrations $ "down migrations not available, current DB version is " <> show (last vs)
          else confirmMigrations (map down $ take (length vs) ms) "Downgrade DB (data can be lost)?"
      | otherwise = _diffMigrations vs ms toV

    confirmMigrations :: [SchemaChange m] -> String -> m [SchemaChange m]
    confirmMigrations ms msg = liftIO $ do
      putStr $ msg <> " y/n:"
      ok <- getLine
      if ok == "y"
        then return ms
        else exitWith $ ExitFailure 2

    invalidMigrations :: String -> m [SchemaChange m]
    invalidMigrations e = liftIO $ do
      putStrLn $ "Incompatible migrations and DB: " <> e
      exitWith $ ExitFailure 2
