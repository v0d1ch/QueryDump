{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module QueryDump
  ( dumpQuery
  , RunDbM
  , QueryMode (..)
  ) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Logger (MonadLogger (..))
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import qualified Control.Monad.Trans.Resource as R
import           Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Database.Esqueleto.Internal.Sql as EI
import           Database.Persist.Postgresql (ConnectionString, PersistValue, SqlBackend,
                                              withPostgresqlConn)
import           Database.Persist.Types
import           Text.PrettyPrint


type RunDbM m =
  (MonadBaseControl IO m, MonadIO m, MonadLogger m)

data Config = Config
  { configConnStr :: T.Text }

data QueryMode
  = SelectQuery
  | DeleteQuery
  | InsertQuery
  | UpdateQuery

withConn :: RunDbM m => ConnectionString -> (SqlBackend -> R.ResourceT m a) -> m a
withConn connstr =
  R.runResourceT .
  withPostgresqlConn connstr

dumpQuery
  :: (EI.SqlSelect a r, RunDbM m)
  => Maybe QueryMode -> EI.SqlQuery a -> m ()
dumpQuery qm q = do
  (bld, values) <- getQuery (maybe EI.SELECT chooseQueryMode qm) q
  let valD = map extractPersistValue values
  let queryD = mkDoc bld
  liftIO $ print $
    text "========================================================" $$
    (vcat (zipQueryDoc queryD valD)) $$ last queryD                 $$
    text "========================================================"
  where
    zipQueryDoc qd vd = zipWith (<+>) qd vd
    mkDoc bld =
      map (text . TL.unpack) (TL.splitOn (TL.pack "?") (TLB.toLazyText bld))

getQuery
  :: (EI.SqlSelect a r, RunDbM m)
  => EI.Mode -> EI.SqlQuery a -> m (TLB.Builder, [PersistValue])
getQuery mode q = do
  let Config {..} = loadConfig
  withConn (TE.encodeUtf8 configConnStr)
    (\conn -> return $ EI.toRawSql mode (conn, EI.initialIdentState) q)

loadConfig :: Config
loadConfig =
  Config
  {configConnStr = "host=localhost, port=5432, dbname=ts4-test, user=postgres"}

extractPersistValue :: PersistValue -> Doc
extractPersistValue (PersistText s) = quotes $ text (T.unpack s)
extractPersistValue (PersistByteString bs) =
  quotes $ text (T.unpack $ TE.decodeUtf8With lenientDecode bs)
extractPersistValue (PersistInt64 i) = text $ show i
extractPersistValue (PersistDouble d) = text $ show d
extractPersistValue (PersistRational r) = text $ show r
extractPersistValue (PersistDay d) = quotes $ text $ show d
extractPersistValue (PersistTimeOfDay d) = quotes $ text $ show d
extractPersistValue (PersistUTCTime d) = quotes $ text $ show d
extractPersistValue PersistNull = text "Unexpected null"
extractPersistValue (PersistBool b) = text $ show b
extractPersistValue (PersistList _) = text "Cannot convert PersistList to Text"
extractPersistValue (PersistMap _) = text "Cannot convert PersistMap to Text"
extractPersistValue (PersistObjectId _) =
  text "Cannot convert PersistObjectId to Text"
extractPersistValue (PersistDbSpecific _) =
  text "Cannot convert PersistDbSpecific to Text."

chooseQueryMode :: QueryMode -> EI.Mode
chooseQueryMode SelectQuery = EI.SELECT
chooseQueryMode DeleteQuery = EI.DELETE
chooseQueryMode InsertQuery = EI.INSERT_INTO
chooseQueryMode UpdateQuery = EI.UPDATE
