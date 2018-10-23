{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
module CryptoDepth.Db.Query.Common
( module CryptoDepth.Db.Query.Common
, PathTable
, CD.Sym
, CD.OneDiv
, KnownSymbol
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db
import CryptoDepth.Db.Internal.Table.Run
import CryptoDepth.Db.Internal.Table.Path
import CryptoDepth.Db.Internal.Table.RunSymbol
import qualified CryptoDepth as CD
import qualified Money

import Database.Beam
import Database.Beam.Query.Internal         (QNested)
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax        (PgExpressionSyntax)


-- | Paths whose specified field is equal to 'numeraire'.
-- I.e. "newestPaths isNumeraire" selects all "path"s for which "isNumeraire path == numeraire"
newestPaths
    :: forall numeraire s.
       (KnownSymbol numeraire, PathTable numeraire Postgres)
    => ( PathT numeraire (QExpr PgExpressionSyntax s)
            -> Columnar (QExpr PgExpressionSyntax s) CD.Sym
       )    -- ^ Field that is equal to 'numeraire' symbol
    -> Q PgSelectSyntax CryptoDepthDb s (PathT numeraire (QExpr PgExpressionSyntax s))
newestPaths isNumeraire = do
    path <- newestPathsAll
    guard_ (isNumeraire path ==. val_ numeraireSym)
    return path
  where
    numeraireSym :: Text
    numeraireSym = toS $ symbolVal (Proxy :: Proxy numeraire)

newestPathsAll
    :: (KnownSymbol numeraire, PathTable numeraire Postgres)
    => Q PgSelectSyntax CryptoDepthDb s (PathT numeraire (QExpr PgExpressionSyntax s))
newestPathsAll = do
    run <- newestRun
    path <- allPaths
    guard_ (_pathRun path `references_` run)
    return path

allPaths
    :: forall numeraire s.
       (KnownSymbol numeraire, PathTable numeraire Postgres)
    => Q PgSelectSyntax CryptoDepthDb s ((PathT numeraire) (QExpr PgExpressionSyntax s))
allPaths = all_ (pathTable cryptoDepthDb :: PathEntityType Postgres numeraire)

newestRun :: Q PgSelectSyntax CryptoDepthDb s (RunT (QExpr PgExpressionSyntax s))
newestRun =
    limit_ 1 sortedRuns
  where
    sortedRuns = orderBy_ (desc_ . _runId) allRuns
    allRuns = all_ (_runs cryptoDepthDb)
