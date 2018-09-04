{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
module CryptoDepth.Db.Query.Query
( newestSellPathSumsSelect
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db
import CryptoDepth.Db.Internal.Table.Run
import CryptoDepth.Db.Internal.Table.Path
import qualified CryptoDepth as CD
import qualified Money

import Database.Beam
import Database.Beam.Query.Internal         (QNested)
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax        (PgExpressionSyntax)


type SlippageQty slippage numeraire = Tagged slippage (Money.Dense numeraire)

type PathQtyExpr s slippage numeraire =
    Columnar' (QExpr PgExpressionSyntax (QNested s)) (SlippageQty slippage numeraire)

type PathSumQuery s numeraire slippage =
    Q PgSelectSyntax CryptoDepthDb s
        ( QExpr PgExpressionSyntax s Text
        , QExpr PgExpressionSyntax s (SlippageQty slippage numeraire)
        )

newestSellPathSumsSelect
    :: forall numeraire slippage notSureWhatThisShouldBe.
    ( KnownSymbol numeraire
    , PathTable numeraire Postgres
    , PathQuantity slippage numeraire notSureWhatThisShouldBe
    )
    => SqlSelect PgSelectSyntax
        ( Text
        , SlippageQty slippage numeraire
        )
newestSellPathSumsSelect =
    select newestSellPathSums

-- | Return slippage sums when going from 'numeraire' to symbol (ie. buying "symbol")
newestBuyPathSums
    :: forall numeraire s slippage.
       ( KnownSymbol numeraire
       , PathTable numeraire Postgres
       , PathQuantity slippage numeraire (QExpr PgExpressionSyntax (QNested s))
       )
    => PathSumQuery s numeraire slippage    -- ^ (symbol, slippageSum)
newestBuyPathSums = newestPathSums _pathSrc _pathDst

-- | Return slippage sums when going from symbol to 'numeraire' (ie. selling "symbol")
newestSellPathSums
    :: forall numeraire s slippage.
       ( KnownSymbol numeraire
       , PathTable numeraire Postgres
       , PathQuantity slippage numeraire (QExpr PgExpressionSyntax (QNested s))
       )
    => PathSumQuery s numeraire slippage    -- ^ (symbol, slippageSum)
newestSellPathSums = newestPathSums _pathDst _pathSrc

-- | Return slippage sums for paths from one specified symbol to another.
-- E.g. "newestPathSums isNumeraire groupField" returns the slippage sums
--  for "path"s grouped by "groupField path" (and where "isNumeraire path == numeraire")
newestPathSums
    :: forall numeraire s slippage.
       ( KnownSymbol numeraire
       , PathTable numeraire Postgres
       , PathQuantity slippage numeraire (QExpr PgExpressionSyntax (QNested s))
       )
    => ( PathT numeraire (QExpr PgExpressionSyntax (QNested s))
            -> QExpr PgExpressionSyntax (QNested s) Text
       )    -- ^ Symbol that is equal to 'numeraire' symbol
    -> ( PathT numeraire (QExpr PgExpressionSyntax (QNested s))
            -> Columnar (QExpr PgExpressionSyntax (QNested s)) CD.Sym
       )    -- ^ Grouping by this symbol, return sum of path slippages for this symbol
    -> PathSumQuery s numeraire slippage    -- ^ (symbol, slippageSum)
newestPathSums isNumeraire groupField =
   aggregate_
        (\path ->
            ( group_ (groupField path)
            , fromMaybe_ 0
                (sumOver_ allInGroupExplicitly_
                    (fromC' (pathQuantity path :: PathQtyExpr s slippage numeraire))
                )
            )
        )
        (newestPaths isNumeraire)
  where
    fromC' :: Columnar' f a -> Columnar f a
    fromC' (Columnar' c) = c

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
    guard_ (_pathRunId path `references_` run)
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
    allRuns = all_ (_runInfo cryptoDepthDb)
