{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
module CryptoDepth.Db.Query.PathSums
( testNewestPathSumsSelect_5
, PathTable
, SlippageQty
, CD.Sym
, CD.OneDiv
, KnownSymbol
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db.Query.Common
import CryptoDepth.Db.Query.RunSymbols
import CryptoDepth.Db
import CryptoDepth.Db.Internal.Table.Run
import CryptoDepth.Db.Internal.Table.Path
import CryptoDepth.Db.Internal.Table.RunSymbol
import qualified CryptoDepth as CD
import qualified Money
import qualified Data.HashMap.Strict    as Map

import Database.Beam
import Database.Beam.Query.Internal         (QNested)
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax        (PgExpressionSyntax)


type SlippageQty slippage numeraire = Tagged slippage (CD.Amount numeraire)

type PathQtyExpr s slippage numeraire =
    Columnar' (QExpr PgExpressionSyntax (QNested s)) (SlippageQty slippage numeraire)

type PathSumQuery s numeraire slippage =
    Q PgSelectSyntax CryptoDepthDb s
        ( QExpr PgExpressionSyntax s Text
        , QExpr PgExpressionSyntax s (SlippageQty slippage numeraire)
        )


testNewestPathSumsSelect_5
    :: ( KnownSymbol numeraire
       , PathTable numeraire Postgres
       )
    => Pg
        (Map.HashMap
            Sym
            ( SlippageQty (OneDiv 20) numeraire
            , SlippageQty (OneDiv 20) numeraire
            )
        )
testNewestPathSumsSelect_5 =  do
    symMap <- Map.fromList <$>
        runSelectReturningList newestPathSumsSelect_5
    return $ trace "Got symMap" ()
    keyLst <- runSelectReturningList newestSymbolsSelect
    return $ trace "Got keyLst" ()
    let zeroQtyMap = Map.fromList $ map zeroQtySym keyLst
    -- TODO: correct precendence?
    return (symMap `Map.union` zeroQtyMap)
  where
    zeroQty :: Tagged slippage (CD.Amount currency)
    zeroQty = Tagged $ fromIntegral 0
    zeroQtySym :: Sym -> (Sym, (SlippageQty slip numeraire, SlippageQty slip numeraire))
    zeroQtySym sym = (sym, (zeroQty, zeroQty))

newestPathSumsSelect_5
    :: forall numeraire.
    ( KnownSymbol numeraire
    , PathTable numeraire Postgres
    )
    => SqlSelect PgSelectSyntax
        ( Text
        , ( SlippageQty (CD.OneDiv 20) numeraire
          , SlippageQty (CD.OneDiv 20) numeraire
          )
        )
newestPathSumsSelect_5 = select $ do
    (buySym, buyQty)   <- newestBuyPathSums
    (sellSym, sellQty) <- newestSellPathSums
    guard_ (buySym ==. sellSym)
    return (buySym, (buyQty, sellQty))

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
                (sum_
                    (fromC' (pathQuantity path :: PathQtyExpr s slippage numeraire))
                )
            )
        )
        (newestPaths isNumeraire)
