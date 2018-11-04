{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
module CryptoDepth.Db.Internal.Query.PathSums
( newestBuySellPathSums
, newestBuySellPathSumsPaginate
, PathTable
, SlippageQty
, CD.Sym
, CD.OneDiv
, KnownSymbol
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db.Internal.Query.Common
import CryptoDepth.Db.Internal.Query.RunSymbols
import CryptoDepth.Db
import CryptoDepth.Db.Internal.Table.Run
import CryptoDepth.Db.Internal.Table.Path
import CryptoDepth.Db.Internal.Table.RunSymbol
import qualified CryptoDepth as CD
import qualified Money
import qualified Data.HashMap.Strict    as Map

import Database.Beam
import Database.Beam.Query.Internal         (QNested, WithRewrittenThread)
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


-- | 'newestBuySellPathSums' with offset and limit.
--
newestBuySellPathSumsPaginate
    :: ( KnownSymbol numeraire
       , PathTable numeraire Postgres
       , PathQuantity slippage numeraire
            (QExpr PgExpressionSyntax (QNested (QNested (QNested (QNested QueryInaccessible)))))
       )
    => Integer  -- ^ Offset
    -> Integer  -- ^ Limit
    -> Pg [( Text
           , ( Tagged slippage (Amount numeraire)
             , Tagged slippage (Amount numeraire)
             )
           )
          ]
newestBuySellPathSumsPaginate offset limit =
    runSelectReturningList $ select
        $ offset_ offset
        $ limit_ limit newestBuySellPathSums

newestBuySellPathSums
    :: ( KnownSymbol numeraire
       , PathTable numeraire Postgres
       , PathQuantity slippage numeraire (QExpr PgExpressionSyntax (QNested (QNested s)))
       )
    => Q PgSelectSyntax CryptoDepthDb s
        ( WithRewrittenThread (QNested s) s
            ( QExpr PgExpressionSyntax (QNested s) Text
            , ( QExpr PgExpressionSyntax (QNested s) (SlippageQty slippage numeraire)
              , QExpr PgExpressionSyntax (QNested s) (SlippageQty slippage numeraire)
              )
            )
        )
newestBuySellPathSums =
    orderBy_ buySellQtySumSym $ do
        (buySym, buyQty)   <- newestBuyPathSums
        (sellSym, sellQty) <- newestSellPathSums
        guard_ (buySym ==. sellSym)
        return (buySym, (buyQty, sellQty))
  where
    buySellQtySumSym (sym, (buyQty', sellQty')) =
        (desc_ $ buyQty' + sellQty', asc_ sym)

-- | Return slippage sums when going from 'numeraire' to symbol (ie. buying "symbol")
newestBuyPathSums
    :: forall numeraire s slippage.
       ( KnownSymbol numeraire
       , PathTable numeraire Postgres
       , PathQuantity slippage numeraire (QExpr PgExpressionSyntax (QNested s))
       )
    => PathSumQuery s numeraire slippage    -- ^ (symbol, slippageSum)
newestBuyPathSums = newestPathSums _pathDst

-- | Return slippage sums when going from symbol to 'numeraire' (ie. selling "symbol")
newestSellPathSums
    :: forall numeraire s slippage.
       ( KnownSymbol numeraire
       , PathTable numeraire Postgres
       , PathQuantity slippage numeraire (QExpr PgExpressionSyntax (QNested s))
       )
    => PathSumQuery s numeraire slippage    -- ^ (symbol, slippageSum)
newestSellPathSums = newestPathSums _pathSrc

-- | Return slippage sums for paths with specified symbol
newestPathSums
    :: forall numeraire s slippage.
       ( KnownSymbol numeraire
       , PathTable numeraire Postgres
       , PathQuantity slippage numeraire (QExpr PgExpressionSyntax (QNested s))
       )
    => ( PathT numeraire (QExpr PgExpressionSyntax (QNested s))
            -> Columnar (QExpr PgExpressionSyntax (QNested s)) CD.Sym
       )    -- ^ Grouping by this symbol, return sum of path slippages for this symbol
    -> PathSumQuery s numeraire slippage    -- ^ (symbol, slippageSum)
newestPathSums groupField =
   aggregate_ symGroupSumQty (newestSymQuantities groupField)
  where
    symGroupSumQty (sym, slippageQty) =
        (group_ sym, fromMaybe_ 0 (sum_ slippageQty))

-- | Return path slippage quantities for all symbols in "RunSymbol".
-- Quantity is zero if no paths exist for the given symol.
newestSymQuantities
    :: forall numeraire s slippage.
       ( KnownSymbol numeraire
       , PathTable numeraire Postgres
       , PathQuantity slippage numeraire (QExpr PgExpressionSyntax (QNested s))
       )
    => ( PathT numeraire (QExpr PgExpressionSyntax (QNested s))
            -> Columnar (QExpr PgExpressionSyntax (QNested s)) CD.Sym
       )    -- ^ Grouping by this symbol, return sum of path slippages for this symbol
    -> Q PgSelectSyntax CryptoDepthDb (QNested s)
            ( QExpr PgExpressionSyntax (QNested s) Text
            , QExpr PgExpressionSyntax (QNested s) (SlippageQty slippage numeraire)
            )
newestSymQuantities groupField = do
    sym  <- newestSymbols
    pathM <- leftJoin_ newestPathsAll (\path -> groupField path ==. _symbolSym sym)
    return (_symbolSym sym, maybe_ 0 toPathQuantity pathM)
  where
    toPathQuantity path = fromC' (pathQuantity path :: PathQtyExpr s slippage numeraire)
