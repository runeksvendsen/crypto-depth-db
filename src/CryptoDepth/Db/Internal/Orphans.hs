{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module CryptoDepth.Db.Internal.Orphans where

import qualified CryptoVenues.Venues            ()
import           CryptoVenues.Fetch.MarketBook  (MarketBook)
import qualified OrderBook.Types                as OB
import qualified CryptoDepth

import qualified Money
import qualified Data.Aeson                     as Json
import           Data.Aeson                     ((.=), (.:))
import           Data.Proxy                     (Proxy(..))
import           GHC.TypeLits                   (KnownSymbol, SomeSymbol(..)
                                                , symbolVal, someSymbolVal
                                                )
import           Data.Maybe                     (fromMaybe)


-- TODO: Move below to "orderbook" library
instance Json.ToJSON CryptoDepth.ABook where
    toJSON (CryptoDepth.ABook ob) = case ob of
        (_ :: OB.OrderBook venue base quote) -> Json.object
            [ "bids"  .= Json.toJSON (OB.obBids ob)
            , "asks"  .= Json.toJSON (OB.obAsks ob)
            , "venue" .= Json.toJSON (symbolVal (Proxy :: Proxy venue))
            , "base"  .= Json.toJSON (symbolVal (Proxy :: Proxy base))
            , "quote" .= Json.toJSON (symbolVal (Proxy :: Proxy quote))
            ]

instance (KnownSymbol base, KnownSymbol quote) =>
        Json.ToJSON (OB.BuySide base quote) where
    toJSON (OB.BuySide orders) = Json.toJSON orders

instance (KnownSymbol base, KnownSymbol quote) =>
        Json.ToJSON (OB.SellSide base quote) where
    toJSON (OB.SellSide orders) = Json.toJSON orders

instance (KnownSymbol base, KnownSymbol quote) =>
        Json.ToJSON (OB.Order base quote) where
    toJSON OB.Order{..} = Json.object
        [ "qty"   .= Json.toJSON (toRational oQuantity)
        , "price" .= Json.toJSON (Money.exchangeRateToRational oPrice)
        ]

instance Json.FromJSON CryptoDepth.ABook where
    parseJSON = Json.withObject "ABook" $ \obj -> do
        venue <- obj .: "venue"
        base  <- obj .: "base"
        quote <- obj .: "quote"
        case someSymbolVal base of
            SomeSymbol (Proxy :: Proxy base) -> case someSymbolVal quote of
                SomeSymbol (Proxy :: Proxy quote) -> do
                    case marketBookVenue venue of
                        Nothing -> fail $ "Unknown venue: " ++ venue
                        Just (MarketBookVenue (Proxy :: Proxy venue)) -> do
                            buySide  <- obj .: "bids"
                            sellSide <- obj .: "asks"
                            let ob = OB.OrderBook buySide sellSide
                            return $ CryptoDepth.ABook (ob :: OB.OrderBook venue base quote)

instance (KnownSymbol base, KnownSymbol quote) =>
        Json.FromJSON (OB.BuySide base quote) where
    parseJSON val = OB.BuySide <$> Json.parseJSON val

instance (KnownSymbol base, KnownSymbol quote) =>
        Json.FromJSON (OB.SellSide base quote) where
    parseJSON val = OB.SellSide <$> Json.parseJSON val

instance (KnownSymbol base, KnownSymbol quote) =>
        Json.FromJSON (OB.Order base quote) where
    parseJSON = Json.withObject "Order" $ \obj ->
        OB.Order <$> fmap Money.dense' (obj .: "qty")
                 <*> fmap exchangeRate' (obj .: "price")
      where
        exchangeRate' r =
            fromMaybe (error $ "Bad rational: " ++ show r)
                      (Money.exchangeRate r)

data MarketBookVenue =
    forall venue. MarketBook venue
    => MarketBookVenue (Proxy venue)

marketBookVenue :: String -> Maybe MarketBookVenue
marketBookVenue "bitfinex" = Just . MarketBookVenue $ Proxy @"bitfinex"
marketBookVenue "bittrex"  = Just . MarketBookVenue $ Proxy @"bittrex"
marketBookVenue "binance"  = Just . MarketBookVenue $ Proxy @"binance"
marketBookVenue "bitstamp" = Just . MarketBookVenue $ Proxy @"bitstamp"
marketBookVenue "coinbase" = Just . MarketBookVenue $ Proxy @"coinbase"
marketBookVenue _          = Nothing
