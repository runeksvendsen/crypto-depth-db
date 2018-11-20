module Main where

import qualified CryptoDepth.Db.Test.Prepare                as Prepare
import qualified CryptoDepth.Db.Query                       as Test
import           CryptoDepth.Db.Internal.Prelude
import           CryptoDepth.Db.Internal.Util               (liquidPathsMap)
import           Orphans                                    ()
import qualified CryptoDepth                                as CD

import qualified Data.HashMap.Strict                        as Map
import qualified Data.HashSet                               as Set
import qualified Database.Beam                              as Beam
import qualified Database.Beam.Postgres                     as Postgres
import           Test.Hspec
import           Data.List                                  (sort, sortOn, sortBy)


main :: IO ()
main =
    Prepare.runWithDb
        hspecMain
        mempty

hspecMain
    :: String
    -> [CD.ABook]
    -> Postgres.Connection
    -> IO ()
hspecMain fileName books conn = hspec $ do
    describe ("newest path sums (" ++ fileName ++ ")") $ do
        it "returns correct sums for 5% slippage" $
            testSumSelect allPathsInfos conn
    describe ("newest paths (" ++ fileName ++ ")") $
        mapM_ (testPathSelect' conn) (Map.toList allPathsInfos)
  where
    testPathSelect' conn (sym, pathInfos) =
        it ("returns correct PathInfos for " ++ toS sym) $
            testPathSelect conn sym pathInfos
    allPathsInfos = CD.allPathsInfos . liquidPathsMap $ books

type TestSlippage = Test.OneDiv 20
type TestNumeraire = "USD"

testPathSelect
    :: Postgres.Connection
    -> CD.Sym
    -> ( [CD.PathInfo TestNumeraire TestSlippage]
       , [CD.PathInfo TestNumeraire TestSlippage]
       )
    -> Expectation
testPathSelect conn sym pathsInfos = do
    result <- Beam.withDatabase conn (Test.newestBuySellPaths sym)
    mkSets result `shouldBe` mkSets pathsInfos
  where
    mkSets :: ( [CD.PathInfo num slip]
              , [CD.PathInfo num slip]
              )
           -> ( ResultSet num slip
              , ResultSet num slip
              )
    mkSets (a,b) = (ResultSet $ Set.fromList a, ResultSet $ Set.fromList b)

testSumSelect
    :: Map.HashMap
        CD.Sym
        ( [CD.PathInfo TestNumeraire TestSlippage]
        , [CD.PathInfo TestNumeraire TestSlippage]
        )
    -> Postgres.Connection
    -> Expectation
testSumSelect allPathsInfos conn =
    Beam.withDatabase conn (run $ Beam.select Test.newestBuySellPathSums)
        >>= assertEquals
  where
    run :: Beam.FromBackendRow Postgres.Postgres a
        => Beam.SqlSelect Postgres.PgSelectSyntax a -> Postgres.Pg [a]
    run = Beam.runSelectReturningList
    assertEquals ::
        [( CD.Sym
         , ( Test.SlippageQty TestSlippage TestNumeraire
           , Test.SlippageQty TestSlippage TestNumeraire
           )
         )
        ]
        -> IO ()
    assertEquals input =
        input
        `shouldBe`
        (sortBy bySumSym . Map.toList $ fmap pathTotals allPathsInfos)
    pathTotals (buyL, sellL) =
        ( sum $ map CD.piQty buyL
        , sum $ map CD.piQty sellL
        )
    bySumSym (sym1, (buyQty1, sellQty1)) (sym2, (buyQty2, sellQty2)) =
        let bySum = (buyQty2+sellQty2) `compare` (buyQty1+sellQty1)
        in if bySum == EQ
            then sym1 `compare` sym2
            else bySum

newtype ResultMap currency = ResultMap
    (Map.HashMap Test.Sym (CD.Amount currency, CD.Amount currency))
        deriving Eq

instance KnownSymbol currency => Show (ResultMap currency) where
    show (ResultMap hm) = unlines . fmap show . sortOn fst $ Map.toList hm

mkResultMap ::
    Map.HashMap
        Test.Sym
        ( Test.SlippageQty slippage numeraire
        , Test.SlippageQty slippage numeraire
        )
    -> ResultMap numeraire
mkResultMap =
    ResultMap . fmap (mapTuple unTagged)
  where
    mapTuple f (a,b) = (f a, f b)

newtype ResultSet currency slippage = ResultSet
    (Set.HashSet (CD.PathInfo currency slippage))
        deriving Eq

instance KnownSymbol currency => Show (ResultSet currency slippage) where
    show (ResultSet hs) = unlines . fmap show . sort $ Set.toList hs
