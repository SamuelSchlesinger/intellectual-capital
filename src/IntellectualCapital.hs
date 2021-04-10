{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
module IntellectualCapital where

import Numeric.Natural
import Data.Map (Map)
import Data.Set (Set)
import Data.Tree (Tree)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import qualified Data.Maybe as Maybe
import Data.Foldable (fold)
import Data.Time (Day, diffDays)
import Data.Time.Calendar.Easter (gregorianEaster)

-- | A quantity of intellectual capital.
newtype IC = IC { unIC :: Rational }
  deriving stock (Show, Read)
  deriving newtype (Num, Fractional, Real, Enum, Eq, Ord)

-- | An identifying key for a user.
newtype UserID = UserID { unUserID :: String }
  deriving newtype (Eq, Ord, Show, Read)

-- | An account, which contains both bonds and intellectual capital.
data Account = Account
  { bonds :: [Bond]
  , capital :: IC
  , equity :: Map AssetID Rational
  , owner :: UserID
  } deriving stock (Eq, Ord, Show, Read)

-- | A bond which pays out a certain coupon at a specified period until expiration,
-- when its final payment is paid out.
data Bond = Bond
  { coupon :: IC -- ^ Amount paid out by the bond per payment.
  , period :: Natural -- ^ Number of days between each payment.
  , expiry :: Day -- ^ The day of the final payment.
  } deriving stock (Eq, Ord, Show, Read)

-- | An identifying key for an equity.
newtype AssetID = AssetID { unAssetID :: String }
  deriving newtype (Eq, Ord)
  deriving stock (Show, Read)

-- | An equity which pays out dividends to its owners upon being referenced.
data Asset = Asset
  { assetID :: AssetID
  , content :: String
  , references :: Set AssetID
  , author :: UserID
  } deriving stock (Eq, Ord, Show, Read)

-- | The data which the central bank needs to keep track of to understand the ownership
-- of bonds and intellectual capital.
data CentralBank = CentralBank
  { date :: Day
  , accounts :: Map UserID Account
  , assets :: Map AssetID Asset
  } deriving stock (Eq, Ord, Show, Read)

issuance :: Day -> Integer
issuance d = 1 where
  _potential = 2^(100 - (n `div` 1000)) 
  n = diffDays d (gregorianEaster 2020)

-- | Issue all bond coupon payments to the users' capital accounts, removing expired bonds
-- after doing so.
stepCentralBank
  :: [Asset]
  -> CentralBank
  -> CentralBank
stepCentralBank newAssets CentralBank{date, accounts, assets} = CentralBank date' accounts' assets' where
  assets' = Map.union (Map.fromList (map (\a -> (assetID a, a)) newAssets)) assets
  accounts' = Map.map updateAccount accounts
  date' = succ date
  atree = assetTrees assets'
  updateAccount Account{bonds, capital, equity, owner} = Account bonds' capital' equity' owner where
    (expiredBonds, bonds') = List.partition ((<= date') . expiry) bonds
    paysOut Bond{period, expiry} = diffDays expiry date' `mod` fromIntegral period == 0
    -- Provide each user with all of the equity for each asset they've created.
    equity' = Map.fromList [(assetID a, 1) | a <- newAssets, author a == owner] `Map.union` equity
    -- We must pay out the final coupon of each expired bond, as well as each of
    -- the coupons of the remaining bonds if they are a multiple of their period
    -- away from their expiry.
    interest = sum (map coupon expiredBonds) + sum (map coupon $ filter paysOut bonds')
    -- Pay the user for every new asset which references an asset they hold equity in.
    dividend = sum do
      { (i, e) <- Map.toList equity';
        a' <- newAssets;
        pure (if depth i a' <= issuance date' then IC $ e * fromInteger (2^(issuance date' - depth i a')) else 0)
      }
    depth i a = case Map.lookup (assetID a) atree of
      Just t -> go 0 t where
        go d (Tree.Node{Tree.rootLabel = x, Tree.subForest = xs})
          | assetID x == i = d
          | otherwise = minimum [ go (d + 1) x' | x' <- xs ]
      Nothing -> issuance date' + 1
    tax = sum do
      { a <- [a | a <- newAssets, author a == owner]
      ; pure $ sum [ fromInteger (2^(issuance date' - depth i a)) | i <- Map.keys assets', depth i a <= issuance date' ]
      }
    capital' = capital + interest + dividend - tax

assetTrees :: Map AssetID Asset -> Map AssetID (Tree Asset)
assetTrees assets = Map.fromList . map (\n@Tree.Node{Tree.rootLabel = x} -> (assetID x, n)) $ Tree.unfoldForest go (Map.elems assets) where
  go a = (a, Maybe.mapMaybe (flip Map.lookup assets) $ Set.elems $ references a) 

example :: CentralBank
example = CentralBank (gregorianEaster 2020) (Map.fromList [(UserID "Sam", account)]) Map.empty where
  account = Account { bonds = [bond], capital = 2000, equity = Map.fromList [], owner = UserID "Sam" }
  bond = Bond 1 1 (gregorianEaster 2021)

exampleAsset :: Asset
exampleAsset = Asset { author = UserID "Sam", content = "poop", assetID = AssetID "Poop", references = Set.empty }

example' :: CentralBank
example' = stepCentralBank [exampleAsset] example
