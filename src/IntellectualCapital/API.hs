{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module IntellectualCapital.API where

import IntellectualCapital
import Servant.API
import Servant.API.Generic

data OrderType = Market | Limit IC | Stop IC IC

data AssetOrder = Sell AssetID UserID OrderType | Buy AssetID UserID OrderType

data IntellectualCapitalAPI route = IntellectualCapitalAPI
  { createAsset :: route :- ReqBody '[JSON] Asset :> PostNoContent
  , assetOrder :: route :- ReqBody '[JSON] AssetOrder :> PostNoContent
  , checkAccount :: route :- ReqBody '[JSON] UserID :> Get '[JSON] Account
  }
