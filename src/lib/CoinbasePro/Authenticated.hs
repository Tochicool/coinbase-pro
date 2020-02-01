{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module CoinbasePro.Authenticated
  ( accounts
  , account
  , listOrders
  , placeOrder
  , placeLimitOrder
  , placeMarketOrder
  , cancelOrder
  , cancelAll
  , fills
  , fees
  , trailingVolume
  ) where


import           Control.Monad                      (void)
import           Data.Aeson                         (encode)
import qualified Data.ByteString.Char8              as C8
import qualified Data.ByteString.Lazy.Char8         as LC8
import           Data.Maybe                         (fromMaybe)
import qualified Data.Set                           as S
import           Data.Text                          (Text, pack, toLower,
                                                     unpack)
import           Network.HTTP.Types                 (SimpleQuery,
                                                     SimpleQueryItem,
                                                     methodDelete, methodGet,
                                                     methodPost, renderQuery,
                                                     simpleQueryToQuery)
import           Servant.Client                     (ClientM)

import           CoinbasePro.Authenticated.Accounts (Account, AccountId (..),
                                                     Fees, TrailingVolume (..))
import qualified CoinbasePro.Authenticated.API      as API
import           CoinbasePro.Authenticated.Fills    (Fill)
import           CoinbasePro.Authenticated.Orders   (Order, PlaceOrderBody (..),
                                                     STP, Status (..),
                                                     Statuses (..), TimeInForce,
                                                     Stop, statuses,
                                                     CancelAfter)
import           CoinbasePro.Authenticated.Request  (CBAuthT (..), authRequest)
import           CoinbasePro.Request                (RequestPath)


import           CoinbasePro.Types                  (OrderId (..),
                                                     OrderType(..), Price, 
                                                     ProductId (..), Side, Size,
                                                     Funds)


-- | https://docs.pro.coinbase.com/?javascript#accounts
accounts :: CBAuthT ClientM [Account]
accounts = authRequest methodGet "/accounts" "" API.accounts


-- | https://docs.pro.coinbase.com/?javascript#get-an-account
account :: AccountId -> CBAuthT ClientM Account
account aid@(AccountId t) = authRequest methodGet requestPath "" $ API.singleAccount aid
  where
    requestPath = "/accounts/" ++ unpack t


-- | https://docs.pro.coinbase.com/?javascript#list-orders
listOrders :: Maybe [Status] -> Maybe ProductId -> CBAuthT ClientM [Order]
listOrders st prid = authRequest methodGet (mkRequestPath "/orders") "" $ API.listOrders (defaultStatus st) prid
  where
    mkRequestPath :: RequestPath -> RequestPath
    mkRequestPath rp = rp ++ (C8.unpack . renderQuery True . simpleQueryToQuery $ mkOrderQuery st prid)

    mkOrderQuery :: Maybe [Status] -> Maybe ProductId -> SimpleQuery
    mkOrderQuery ss p = mkStatusQuery ss <> mkProductQuery p

    mkStatusQuery :: Maybe [Status] -> [SimpleQueryItem]
    mkStatusQuery ss = mkSimpleQueryItem "status" . toLower . pack . show <$> S.toList (unStatuses . statuses $ defaultStatus ss)

    defaultStatus :: Maybe [Status] -> [Status]
    defaultStatus = fromMaybe [All]


{-# DEPRECATED placeOrder "Use `placeLimitOrder` or `placeMarketOrder` instead" #-}
-- | https://docs.pro.coinbase.com/?javascript#place-a-new-order
placeOrder :: ProductId -> Side -> Size -> Price -> Bool -> Maybe OrderType -> Maybe STP -> Maybe TimeInForce -> CBAuthT ClientM Order
placeOrder prid sd sz price po ot stp tif =
    authRequest methodPost "/orders" (LC8.unpack $ encode body) $ API.placeOrder body
  where
    body = PlaceOrderBody Nothing ot sd prid stp Nothing Nothing (Just price) 
        (Just sz) tif Nothing (Just po) Nothing


-- | https://docs.pro.coinbase.com/?javascript#place-a-new-order
placeLimitOrder :: Side -> ProductId -> Price -> Maybe OrderId -> Maybe STP -> Maybe Stop -> Maybe Price -> Maybe Size -> Maybe TimeInForce -> Maybe CancelAfter -> Maybe Bool -> CBAuthT ClientM Order
placeLimitOrder sd prid price coid stp stop stopPrice sz tif ca po =
    authRequest methodPost "/orders" (LC8.unpack $ encode body) $ API.placeOrder body
  where
    body = PlaceOrderBody coid (Just Limit) sd prid stp stop stopPrice 
        (Just price) sz tif ca po Nothing


-- | https://docs.pro.coinbase.com/?javascript#place-a-new-order
placeMarketOrder :: Side -> ProductId -> Maybe OrderId -> Maybe STP -> Maybe Stop -> Maybe Price -> Maybe Size -> Maybe Funds -> CBAuthT ClientM Order
placeMarketOrder sd prid coid stp stop stopPrice sz funds =
    authRequest methodPost "/orders" (LC8.unpack $ encode body) $ API.placeOrder body
  where
    body = PlaceOrderBody coid (Just Market) sd prid stp stop stopPrice Nothing
     sz Nothing Nothing Nothing funds

-- | https://docs.pro.coinbase.com/?javascript#cancel-an-order
cancelOrder :: OrderId -> CBAuthT ClientM ()
cancelOrder oid = void . authRequest methodDelete (mkRequestPath "/orders") "" $ API.cancelOrder oid
  where
    mkRequestPath :: RequestPath -> RequestPath
    mkRequestPath rp = rp ++ "/" ++ unpack (unOrderId oid)


-- | https://docs.pro.coinbase.com/?javascript#cancel-all
cancelAll :: Maybe ProductId -> CBAuthT ClientM [OrderId]
cancelAll prid = authRequest methodDelete (mkRequestPath "/orders") "" (API.cancelAll prid)
  where
    mkRequestPath :: RequestPath -> RequestPath
    mkRequestPath rp = rp ++ (C8.unpack . renderQuery True . simpleQueryToQuery $ mkProductQuery prid)


-- | https://docs.pro.coinbase.com/?javascript#fills
fills :: Maybe ProductId -> Maybe OrderId -> CBAuthT ClientM [Fill]
fills prid oid = authRequest methodGet mkRequestPath "" (API.fills prid oid)
  where
    brp = "/fills"

    mkRequestPath :: RequestPath
    mkRequestPath = brp ++ (C8.unpack . renderQuery True . simpleQueryToQuery $ mkSimpleQuery prid oid)

    mkSimpleQuery :: Maybe ProductId -> Maybe OrderId -> SimpleQuery
    mkSimpleQuery p o = mkProductQuery p <> mkOrderIdQuery o


-- | https://docs.pro.coinbase.com/?javascript#get-current-fees
fees :: CBAuthT ClientM Fees
fees = authRequest methodGet mkRequestPath "" API.fees
  where
    mkRequestPath :: RequestPath
    mkRequestPath = "/fees"


-- | https://docs.pro.coinbase.com/?javascript#trailing-volume
trailingVolume :: CBAuthT ClientM [TrailingVolume]
trailingVolume = authRequest methodGet mkRequestPath "" API.trailingVolume
  where
    mkRequestPath :: RequestPath
    mkRequestPath = "/users/self/trailing-volume"


mkSimpleQueryItem :: String -> Text -> SimpleQueryItem
mkSimpleQueryItem s t = (C8.pack s, C8.pack $ unpack t)


mkProductQuery :: Maybe ProductId -> [SimpleQueryItem]
mkProductQuery = maybe [] (return . mkSimpleQueryItem "product_id" . unProductId)


mkOrderIdQuery :: Maybe OrderId -> SimpleQuery
mkOrderIdQuery = maybe [] (return . mkSimpleQueryItem "order_id" . unOrderId)
