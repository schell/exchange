module Order.PureOperations (
    pureInsertOrder,
    pureExpireOrders
) where

import Types
import qualified Data.Set as S


pureInsertOrder :: Id -> OrderType -> UTCTime -> BTC -> USD -> Orders -> (Order, Orders)
pureInsertOrder uid ordTy expr btc usd orders =
    let oid     = _nextOrderId  orders
        buys    = _buyOrders orders
        sells   = _sellOrders orders
        buys'   = if ordTy == Buy then insertNewOrderToLedger order buys else buys
        sells'  = if ordTy == Sell then insertNewOrderToLedger order sells else sells
        order  = Order ordTy RawOrder { _orderId = oid
                                      , _orderOwner = uid
                                      , _orderExpires = expr
                                      , _orderAmount = btc
                                      , _orderThreshold = usd
                                      }
    in (order, Orders { _buyOrders = buys'
                      , _sellOrders = sells'
                      , _nextOrderId = succ oid
                      })


insertNewOrderToLedger :: Order -> Ledger -> Ledger
insertNewOrderToLedger o l = l{_needsFunding = S.insert o os}
    where os = _needsFunding l


pureExpireOrders :: UTCTime -> Orders -> Orders
pureExpireOrders now (Orders buys sells oid) = 
    Orders (pureExpireFromLedger now buys) (pureExpireFromLedger now sells) oid


pureExpireFromLedger :: UTCTime -> Ledger -> Ledger
pureExpireFromLedger t (Ledger nf nm e c) = 
    let (exnf,nf')  = S.partition expired nf 
        (exnm,nm')  = S.partition expired nm
        expired (Order _ ro) = t >= _orderExpires ro
    in Ledger nf' nm' (exnm `S.union` exnf `S.union` e) c

