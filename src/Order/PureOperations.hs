module Order.PureOperations where

import Types


insertNewOrder :: Id -> OrderType -> UTCTime -> BTC -> USD -> Orders -> (Order, Orders)
insertNewOrder uid ordTy expr btc usd orders =
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
insertNewOrderToLedger o l = l{_needsFunding = LedgerNeedsFunding (o:os)}
    where LedgerNeedsFunding os = _needsFunding l
