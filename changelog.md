# [Unreleased]
 - Added `placeLimitOrder` and `placeMarketOrder`, allowing for orders to
  optionally specify stop and stop price. `placeLimitOrder` exposes
  'cancelAfter' option for limit orders and `placeMarketOrders` allows for
  market orders with defined funds but no size.
 - `placeOrder` remains for backwards-compatibility but is now deprecated.

# Version 0.7.2.0
 - Upgraded to stack lts-14.17

# Version 0.7.1.0
 - Fixed broken examples

# Version 0.7.0.0
 - Unauthenticated requests are now all in the `ClientM` monad. `CoinbasePro.Request.run` is now required
   to operate in the IO monad.

   Example: `run (trades (ProductId "BTC-USD")) >>= print`

- Added `run_`, `runSandbox`, `runSandboxWithManager` in `CoinbasePro.Request`
- Added `currencies`, `fees`, and `trailingVolume` queries
