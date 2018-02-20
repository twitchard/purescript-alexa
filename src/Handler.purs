module Amazon.Alexa.Handler
  ( makeHandler
  , Handler
  )
  where

import Prelude

import Control.Monad.Aff (Aff, Error, Fiber, attempt, launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3, mkEffFn3, runEffFn2)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)

foreign import null :: Foreign

-- | Converts a curried function to produce an
-- | uncurried function of the form expected by
-- | AWS lambda.
-- |
-- | ```
-- | myHandler :: ∀ e. Foreign → Foreign → Aff e Foreign
-- | myHandler event context =
-- |   pure <<< encode $ "Return this string"
-- |
-- | handler = makeHandler myHandler
-- | ```
-- |
-- | will produce the equivalent of
-- |
-- | ```
-- | exports.handler = function (event, context, callback) {
-- |   return callback(null, "Return this string")
-- | }
-- | ```
makeHandler :: forall eff. (Foreign → Foreign → (Aff eff Foreign)) → Handler eff
makeHandler fn = mkEffFn3 fn'
  where
    fn' event ctx callback = launchAff do
      result <- attempt (fn event ctx)
      case result of
        Left err → liftEff $ runEffFn2 callback (toNullable (Just err)) null
        Right val → liftEff $ runEffFn2 callback (toNullable Nothing) val
      pure unit

type Handler eff = EffFn3 eff Foreign Foreign (EffFn2 eff (Nullable Error) Foreign Unit) (Fiber eff Unit)
