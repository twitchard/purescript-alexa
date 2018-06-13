module Amazon.Alexa.Handler
  ( makeHandler
  , Handler
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect.Aff (Aff, Error, Fiber, attempt, launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, EffectFn3, mkEffectFn3, runEffectFn2)
import Foreign (Foreign, unsafeToForeign)

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
makeHandler :: (Foreign → Foreign → (Aff Foreign)) → Handler
makeHandler fn = mkEffectFn3 fn'
  where
    fn' event ctx callback = launchAff do
      result <- attempt (fn event ctx)
      case result of
        Left err → liftEffect $ runEffectFn2 callback (toNullable (Just err)) (unsafeToForeign (toNullable Nothing))
        Right val → liftEffect $ runEffectFn2 callback (toNullable Nothing) val
      pure unit

type Handler = EffectFn3 Foreign Foreign (EffectFn2 (Nullable Error) Foreign Unit) (Fiber Unit)
