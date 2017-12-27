module Web.Amazon.Alexa.Lens where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Type.Data.Symbol (SProxy(..))

_response :: ∀ r a. Lens' { response :: Maybe a | r} (Maybe a)
_response = prop (SProxy :: SProxy "response")

_outputSpeech :: ∀ r. Lens' { outputSpeech :: String | r} String
_outputSpeech = prop (SProxy :: SProxy "outputSpeech")

_text :: ∀ r. Lens' { text :: String | r} String
_text = prop (SProxy :: SProxy "text")
