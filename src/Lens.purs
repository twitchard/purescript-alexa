module Web.Amazon.Alexa.Lens where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Type.Data.Symbol (SProxy(..))

_response :: ∀ r a. Lens' { response :: a | r} a
_response = prop (SProxy :: SProxy "response")

_outputSpeech :: ∀ r a. Lens' { outputSpeech :: a | r} a
_outputSpeech = prop (SProxy :: SProxy "outputSpeech")

_text :: ∀ r. Lens' { text :: String | r} String
_text = prop (SProxy :: SProxy "text")
