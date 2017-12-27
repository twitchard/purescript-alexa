module Web.Amazon.Alexa.Lens where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Data.Symbol (SProxy(..))

_response :: ∀ r a. Lens' { response :: a | r} a
_response = prop (SProxy :: SProxy "response")

_sessionAttributes:: ∀ r a. Lens' { sessionAttributes :: a | r} a
_sessionAttributes = prop (SProxy :: SProxy "sessionAttributes")

_outputSpeech :: ∀ r a. Lens' { outputSpeech :: a | r} a
_outputSpeech = prop (SProxy :: SProxy "outputSpeech")

_shouldEndSession :: ∀ r. Lens' { shouldEndSession :: Boolean | r} Boolean
_shouldEndSession = prop (SProxy :: SProxy "shouldEndSession")

_text :: ∀ r. Lens' { text :: String | r} String
_text = prop (SProxy :: SProxy "text")
