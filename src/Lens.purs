module Web.Amazon.Alexa.Lens where

import Data.Lens (Lens', lens)
import Data.Lens.Record (prop)
import Type.Data.Symbol (SProxy(..))
import Web.Amazon.Alexa.Types (AlexaRequest(..), AlexaSession, AlexaContext)

_body :: Lens' AlexaRequest
  { version :: String
  , session :: AlexaSession
  , context :: AlexaContext
  }
_body = lens get set
  where
    get (LaunchRequest r) =
      { version: r.version
      , session: r.session
      , context: r.context
      }
    get (IntentRequest r) = 
      { version: r.version
      , session: r.session
      , context: r.context
      }
    get (SessionEndedRequest r) =
      { version: r.version
      , session: r.session
      , context: r.context
      }
    
    set (LaunchRequest r) s = LaunchRequest (r
      { version = s.version
      , session = s.session
      , context = s.context
      })
    set (IntentRequest r) s = IntentRequest (r
      { version = s.version
      , session = s.session
      , context = s.context
      })
    set (SessionEndedRequest r) s = SessionEndedRequest (r
      { version = s.version
      , session = s.session
      , context = s.context
      })

_response :: ∀ r a. Lens' { response :: a | r} a
_response = prop (SProxy :: SProxy "response")

_sessionAttributes:: ∀ r a. Lens' { sessionAttributes :: a | r} a
_sessionAttributes = prop (SProxy :: SProxy "sessionAttributes")

_outputSpeech :: ∀ r a. Lens' { outputSpeech :: a | r} a
_outputSpeech = prop (SProxy :: SProxy "outputSpeech")

_card :: ∀ r a. Lens' { card :: a | r } a
_card = prop (SProxy :: SProxy "card")

_type :: ∀ r a. Lens' { type :: a | r } a
_type = prop (SProxy :: SProxy "type")

_reprompt :: ∀ r a. Lens' { reprompt :: a | r} a
_reprompt = prop (SProxy :: SProxy "reprompt")

_shouldEndSession :: ∀ r. Lens' { shouldEndSession :: Boolean | r} Boolean
_shouldEndSession = prop (SProxy :: SProxy "shouldEndSession")

_text :: ∀ r. Lens' { text :: String | r} String
_text = prop (SProxy :: SProxy "text")
