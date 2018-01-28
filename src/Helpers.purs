module Web.Amazon.Alexa.Helpers where

import Prelude

import Data.Lens (set)
import Data.Maybe (Maybe(..))
import Web.Amazon.Alexa.Types (AlexaResponse)
import Web.Amazon.Alexa.Lens (_outputSpeech, _reprompt, _response, _sessionAttributes, _shouldEndSession)

{-
  An empty AlexaResponse
-}
--  TODO: It's an error for a response to lack both outputSpeech and a card. That would be great to enforce via types.
emptyResponse :: ∀ a. a → AlexaResponse a
emptyResponse sess =
  { version : "1.0"
  , sessionAttributes : sess
  , response :
    { outputSpeech : Nothing
    , card : Nothing
    , reprompt : Nothing
    , shouldEndSession : false
    }
  }

{-
  Augments an AlexaResponse with PlainText speech corresponding to the provided string. That is,
  it returns an AlexaResponse modified such that Alexa will "say" the provided string.
-}
-- TODO: It's probably a mistake for the input to this function to be an AlexaResponse that
-- already has outputSpeech set. It would be great to be able to enforce via types that
-- outputSpeech is only set once in the course of constructing an AlexaResponse.
say :: ∀ a . String → AlexaResponse a → AlexaResponse a
say speech ar = set (_response <<< _outputSpeech) (Just { type : "PlainText", text : speech }) ar

{-
  Augments an AlexaResponse with a reprompt that includes PlainText outputSpeech corresponding
  to the provided string. Alexa will deliver the reprompt if the user says nothing for 8 seconds.
-}
-- TODO: Similarly to 'say', it would be great to enforce that reprompt only gets set once.
reprompt :: ∀ a . String → AlexaResponse a → AlexaResponse a
reprompt speech ar = set (_response <<< _reprompt) (Just { outputSpeech : { type : "PlainText", text : speech }}) ar

{- Sets the 'shouldEndSession' boolean on the AlexaResponse to false. This will cause
   Alexa, after she has delivered the response, to listen for the user's next utterance
   and deliver it to the skill. The user need not say the wake word again. Furthermore,
   on the user's next utterance, the AlexaRequest provided to the skill will contain
   whatever 'session' object is attached to this AlexaResponse.
-}
keepGoing :: ∀ a. AlexaResponse a → AlexaResponse a
keepGoing ar = set (_response <<< _shouldEndSession) false ar

{- Sets the 'shouldEndSession' boolean on the AlexaResponse to false. This will cause
   Alexa not to listen for another utterance after the response is delivered. If the
   user wishes to continue interacting with the skill, he or she will have to say
   the wake word (Alexa) and invoke the skill by name
-}
stopGoing :: ∀ a. AlexaResponse a → AlexaResponse a
stopGoing ar = set (_response <<< _shouldEndSession) true ar

{- Replaces the 'session' on the AlexaResponse with the provided value
-}
setSession :: ∀ a. a → AlexaResponse a → AlexaResponse a
setSession sess ar = set (_sessionAttributes) sess ar
