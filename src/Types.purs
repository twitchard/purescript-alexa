module Web.Amazon.Alexa.Types
  ( AlexaRequest (..)
  , AlexaLaunchRequest
  , AlexaIntentRequest
  , AlexaSessionEndedRequest
  , AlexaSession
  , AlexaContext
  , AlexaIntent
  , AlexaError
  , AlexaApplication
  , AlexaUser
  , AlexaResponse
  ) where

import Prelude

import Data.Foreign (Foreign, ForeignError(..), F, fail)
import Data.Maybe (Maybe)
import Simple.JSON (class ReadForeign, read)

-- REQUEST TYPES
type AlexaSession =
  { new :: Boolean
  , sessionId :: String
  , application :: AlexaApplication
  , attributes :: Foreign
  , user :: AlexaUser
  }

type AlexaContext = {} -- TODO
type AlexaApplication = {} -- TODO
type AlexaUser =
  { userId :: String
  , accessToken :: Maybe String
  -- , permissions :: TODO
  }

data AlexaRequest
  = LaunchRequest
      { request :: AlexaLaunchRequest
      , version :: String
      , session :: AlexaSession
      , context :: AlexaContext
      }
  | IntentRequest
      { request :: AlexaIntentRequest
      , version :: String
      , session :: AlexaSession
      , context :: AlexaContext
      }
  | SessionEndedRequest
      { request :: AlexaSessionEndedRequest
      , version :: String
      , session :: AlexaSession
      , context :: AlexaContext
      }

type AlexaLaunchRequest =
  { requestId :: String
  , timestamp :: String
  , locale :: String
  }

type AlexaIntentRequest = 
  { dialogState :: Maybe String
  , intent :: AlexaIntent
  , requestId :: String
  , timestamp :: String
  , locale :: String
  }

type AlexaSessionEndedRequest =
  { reason :: String
  , error :: Maybe AlexaError
  , requestId :: String
  , timestamp :: String
  , locale :: String
  }

type AlexaIntent =
  { name :: String
  , confirmationStatus :: Maybe String
  , slots :: Foreign
  }

type AlexaError = 
  { type :: String
  , message :: String
  }

instance rfAlexaRequest :: ReadForeign AlexaRequest where
  readImpl f = read f >>= readByType f 
    where
      readByType :: Foreign → {request :: {type :: String}} → F AlexaRequest
      readByType f' event
        | event.request.type == "LaunchRequest"       = (map LaunchRequest) $ read f'
        | event.request.type == "IntentRequest"       = (map IntentRequest) $ read f'
        | event.request.type == "SessionEndedRequest" = (map SessionEndedRequest) $ read f' 
        | otherwise = fail <<< ForeignError $ "Unknown type " <> event.request.type

-- RESPONSE TYPES
type AlexaResponse a =
  { version :: String
  , sessionAttributes :: a
  , response ::
    { outputSpeech :: Maybe
      { type :: String
      , text :: String
      }
    , card :: Maybe
      { type :: String
      , title :: String
      , content :: String
      }
    , reprompt :: Maybe
      { outputSpeech ::
        { type :: String
        , text :: String
        }
      }
    , shouldEndSession :: Boolean
    }
  }
