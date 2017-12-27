module Web.Amazon.Alexa
  ( AlexaRequest (..)
  , AlexaLaunchRequest
  , AlexaBody
  , AlexaIntentRequest
  , AlexaSessionEndedRequest
  , AlexaRequestCommon
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

type AlexaBody =
  ( version :: String
  , session :: AlexaSession
  , context :: AlexaContext
  )

type AlexaContext = {} -- TODO
type AlexaApplication = {} -- TODO
type AlexaUser = {} -- TODO

data AlexaRequest
  = LaunchRequest { request :: AlexaLaunchRequest | AlexaBody }
  | IntentRequest { request :: AlexaIntentRequest | AlexaBody }
  | SessionEndedRequest { request :: AlexaSessionEndedRequest | AlexaBody }
  -- | TODO AlexaAudioPlayer...
  -- | TODO AlexaVideoApp...
  -- | TODO PlaybackController...

type AlexaRequestCommon =
  ( requestId :: String
  , timestamp :: String
  , locale :: String
  )

type AlexaLaunchRequest = {|AlexaRequestCommon}

type AlexaIntentRequest = 
  { dialogState :: Maybe String
  , intent :: AlexaIntent
  | AlexaRequestCommon
  }

type AlexaSessionEndedRequest =
  { reason :: String
  , error :: AlexaError
  | AlexaRequestCommon
  }

type AlexaIntent =
  { name :: String
  , confirmationStatus :: String
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
