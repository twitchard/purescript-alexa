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
  , BuiltInIntent (..)
  , readBuiltInIntent
  ) where

import Prelude

import Data.Foreign (Foreign, ForeignError(..), F, fail)
import Data.Maybe (Maybe(..))
import Simple.JSON (class ReadForeign, read)

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

-- | An AlexaRequest represents the JSON body provided by Alexa when
-- | invoking your skill.
-- |
-- | If the user invokes your skill simply by naming it ("Alexa, open
-- | <YOUR SKILL> "), without providing further instructions, then
-- | Amazon will issue a LaunchRequest.
-- |
-- | If the user invokes your skill providing specific instructions
-- | ("Alexa, ask <YOUR SKILL> to <DO SOMETHING>"), or if the user
-- | has already entered into your skill and responds to a prompt,
-- | then Amazon will determine the most likely "intent" of those
-- | defined by your skill's "language model" and issue an appropriate
-- | IntentRequest to your skill.
-- |
-- | If your skill is prompting the user for a response, and the
-- | user does not respond within 8 seconds after the prompt (or
-- | the "reprompt", if one was specified), or if the user says
-- | "exit", then Amazon will issue a "SessionEnded" request.
-- | Unlike to LaunchRequests and IntentRequests, you cannot reply
-- | to a SessionEnded request and have speech delivered or a card
-- | displayed to the user. Your reply will be ignored by Alexa.
-- | The purpose of the SessionEnded response is to give you a
-- | chance to do any processing or clean-up, now that the session
-- | is over.
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

instance rfAlexaRequest :: ReadForeign AlexaRequest where
  readImpl f = read f >>= readByType f
    where
      readByType :: Foreign → {request :: {type :: String}} → F AlexaRequest
      readByType f' event
        | event.request.type == "LaunchRequest"       = (map LaunchRequest) $ read f'
        | event.request.type == "IntentRequest"       = (map IntentRequest) $ read f'
        | event.request.type == "SessionEndedRequest" = (map SessionEndedRequest) $ read f'
        | otherwise = fail <<< ForeignError $ "Unknown type " <> event.request.type

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

-- | This data type enumerates all the [standard, built-in intents](https://developer.amazon.com/docs/custom-skills/standard-built-in-intents.html)
-- | supported by Alexa. Amazon will not certify your skill if you do not
-- | handle the AmazonCancelIntent, AmazonStopIntent and AmazonHelpIntent
data BuiltInIntent
  = AmazonCancelIntent
  | AmazonHelpIntent
  | AmazonLoopOffIntent
  | AmazonLoopOnIntent
  | AmazonNextIntent
  | AmazonNoIntent
  | AmazonPauseIntent
  | AmazonPreviousIntent
  | AmazonRepeatIntent
  | AmazonResumeIntent
  | AmazonShuffleOffIntent
  | AmazonShuffleOnIntent
  | AmazonStartOverIntent
  | AmazonStopIntent
  | AmazonYesIntent

-- | This function translates reads a built-in intent by its name.
readBuiltInIntent :: String → Maybe BuiltInIntent
readBuiltInIntent s
  | s == "AMAZON.CancelIntent"     = Just AmazonCancelIntent
  | s == "AMAZON.HelpIntent"       = Just AmazonHelpIntent
  | s == "AMAZON.LoopOffIntent"    = Just AmazonLoopOffIntent
  | s == "AMAZON.LoopOnIntent"     = Just AmazonLoopOnIntent
  | s == "AMAZON.NextIntent"       = Just AmazonNextIntent
  | s == "AMAZON.NoIntent"         = Just AmazonNoIntent
  | s == "AMAZON.PauseIntent"      = Just AmazonPauseIntent
  | s == "AMAZON.PreviousIntent"   = Just AmazonPreviousIntent
  | s == "AMAZON.RepeatIntent"     = Just AmazonRepeatIntent
  | s == "AMAZON.ResumeIntent"     = Just AmazonResumeIntent
  | s == "AMAZON.ShuffleOffIntent" = Just AmazonShuffleOffIntent
  | s == "AMAZON.ShuffleOnIntent"  = Just AmazonShuffleOnIntent
  | s == "AMAZON.StartOverIntent"  = Just AmazonStartOverIntent
  | s == "AMAZON.StopIntent"       = Just AmazonStopIntent
  | s == "AMAZON.YesIntent"        = Just AmazonYesIntent
  | otherwise = Nothing

-- | An AlexaResponse represents the JSON object you must provide
-- | to Amazon in response to an AlexaRequest to have Alexa deliver
-- | speech to the user, or display a "card" to the user.
-- | You may also set "shouldEndSession" to false in order to prompt
-- | the user for a response after your reply is delivered. In this case,
-- | you will receive an IntentRequest when the user replies to your
-- | prompt (or a SessionEndedRequest if the user does not reply) and
-- | whatever value you provide for 'sessionAttributes' will be placed
-- | onto the 'session.attributes' property of that AlexaRequest.
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
