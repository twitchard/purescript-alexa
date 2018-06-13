module Amazon.Alexa.Manifest where

import Data.Maybe (Maybe)
import Data.Map (Map)

type Manifest =
  { skillManifest :: 
    { publishingInformation ::
      { locales :: Map String Locale
      , testingInstructions :: String
      , category :: String
      , distributionCountries :: Array String
      }
    , apis ::
      { custom ::
        { endpoint :: Endpoint
        , interfaces :: Maybe (Array { "type" :: String })
        }
      } 
    , manifestVersion :: String
    , permissions :: Array Permission
    , privacyAndCompliance ::
      { allowsPurchases :: Boolean
      , usesPersonalInfo :: Boolean
      , isChildDirected :: Boolean
      , isExportCompliant :: Boolean
      , containsAds :: Boolean
      , locales :: Map String LocalePrivacy
      }
    , events :: Maybe { endpoint :: Endpoint }
    , subscriptions :: Array Subscription
    , regions :: Map String Endpoint
    }
  }

type Locale =
  { name :: String
  , summary :: String
  , examplePhrases :: Array String
  , description :: String
  , keywords :: Array String
  }

type Permission = {} -- TODO

type Subscription = { eventName :: String }

type Endpoint = 
  { uri :: Maybe String
  , sourceDir :: Maybe String
  }

type LocalePrivacy = 
  { privacyPolicyUrl :: String
  , termsOfUseUrl :: String
  }

