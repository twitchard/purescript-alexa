module Amazon.Alexa.Manifest where

import Data.Maybe (Maybe)
import Foreign.Object (Object)

type Manifest =
  { skillManifest :: 
    { publishingInformation ::
      { locales :: Object Locale
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
      , locales :: Object LocalePrivacy
      }
    , events :: Maybe { endpoint :: Endpoint }
    , subscriptions :: Array Subscription
    , regions :: Object Endpoint
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

