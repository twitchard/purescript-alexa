module Amazon.Alexa.Manifest where

import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.StrMap (StrMap)

type Manifest =
  { skillManifest :: 
    { publishingInformation ::
      { locales :: StrMap Locale
      , testingInstructions :: String
      , category :: String
      , distributionCountries :: Array String
      }
    , apis :: { custom :: { endpoint :: Endpoint } } 
    , manifestVersion :: String
    , permissions :: Array Permission
    , privacyAndCompliance ::
      { allowsPurchases :: Boolean
      , usesPersonalInfo :: Boolean
      , isChildDirected :: Boolean
      , isExportCompliant :: Boolean
      , containsAds :: Boolean
      , locales :: StrMap LocalePrivacy
      }
    , events :: NullOrUndefined { endpoint :: Endpoint }
    , subscriptions :: Array Subscription
    , regions :: StrMap Endpoint
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
  { uri :: NullOrUndefined String
  , sourceDir :: NullOrUndefined String
  }

type LocalePrivacy = 
  { privacyPolicyUrl :: String
  , termsOfUseUrl :: String
  }

