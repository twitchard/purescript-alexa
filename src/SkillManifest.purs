module Amazon.Alexa.Skill.Manifest where

import Data.Maybe (Maybe)
import Data.StrMap (StrMap)

type Manifest =
  { manifest :: 
    { publishingInformation ::
      { locales :: StrMap Locale
      , isAvailableWorldWide :: Boolean
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
    , events :: Maybe { endpoint :: Endpoint }
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
  { uri :: Maybe String
  , sourceDir :: Maybe String
  }

type LocalePrivacy = 
  { privacyPolicyUrl :: String
  , termsOfUseUrl :: String
  }

