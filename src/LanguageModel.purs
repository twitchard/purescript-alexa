module Amazon.Alexa.Skill.LanguageModel where

type LanguageModel =
  { invocationName :: String
  , intents :: Array
      { name :: String
      , slots :: Array
          { name :: String
          , type :: String
          }
      , samples :: Array String
      }
  , types :: Array
      { values :: Array 
          { name ::
              { value :: String
              , synonyms :: Array String
              }
          }
      , name :: String
      }
  }
