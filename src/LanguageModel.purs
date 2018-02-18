module Amazon.Alexa.Skill.LanguageModel where

-- | LanguageModel is the type of the object that you provide to Amazon
-- | in order to define the voice interface for your skill.
-- |
-- | If you are using the [ASK CLI](https://developer.amazon.com/docs/smapi/quick-start-alexa-skills-kit-command-line-interface.html)
-- | tools, you should serialize an object of type
-- | ```
-- | { interactionModel :: { languageModel :: LanguageModel } }
-- | ```
-- | to JSON and place it in (for example) /models/en-US.json.
-- |
-- | Your language model can be deployed with the command
-- | `ask deploy --target model`
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
