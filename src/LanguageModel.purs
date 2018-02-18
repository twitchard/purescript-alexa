module Amazon.Alexa.Skill.LanguageModel where

-- | LanguageModel is the type of the object that you provide to Amazon
-- | in order to define the voice interface or
-- | ['interaction model']('https://developer.amazon.com/docs/custom-skills/custom-interaction-model-reference.html)
-- | for your skill, and defines the 'intents' and 'slots' that
-- | Alexa should listen for and pass along to your skill.
-- |
-- | ### Intents
-- |
-- | The 'intents' array may include Amazon built-in intents -- for
-- | example:
-- | ```
-- |    {
-- |      "name": "AMAZON.HelpIntent",
-- |      "samples": [],
-- |      "slots": []
-- |    },
-- | ```
-- | For a built-in intent you should not provide any slots or samples.
-- |
-- | The 'intents' array may also include custom intents, that you
-- | define yourself. For example:
-- | ```
-- | "intents": [
-- |    {
-- |      "name": "FavoriteProgrammingLanguageIntent",
-- |      "samples": [
-- |        "My favorite programming language is {Lang}",
-- |        "My favorite language is {Lang}",
-- |        "I like {Lang} the most",
-- |        "The programming language I most enjoy using is {Lang}"
-- |      ],
-- |      "slots": [
-- |        {
-- |          "name": "Lang",
-- |          "type": "ProgrammingLanguage"
-- |        }
-- |      ]
-- |    },
-- |    ...
-- | ```
-- | Provide as many samples as you can think of -- this is how Alexa
-- | learns what phrases to interpret as belonging to an intent.
-- |
-- | ### Slots
-- |
-- | A 'slot' is a placeholder for a user-specified value. Your slot
-- | must have a 'type', which helps Alexa listen for certain values that are
-- | likely to be said. For example, a "types of trees" slot type vs. a
-- | "vacation spots" slot type might help Alexa hear "beech" and not "beach".
-- |
-- | Amazon provides [built-in slot types](https://developer.amazon.com/docs/custom-skills/slot-type-reference.html),
-- | and you may also define your own slot types by defining them inside
-- | the 'types' array in your language model. For example
-- | 
-- | "types": [
-- |   {
-- |     "name": "ProgrammingLanguage",
-- |     "values": [
-- |       {
-- |         "name": {
-- |           "synonyms": [],
-- |           "value": "purescript"
-- |         }
-- |       },
-- |       {
-- |         "name": {
-- |           "synonyms": [],
-- |           "value": "java"
-- |         }
-- |       },
-- |       ...
-- |     ]
-- |  }
-- |
-- | Alexa will use the values of your slot types as hints, but will not
-- | consider them to be exhaustive. Alexa might very well invoke your
-- | skill and specify a value for the slot that does not appear in
-- | the list.
-- |
-- | ### Deployment
-- |
-- | If you are using the [ASK CLI](https://developer.amazon.com/docs/smapi/quick-start-alexa-skills-kit-command-line-interface.html)
-- | tools, you should serialize an object of type
-- | ```
-- | { interactionModel :: { languageModel :: LanguageModel } }
-- | ```
-- | to JSON and place it in (for example) `/models/en-US.json.`
-- |
-- | Your language model can be deployed with the command
-- | ```
-- | ask deploy --target model`
-- | ```
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
