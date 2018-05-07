module Test.Main where

import Prelude

import Amazon.Alexa.LanguageModel (Intent(..), LanguageModel(..), SlotType(..), SlotValue(..), SlotValueName(..))
import Amazon.Alexa.LanguageModel (Slot(..)) as LM
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Array.NonEmpty (NonEmptyArray, cons')
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, from)
import Data.StrMap (StrMap, singleton)
import Debug.Trace (spy)
import Skill (class Slot, languageModel, skill)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal) as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Type.Prelude (Proxy(..))

data TestInput
  = Stop
  | MyNameIs { name :: NameSlot }


derive instance gTestInput :: Generic TestInput _

myFrom :: TestInput → _ 
myFrom = from

newtype NameSlot = NameSlot String

instance slotNameSlot :: Slot NameSlot where
  slotTypeName _ = (spy "NameSlot")
  slotValues _ = [ "richard", "angelica", "mozzarella" ] <#> \name → { value : name, synonyms: [] }

samples :: StrMap (NonEmptyArray String)
samples = 
  singleton "MyNameIs" $
    cons' "My name is {name}"
        [ "I am called {name}"
        ]

slotSamples :: StrMap (NonEmptyArray (NonEmptyArray String))
slotSamples =
  singleton "name" $
    cons' (pure "richard")
        [ (pure "angelica")
        , (pure "mozzarella")
        ]

main :: forall t6.
   Eff
     ( console :: CONSOLE
     , testOutput :: TESTOUTPUT
     , avar :: AVAR
     | t6
     )
     Unit
main = runTest do
  suite "SimpleSkill" do
    test "LanguageModel" do
      let testSkill = skill (Proxy :: Proxy TestInput) "foo" samples --slotSamples
      let result = languageModel testSkill
      let (expectedResult :: LanguageModel) = LanguageModel
            { "invocationName" : "foo"
            , "intents":
              [ Intent { name : "AMAZON.StopIntent"
                       , slots : []
                       , samples : []
                       }
              , Intent { name : "MyNameIsIntent"
                       , slots :
                         [ LM.Slot { name: "name"
                           , type: "NameSlot"
                           }
                         ]
                       , samples :
                         [ "My name is {name}"
                         , "I am called {name}"
                         ]
                       }
              ]
            , "types":
              [ SlotType { "name" : "NameSlot"
                , "values" : [ SlotValue { "name": SlotValueName { "value": "richard", "synonyms": [] } }
                             , SlotValue { "name": SlotValueName { "value": "angelica", "synonyms": [] } }
                             , SlotValue { "name": SlotValueName { "value": "mozzarella", "synonyms": [] } }
                             ]
                }
              ]
            }
      Assert.equal (Right expectedResult) result
