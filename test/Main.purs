module Test.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe)
import EasyAlexa (class Slot)

data Input
  = Cancel
  | Increment { otherNumber :: Maybe IntSlot }

derive instance genericInput :: Generic Input _

newtype IntSlot = IntSlot Int

instance slotIntSlot :: Slot IntSlot where 
  parseSlot = map IntSlot <<< fromString
  name _ = "int"
  slotValues _ =
    [ { value : "one"
      , synonyms : []
      }
    ]
