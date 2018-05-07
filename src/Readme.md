Alexa Skills involve
  1. Custom Intents
  2. Built-in Intents
  3. Slots
  4. Slot Names
  4. Built-in Slot Types
  5. Custom Slot Types
  6. Sample Utterances
  7. Sample Values
  8. SlotTypes

Provide an input type

data Input = 
  StopIntent
  MooseIntent { antler :: Antler }

samples = 
  { "MooseIntent":
    [ "I am a moose"
    , "Coming out of the left of my head I have an {antler}"
    , "Coming out of the right of my head I have an {antler}"
    ]
  }

data Antler
  = LeftAntler
  | RightAntler

class Slot a where
  fromSlot :: Maybe String -> a

instance Slot (Maybe a) where
  fromSlot Nothing = Nothing
  fromSlot (Just x) = Just (fromSlot x)

instance antlerSlot :: Slot Antler

1. Every input type must correspond to a custom intent or a built-in intent
2. Built-in intents may not be associated with slots
3. Every slot name
