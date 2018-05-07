module Skill where

import Prelude

import Amazon.Alexa.LanguageModel (Intent(..), LanguageModel(..), Slot(..), SlotType(..), SlotValue(..), SlotValueName(..))
import Data.Array (elem, fromFoldable, nub, null, partition)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic, Constructor, Field, NoArguments, Product, Rec, Sum)
import Data.List (List(..), reverse, (:))
import Data.List (fromFoldable) as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap (StrMap, keys, lookup, member)
import Data.String (fromCharArray, toCharArray)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (for_)
import Type.Prelude (Proxy(Proxy))
import Type.Proxy (Proxy)

newtype Name = Name String

class Slot a where 
  slotTypeName :: (Proxy a) → String
  slotValues :: (Proxy a) → Array { value :: String, synonyms :: Array String }

data Skill a = Skill
  { invocationName :: String
  , samples :: StrMap (NonEmptyArray String)
  }

skill :: ∀ a
  . (Proxy a)
  → String
  → StrMap (NonEmptyArray String)
  → Skill a
skill _ invocationName samples {- slotSamples -} = Skill {invocationName, samples}

type IntentRec =
  { intentName :: String
  , slots :: Array SlotRec
  }

class IntentSum rep where
  intentList
    :: (Proxy rep)
    → NonEmptyArray IntentRec

instance emptyIntentSum ::
  ( IsSymbol name
  ) => IntentSum (Constructor name NoArguments) where
    intentList _ = pure
                     { intentName
                     , slots : []
                     }
      where
        intentName = (reflectSymbol (SProxy :: SProxy name))

instance slotsSumNames ::
  ( IsSymbol name
  , Slots slots
  ) => IntentSum (Constructor name (Rec slots)) where
    intentList _ = pure
                     { intentName
                     , slots : fromFoldable $ slotList (Proxy :: Proxy slots)
                     }
      where
        intentName = (reflectSymbol (SProxy :: SProxy name))

instance sumIntentSum :: 
  ( IntentSum a
  , IntentSum b
  ) => IntentSum (Sum a b) where
  intentList _ = aIntentNames <> bIntentNames
    where
      aIntentNames = intentList (Proxy :: Proxy a)
      bIntentNames = intentList (Proxy :: Proxy b)

type SlotRec =
  { name :: String
  , typeName :: String
  , values :: Array { value :: String, synonyms :: Array String}
  }

class Slots rep where
  slotList :: (Proxy rep) → NonEmptyArray SlotRec

instance zSlots ::
  ( IsSymbol sname
  , Slot ty
  ) => Slots (Field sname ty) where
  slotList _ = pure { name, typeName, values }
    where
      name = reflectSymbol (SProxy :: SProxy sname)
      typeName = slotTypeName (Proxy :: Proxy ty)
      values = slotValues (Proxy :: Proxy ty)

instance sSlots ::
  ( IsSymbol sname
  , Slots tail
  , Slot ty
  ) => Slots (Product (Field sname ty) tail) where

  slotList _ =
    pure { name, typeName, values } <>
    (slotList (Proxy :: Proxy tail))
    where
      name = reflectSymbol (SProxy :: SProxy sname)
      typeName = slotTypeName (Proxy :: Proxy ty)
      values = slotValues (Proxy :: Proxy ty)


languageModel :: ∀ a rep
  . IntentSum rep
  => Generic a rep
  => Skill a → Either String LanguageModel
languageModel (Skill {invocationName, samples}) = do
  builtinIntentsCantHaveSlots
  customIntentsMustHaveSamples
  noExtraIntentSamples
  noExtraSlotNames
  pure $ LanguageModel
           { invocationName
           , intents
           , types
           }
  where
    intents = renderIntent <$> allIntentRecs # fromFoldable
    types = renderSlotType <$> allSlotRecs

    allIntentRecs :: NonEmptyArray IntentRec
    allIntentRecs = intentList (Proxy :: Proxy rep)

    allSlotRecs :: Array SlotRec
    allSlotRecs = (join <<< fromFoldable) $ allIntentRecs <#> (\i → i.slots)

    renderIntent :: IntentRec → Intent
    renderIntent i = Intent { name , slots , samples : samples' }
      where
        name = 
          if elem i.intentName standardBuiltins
            then "AMAZON." <> i.intentName <> "Intent"
            else i.intentName <> "Intent"
        slots = i.slots <#> \{name: slotName, typeName} → Slot { name: slotName, type: typeName}
        samples' = (lookup i.intentName samples) <#> fromFoldable # fromMaybe []

    renderSlotType :: SlotRec → SlotType
    renderSlotType sr = SlotType { name, values }
      where
        name = sr.typeName
        values = sr.values <#> \v →
          SlotValue
            { name: SlotValueName { value : v.value, synonyms: v.synonyms } }

    partitionedIntentRecs = 
      partition (\i → elem i.intentName standardBuiltins) (fromFoldable allIntentRecs)
    builtinIntentRecs = partitionedIntentRecs.yes
    customIntentRecs = partitionedIntentRecs.no

    failUnless :: Boolean → String → Either String Unit
    failUnless cond msg = if cond then pure unit else Left msg

    builtinIntentsCantHaveSlots = 
      for_ builtinIntentRecs \i →
        failUnless
          (null i.slots)
          ("built-in intent " <> i.intentName <> " should not have a slot")
          
    customIntentsMustHaveSamples =
      for_ customIntentRecs \i →
        failUnless
          (member i.intentName samples)
          ("custom intent " <> i.intentName <> " must have sample utterances")

    noExtraIntentSamples = 
      for_ (keys samples) \name →
        failUnless
          (elem name usedIntentNames)
          ("sample utterances were provided for an intent called " <> name <>
           ", which did not correspond to a constructor in the input type"
          )
      where 
        usedIntentNames = customIntentRecs <#> \i → i.intentName

    noExtraSlotNames =
      for_ customIntentRecs \i → do
        utterances <- lookup i.intentName samples
                        # note ("custom intent " <> i.intentName <>
                                " must have sample utterances")
        let usedInUtterances = (nub <<< join <<< fromFoldable) $ (parseSlots <$> utterances)
        let usedInIntentDefinition = i.slots <#> \s → s.name
        for_ usedInUtterances \name → do
          failUnless
            (elem name usedInIntentDefinition)
            ("A slot placeholder {" <> name <>
             "} was inside a sample utterance for the intent " <> i.intentName <>
             " but was not a field name on the intent's associated record"
            )
        for_ usedInIntentDefinition \name → do
          failUnless
            (elem name usedInUtterances)
            ("A slot named " <> name <>
              " was a field name on the record associated with the intent " <> i.intentName <>
              " but none of the sample utterances for that intent contained" <>
                " a slot placeholder {" <> name <> "}"
            )

    parseSlots :: String → Array String
    parseSlots utterance =
      (fromFoldable <<< reverse) $
        step Nil Nothing      chars
      where
        step acc _            Nil        = acc
        step acc Nothing      ('{' : xs) = step acc        (Just Nil      ) xs
        step acc Nothing      ( x  : xs) = step acc        Nothing          xs
        step acc (Just inner) ('}' : xs) = step (word:acc) Nothing          xs
          where word = (fromCharArray <<< fromFoldable <<< reverse) $ inner
        step acc (Just inner) ( x  : xs) = step acc        (Just (x:inner)) xs

        chars = (List.fromFoldable (toCharArray utterance))
        
    allSlotNames = intentList

standardBuiltins :: Array String
standardBuiltins =
  [ "Cancel"
  , "Fallback"
  , "Help"
  , "LoopOff"
  , "LoopOn"
  , "Next"
  , "No"
  , "Pause"
  , "Previous"
  , "Repeat"
  , "Resume"
  , "ShuffleOff"
  , "ShuffleOn"
  , "StartOver"
  , "Stop"
  , "Yes"
  ]
