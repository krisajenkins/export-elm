{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Elm.Decoder where

import qualified Data.Char       as Char
import           Data.List
import           Data.Text
import           Data.Time.Clock
import           GHC.Generics

asVariableName :: String -> String
asVariableName = fmap Char.toLower

------------------------------------------------------------

class ToElmDecoder a where
  toElmDecoder :: a -> String

  default toElmDecoder :: (Generic a, GenericToElmDecoder (Rep a)) => a -> String
  toElmDecoder x = genericToElmDecoder (from x)

class GenericToElmDecoder f where
  genericToElmDecoder :: f a -> String

------------------------------------------------------------

instance (Datatype c,GenericToElmDecoder f) => GenericToElmDecoder (D1 c f) where
  genericToElmDecoder d@(M1 x) =
    variableName ++ "Decoder : Decoder " ++ typeName ++ "\n" ++
    variableName ++ "Decoder = " ++ genericToElmDecoder x
    where typeName = datatypeName d
          variableName = asVariableName typeName

instance (Constructor c, GenericToElmDecoder f) => GenericToElmDecoder (C1 c f) where
  genericToElmDecoder c@(M1 x) =  conName c ++ "\n  `map`  " ++ genericToElmDecoder x

instance (Selector c, GenericToElmDecoder f) => GenericToElmDecoder (S1 c f) where
  genericToElmDecoder s@(M1 x) = "(\"" ++ selName s ++ "\"" ++ genericToElmDecoder x ++ ")"

instance (ToElmDecoder c) => GenericToElmDecoder (K1 R c) where
  genericToElmDecoder (K1 x) = " := " ++ toElmDecoder x

instance (GenericToElmDecoder f, GenericToElmDecoder g) => GenericToElmDecoder (f :*: g) where
  genericToElmDecoder ~(x :*: y) = genericToElmDecoder x ++ "\n  `apply` " ++ genericToElmDecoder y

------------------------------------------------------------

instance ToElmDecoder Int where
  toElmDecoder _ = "int"

instance ToElmDecoder Double where
  toElmDecoder _ = "float"

instance ToElmDecoder Float where
  toElmDecoder _ = "float"

instance ToElmDecoder String where
  toElmDecoder _ = "string"

instance ToElmDecoder UTCTime where
  toElmDecoder _ = "date"

-- Elm has a Text type, but it's about formatting rather than data.
instance ToElmDecoder Text where
  toElmDecoder _ = "string"

instance ToElmDecoder (Maybe Int) where
  toElmDecoder _ = "maybe int"

instance ToElmDecoder (Maybe Text) where
  toElmDecoder _ = "maybe string"
