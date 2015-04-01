{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Elm.Record where

import           Data.List
import           Data.Text
import           Data.Time.Clock
import           GHC.Generics

------------------------------------------------------------

class ToElmRecord a where
  toElmRecord :: a -> String

  default toElmRecord :: (Generic a, GenericToElmRecord (Rep a)) => a -> String
  toElmRecord x = genericToElmRecord (from x)

class GenericToElmRecord f where
  genericToElmRecord :: f a -> String

------------------------------------------------------------

instance (Datatype d, GenericToElmRecord f) => GenericToElmRecord (D1 d f) where
  genericToElmRecord d@(M1 x) = "type " ++ datatypeName d ++ " = " ++ genericToElmRecord x

instance (Constructor c, GenericToElmRecord f) => GenericToElmRecord (C1 c f) where
  genericToElmRecord c@(M1 x) = conName c ++ " {\n  " ++ genericToElmRecord x ++ "}"

instance (Selector c, GenericToElmRecord f) => GenericToElmRecord (S1 c f) where
  genericToElmRecord s@(M1 x) = selName s ++ genericToElmRecord x

instance (ToElmRecord c) => GenericToElmRecord (K1 R c) where
  genericToElmRecord (K1 x) = " : " ++ toElmRecord x

instance (GenericToElmRecord f, GenericToElmRecord g) => GenericToElmRecord (f :*: g) where
  genericToElmRecord ~(x :*: y) = genericToElmRecord x ++ "\n  ," ++ genericToElmRecord y

------------------------------------------------------------

instance ToElmRecord Int where
  toElmRecord _ = "Int"

instance ToElmRecord Double where
  toElmRecord _ = "Float"

instance ToElmRecord Float where
  toElmRecord _ = "Float"

instance ToElmRecord String where
  toElmRecord _ = "String"

-- Elm has a Text type, but it's about formatting rather than data.
instance ToElmRecord Text where
  toElmRecord _ = "String"

instance ToElmRecord UTCTime where
  toElmRecord _ = "Date"

instance ToElmRecord (Maybe Int) where
  toElmRecord _ = "Maybe Int"

instance ToElmRecord (Maybe Text) where
  toElmRecord _ = "Maybe String"
