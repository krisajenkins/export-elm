module Elm (module Elm.Record, module Elm.Decoder) where

import           Elm.Decoder
import           Elm.Record

-- Despite the terrible naming convention, Generics seems quite useful. It autogenerates structures like:
--
--   Rep Person () :: *
--   = M1
--        D
--        Elm.D1Person
--        (M1
--           C
--           Elm.C1_0Person
--           (M1 S Elm.S1_0_0Person (K1 R (Maybe [Char]))
--            :*: M1 S Elm.S1_0_1Person (K1 R Int)))
--        ()
--
-- Notes: Use ':kind! Rep Person ()' to get the generic representation.
--
-- From that we can walk the tree to generate almost any code with
--  about 5 structure-instances (M1, :*:, :+:, etc.)
