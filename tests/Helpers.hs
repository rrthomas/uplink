

module Helpers where

import Protolude

import Test.Tasty.HUnit

type To a b = a -> b
type From b a = b -> Either Text a

roundTripTest
  :: (Show a, Eq a)
  => To a b
  -> From b a
  -> a
  -> Assertion
roundTripTest to from x =
  Right x @=? from (to x)
