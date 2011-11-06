{-# LANGUAGE DeriveDataTypeable #-}
-- | Utilities for serializing enum values.

module Data.Enum.Print where

import Control.Arrow
import Data.Char  (toLower)
import Text.Regex
import Data.Maybe
import Control.Exception
import Data.Typeable

-- | Print an Enum for external use.
showEnum :: (Show a) => a -> String
showEnum = flatten . show where
  flatten = map toLower . upperToDash
  upperToDash = flip (subRegex (mkRegex "([a-z0-9])([A-Z])")) "\\1_\\2"
              . flip (subRegex (mkRegex "_")) "."
              
-- | Parses the enum or returns nothing.
readEnum :: (Enum a,Show a,Read a) => String -> Maybe a
readEnum = (`lookup` map (showEnum &&& id) [toEnum 0 ..])

-- | Throws an exception if it cannot parse.
readEnumForce :: (Enum a,Show a,Read a,Monad m) => String -> String -> m a
readEnumForce desc str =
  maybe (throw (EnumReadFail desc str)) return (readEnum str)

data EnumReadFail = EnumReadFail String String
  deriving (Typeable,Show)
instance Exception EnumReadFail
