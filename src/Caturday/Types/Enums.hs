module Caturday.Types.Enums where

import Data.Enum.Print
import Database.HaskellDB.Get
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Query

data ArticleType
  = ResearchArticle
  | Editorial
  deriving (Show,Enum,Read,Eq)

instance GetValue ArticleType where
  getValue = getValueWith readEnum "ArticleType"

instance ShowConstant ArticleType where
  showConstant = StringLit . showEnum
