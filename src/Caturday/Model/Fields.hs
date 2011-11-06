{-# LANGUAGE TemplateHaskell #-}

-- | All database fields.

module Caturday.Model.Fields where

import qualified Caturday.Types.Enums          as Types (ArticleType)

import           Database.HaskellDB.PostgreSQL
import           Database.HaskellDB.TH

-- Keys.
field "Id" "id" "id" [t|Int|]

-- Data fields.
field "Title" "title" "title" [t|String|]
field "Count" "count" "count" [t|Int|]

-- | Search fields.
field "Textsearchable" "textsearchable" "textsearchable_index_col" [t|TSVector|]

-- | Enum types.
field "ArticleType" "articleType" "type" [t|Types.ArticleType|]
