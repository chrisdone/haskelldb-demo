{-# LANGUAGE TemplateHaskell #-}

-- | Database tables and entities.

module Caturday.Model.Tables where

import Caturday.Model.Fields as Fields

import Database.HaskellDB.TH
import Prelude ()

-- | Content table.
table "content" "eudl.content"
  ['id
  ,'title
  ,'articleType
  ]

-- | Content table with searchable full text field.
table "contentSearchable" "eudl.content"
  ['id
  ,'title
  ,'textsearchable
  ]
