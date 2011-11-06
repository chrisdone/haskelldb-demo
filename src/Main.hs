module Main where

import qualified Caturday.Model.Fields             as F
import qualified Caturday.Model.Tables             as T
import           Caturday.Types.Enums

import           Database.HaskellDB
import           Database.HaskellDB.HDBRec
import           Database.HaskellDB.Extra

import           Database.HaskellDB.HDBC
import           Database.HaskellDB.Pagination
import           Database.HaskellDB.Sql.PostgreSQL
import           Database.HaskellDB.PostgreSQL
import           Database.HDBC.PostgreSQL          (connectPostgreSQL)

import           Data.Pagination
import           Data.Default

simpleSelection = do
  table T.content

simpleProjection = do
  content <- table T.content
  project $ F.id << content!F.id

simpleProjection2 = do
  content <- table T.content
  project $ F.id    << content!F.id
          # F.title << content!F.title

simpleStringProjection = do
  content <- table T.content
  project $ F.id    << constant 123
          # F.title << constant "Hello, World!"

simpleRestriction = do
  content <- table T.content
  restrict $ content!F.title .==. val "Coco Jambo"
  return content

simpleDoubleSelection = do
  table T.content
  table T.content

simpleDelete conn = do
  delete conn
         T.content
         (\content -> content!F.title .==. val "Coco Jambo")

simpleUpdate conn = do
  update conn
         T.content
         (\content -> content!F.title .==. val "Coco Jambo")
         (\content -> F.title << val "What is Love?")

simpleInsert conn = do
  insert conn
         T.content
         ( F.id    << val 123
         # F.title << val "What is Love?"
         # F.articleType << val Editorial)

getArticles conn pn q = fmap (map (!F.title)) $ query conn $ do
  article <- getContent q
  paginate pn
  return article

getArticlesCount conn q = fmap (sum . map (!F.count)) $ query conn $ do
  article <- getContent q
  project $ F.count << count (article!F.id)

getContent q = do
  article <- table T.content
  restrict $ article!F.title .==. val q
  return article

getContentFullText q = do
  article <- table T.contentSearchable
  restrict $ article!F.textsearchable .@@. (to_tsquery (val q))
  order [descExpr $ ts_rank_cd (article!F.textsearchable)
                               (to_tsquery (val q))]
  return article

restrictOnArticleType conn = query conn $ do
  content <- table T.content
  restrict $ content!F.articleType .==. val Editorial
  return content

withDB opts = hdbcConnect generator (connectPostgreSQL conninfo)
  where conninfo = unwords [ k ++ "=" ++ v | (k,v) <- opts]

opts = [("host","localhost")
       ,("user","your_user")
       ,("password","your_pass")
       ,("dbname","your_db")]

main = putStrLn "Enter whichever code you want to try out here."
