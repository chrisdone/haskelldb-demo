-----------------------------------------------------------
-- |
-- Module      :  Optimize
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non portable
--
-- Defines standard optimizations performed on PrimQuery's
-- (relational expressions).
--
-- 
-----------------------------------------------------------

module Database.HaskellDB.Optimize (optimize, optimizeCriteria) where

import Control.Exception (assert)
import Data.List (intersect, (\\), union, nub)
import Database.HaskellDB.PrimQuery

-- | Optimize a PrimQuery
optimize :: PrimQuery -> PrimQuery
optimize = hacks
           . mergeProject
           . removeEmpty
           . removeDead
           . pushRestrict
           . optimizeExprs

-- | Optimize a set of criteria.
optimizeCriteria :: [PrimExpr] -> [PrimExpr]
optimizeCriteria = filter (not . exprIsTrue) . map optimizeExpr


-- | Hacks needed by some back-ends.
--   FIXME: this is silly.
hacks :: PrimQuery -> PrimQuery
hacks = includeOrderFieldsInSelect

-- | HACK: All fields that we order by must also be in the result in 
--   PostgreSQL, since we use SELECT DISTINCT.
includeOrderFieldsInSelect :: PrimQuery -> PrimQuery
includeOrderFieldsInSelect = 
    foldPrimQuery (Empty, BaseTable, proj, Restrict, Binary, Group, Special)
    where 
      proj ass p = Project (ass++ass') p
          where ass' = [(a, AttrExpr a) | a <- new ]
                new = orderedBy p \\ concatMap (attrInExpr . snd) ass
                orderedBy = foldPrimQuery ([], \_ _ -> [], \_ _ -> [],
                                           \_ _ -> [], \_ _ _ -> [], \_ _ -> [], special)
                special (Order es) p = attrInOrder es `union` p
                special _ p = p

-- | Remove unused attributes from projections.
removeDead :: PrimQuery -> PrimQuery
removeDead query
        = removeD (attributes query) query

removeD :: Scheme -- ^ All live attributes (i.e. all attributes 
		  -- that are in the result of the query)
	-> PrimQuery
	-> PrimQuery
removeD live (Binary op query1 query2)
        = assert (all (`elem` (live1 ++ live2)) live)
		 Binary op (removeD live1 query1) (removeD live2 query2)
	  where
          live1 = live `intersect` attributes query1
	  live2 = live `intersect` attributes query2

removeD live (Project assoc query)
        = assert (all (`elem` (map fst newAssoc)) live)
          Project newAssoc (removeD newLive query)
        where
	  -- The live attributes in the nested query.
	  newLive :: Scheme
          newLive       = concat (map (attrInExpr . snd) newAssoc)

	  -- All associations that define attributes that are live
	  -- or that will be put in a GROUP BY clause.
	  -- These are the associations that will be kept.
	  newAssoc :: Assoc
          newAssoc = filter (isLive live) assoc

removeD live (Restrict x query)
        = Restrict x (removeD (live ++ attrInExpr x) query)

removeD live (Special (Order xs) query)
	= Special (Order xs) (removeD (live ++ attrInOrder xs) query)

removeD live (Group cols query) = Group newAssoc (removeD newLive query)
  where
    newLive :: Scheme
    newLive = concat (map (attrInExpr . snd) newAssoc)
    newAssoc :: Assoc
    newAssoc = filter (isLive live) cols

removeD live query = query

-- | Determines if the given column (attribute/expression pair)
-- exists in the scheme given.
isLive :: Scheme -> (Attribute,PrimExpr) -> Bool
isLive live (attr,expr) = attr `elem` live

schemeOf :: (PrimExpr -> Bool) -> Assoc -> Scheme
schemeOf f = map fst . filter (f . snd)

-- | Remove unused parts of the query
removeEmpty :: PrimQuery -> PrimQuery
removeEmpty 
        = foldPrimQuery (Empty, BaseTable, project, restrict, binary, group, special)
        where
          -- Messes up queries without a table, e.g. constant queries
	  -- disabled by Bjorn Bringert 2004-04-08
          --project assoc Empty   = Empty
          project assoc query   | null assoc    = query
                                | otherwise     = Project assoc query

          restrict x Empty      = Empty
          restrict x query      = Restrict x query

          special op Empty	= Empty
          special op query	= Special op query

          binary op Empty query   = case op of Times      -> query
                                               _          -> Empty

          binary op query Empty   = case op of Times      -> query
                                               Difference -> query
                                               _          -> Empty
          binary op query1 query2 = Binary op query1 query2
          group _ Empty = Empty
          group cols query = Group cols query

-- | Collapse adjacent projections
mergeProject :: PrimQuery -> PrimQuery
mergeProject q
        = foldPrimQuery (Empty, BaseTable, project, Restrict, Binary, Group, Special) q
        where
          project assoc1 (Project assoc2 query)
                | equal assoc1 assoc2 = Project assoc2 query
             	| safe assoc1 query = Project (subst assoc1 assoc2) query
             	where

	  project assoc query@(Binary Times _ _) = Project assoc query
	  project assoc (Binary op (Project assoc1 query1)
          		           (Project assoc2 query2))
          	| safe assoc1 query1 && safe assoc2 query2
          		= Binary op (Project newAssoc1 query1)
          		            (Project newAssoc2 query2)
          		where
          		  newAssoc1  = subst assoc assoc1
          		  newAssoc2  = subst assoc assoc2
          
          project assoc query
                = Project assoc query
	 
          -- Replace columns in a1 with
          -- expressions from a2. 
	  subst :: Assoc -- ^ Association that we want to change
		-> Assoc -- ^ Association containing the substitutions
		-> Assoc
          subst a1 a2
                = map (\(attr,expr) -> (attr, substAttr a2 expr)) a1

          -- It is safe to merge two projections in two cases. 
          --  1. All columns in the outer projections are attributes, with no
          --     computation. This means they merely copy values from the inner
          --     projection to the outer and can be eliminated.
          --  2. The inner projection does not group on any columns.
          safe :: Assoc -- ^ Outer projection's columns.
               -> PrimQuery -- ^ Inner projection's query.
               -> Bool
          safe outer innerQuery = all isAttr outer || null (groups innerQuery)
            where
              isAttr (_, AttrExpr _) = True
              isAttr _ = False
          
          -- Are two associations equal?
          equal :: Assoc -> Assoc -> Bool
          equal assoc1 assoc2 = length assoc1 == length assoc2 &&
                                (all (\((a1, _),(a2, _)) -> a1 == a2) $ zip assoc1 assoc2)
          -- Returns grouped columns for a projection. 
          groups :: PrimQuery -> Scheme
          groups = foldPrimQuery ([], \ _ _ -> [],
                                  \ _ _ -> [], restrict, 
                                  \ _ _ _ -> [], group, special)
            where
              restrict _ rest = rest
              group cols _ = map fst cols
              special _ rest = rest


-- | Push restrictions down through projections and binary ops.
pushRestrict :: PrimQuery -> PrimQuery

pushRestrict (Binary op query1 query2)
        = Binary op (pushRestrict query1) (pushRestrict query2)

pushRestrict (Project assoc query)
        = Project assoc (pushRestrict query)

pushRestrict (Group assoc query)
        = Group assoc (pushRestrict query)

-- restricts

pushRestrict (Restrict x (Project assoc query))
	     | safe = Project assoc (pushRestrict (Restrict expr query))
        where
	  -- since we passed a project, we need to replace all attributes
	  -- with the expression they are bound to by the project
          expr  = substAttr assoc x
	  -- aggregate expressions are not allowed in restricts
	  safe = not (isAggregate expr) && not (hasAggregates query)
          hasAggregates (Project _ _) = False
          hasAggregates (BaseTable  _ _)  = False
          hasAggregates (Restrict _ qry) = hasAggregates qry
          hasAggregates (Group _ _) = True
          hasAggregates (Binary _ left right) = hasAggregates left || hasAggregates right
          hasAggregates (Special   _ qry) = hasAggregates qry
          hasAggregates Empty = False

pushRestrict (Restrict x (Binary op query1 query2))
        | noneIn1   = Binary op query1 (pushRestrict (Restrict x query2))
        | noneIn2   = Binary op (pushRestrict (Restrict x query1)) query2
        -- otherwise fall through
        where
          attrs     = attrInExpr x
          noneIn1   = null (attrs `intersect` attributes query1)
          noneIn2   = null (attrs `intersect` attributes query2)

pushRestrict (Restrict x (query@(Restrict _ _)))
        = case (pushed) of
            (Restrict _ _)    -> Restrict x pushed
            _                 -> pushRestrict (Restrict x pushed)
        where
          pushed = pushRestrict query

pushRestrict (Restrict x (Special op query))
	= Special op (pushRestrict (Restrict x query))

pushRestrict (Restrict x query)
        = Restrict x (pushRestrict query)


-- also push specials

-- Order is only pushed if it does not cause it to
-- end up with non-attribute expressions in the ordering
pushRestrict (Special (Order xs) (Project assoc query))
    | safe = Project assoc (pushRestrict (Special (Order xs') query))
	where
	  xs' = [OrderExpr o (substAttr assoc e) | OrderExpr o e <- xs]
	  safe = and [not (isAggregate e) | OrderExpr _ e <- xs']

-- Top is pushed through Project if there are no aggregates in the project
-- Aggregates can change the number of results.
pushRestrict (Special top@(Top _) (Project assoc query))
    | not (any isAggregate (map snd assoc)) 
        = Project assoc (pushRestrict (Special top query))
        
-- Offset is pushed through Project if there are no aggregates in the project
-- Aggregates can change the number of results.
pushRestrict (Special offset@(Offset _) (Project assoc query))
    | not (any isAggregate (map snd assoc)) 
        = Project assoc (pushRestrict (Special offset query))

pushRestrict (Special op (query@(Special _ _)))
	= case (pushed) of
	   (Special _ _)  -> Special op pushed
	   _		  -> pushRestrict (Special op pushed)
	where
          pushed = pushRestrict query

pushRestrict (Special op query)
	= Special op (pushRestrict query)

-- otherwise do nothing
pushRestrict query
        = query


optimizeExprs :: PrimQuery -> PrimQuery
optimizeExprs = foldPrimQuery (Empty, BaseTable, Project, restr, Binary, Group, Special)
    where 
      restr e q | exprIsTrue e' = q
                | otherwise = Restrict e' q
          where e' = optimizeExpr e

optimizeExpr :: PrimExpr -> PrimExpr
optimizeExpr = foldPrimExpr (AttrExpr,ConstExpr,bin,un,AggrExpr,CaseExpr,ListExpr,ParamExpr,FunExpr, CastExpr)
    where
      bin OpAnd e1 e2
          | exprIsFalse e1 || exprIsFalse e2 = exprFalse
          | exprIsTrue e1 = e2
          | exprIsTrue e2 = e1
      bin OpOr e1 e2
          | exprIsTrue e1 || exprIsTrue e2 = exprTrue
          | exprIsFalse e1 = e2
          | exprIsFalse e2 = e1
      bin OpIn _ (ListExpr []) = exprFalse
      bin op e1 e2 = BinExpr op e1 e2

      un OpNot (ConstExpr (BoolLit b)) = ConstExpr (BoolLit (not b))
      un op e = UnExpr op e

exprTrue :: PrimExpr
exprTrue = ConstExpr (BoolLit True)

exprFalse :: PrimExpr
exprFalse = ConstExpr (BoolLit False)

exprIsTrue :: PrimExpr -> Bool
exprIsTrue (ConstExpr (BoolLit True)) = True
exprIsTrue _ = False

exprIsFalse :: PrimExpr -> Bool
exprIsFalse (ConstExpr (BoolLit False)) = True
exprIsFalse _ = False