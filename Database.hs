{-# LANGUAGE TemplateHaskell #-}
module Yanagi.Database (query, makeResult, convertNames) where

import Database.HDBC (IConnection, SqlValue(SqlNull), SqlError(..),
                      prepare, execute, commit, catchSql, rollback, finish,
                      throwSqlError,
                      fromSql)

import Yanagi.Types
import Data.Char (toUpper)


-- All error messages are not displayed in default.
-- To display all error messages, the catchSql function is nested.
query :: (IConnection conn) =>
         conn -> String -> [SqlValue] -> IO Integer
query conn sql params = do
  sth <- prepare conn sql
  catchSql ( do
             ret <- execute sth params
             commit conn
             return ret
           )
             (\e@(SqlError _ _ _) -> do
                rollback conn
                finish sth
                       `catchSql` \x@(SqlError _ _ _) ->
                           throwSqlError $ x { seErrorMsg = "PREPARE: " ++ seErrorMsg e ++ ", EXECUTE: " ++ seErrorMsg x }
                return 0
             )

convertNames :: [String] -> [String]
convertNames [] = []
convertNames (a:as) = convertName a:convertNames as

convertName :: String -> String
convertName [] = []
convertName ('_':a:as) = toUpper a:convertName as
convertName (a:as) = a:convertName as

makeResult :: [String] -> [[SqlValue]] -> Result
makeResult columns values = Loop (map makeResult' values)
    where makeResult' = convertRow columns

convertRow :: [String] -> [SqlValue] -> Results
convertRow [] _ = []
convertRow _ [] = []
convertRow (c:cs) (SqlNull:vs) = (c, Text ""):convertRow cs vs
convertRow (c:cs) (v:vs) =
    (c, Text ((fromSql v)::String)):convertRow cs vs


