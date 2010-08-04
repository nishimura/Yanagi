{-# LANGUAGE TemplateHaskell #-}
module Yanagi.Database.TablesInfo (initTablesInfo) where


import Language.Haskell.TH
import Database.HDBC (IConnection, SqlValue,
                      getTables, quickQuery', fromSql)
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Database.HDBC.PostgreSQL as Pgsql


class (IConnection conn) => YanagiConn conn where
    columnsQuery :: conn -> String -> IO [[SqlValue]]

instance YanagiConn Sqlite3.Connection where
    columnsQuery conn table = quickQuery' conn ("pragma table_info(" ++ table ++ ")") []

instance YanagiConn Pgsql.Connection where
    columnsQuery conn table = quickQuery' conn (concat
        ["select a.attrelid, a.attname, t.typname, a.attnotnull, a.atthasdef, a.atttypmod "
        ,"from pg_class c, pg_attribute a, pg_type t, pg_namespace n "
        ,"where relkind in ('r','v') AND (c.relname='" ++ table
        ,"' or c.relname = lower('" ++ table ++ "')) "
        ,"and c.relnamespace=n.oid and n.nspname='public' "
        ,"AND a.attnum > 0 "
        ,"AND a.atttypid = t.oid AND a.attrelid = c.oid ORDER BY a.attnum"
        ]) []


initTablesInfo :: (YanagiConn conn) => IO conn -> Q [Dec]
initTablesInfo conn = runIO $ do
  conn' <- conn
  tables <- getTables conn'
  let tablesDec = tableInfoDec tables
  a <- mapM (initTableInfo conn') tables
  return $ tablesDec:colsDec a
      where colsDec cols = [ValD (VarP (mkName "colsInfo"))
                                           (NormalB (ListE cols)) []]

tableInfoDec :: [String] -> Dec
tableInfoDec tables = ValD (VarP (mkName "tablesInfo"))
                            (NormalB (ListE mkTables)) []
    where mkTables = map (\x -> LitE (StringL x)) tables

initTableInfo :: (YanagiConn conn) => conn -> String -> IO Exp
initTableInfo conn table = do
  ret <- columnsQuery conn table
  let cols = map convRowInfo ret
  return (TupE [LitE (StringL table)
               ,ListE (map colsInfoExp cols)])

colsInfoExp :: (String, (String, Bool)) -> Exp
colsInfoExp (name, (typ, notNull)) =
    TupE [LitE (StringL name)
                ,TupE [LitE (StringL typ)
                      ,ConE nn]
         ]
        where nn = if notNull then 'True else 'False



convRowInfo :: [SqlValue] -> (String, (String, Bool))
convRowInfo [_,n,t,nn,_,_] =
    (n', (t', nn'))
        where n' = (fromSql n)::String
              t' = (fromSql t)::String
              nn' = case (fromSql nn)::String of
                      -- Sqlite3
                      "99" -> True
                      "0"  -> False
                      -- PostgreSQL
                      "True"  -> True
                      "False"  -> False
                      _    -> False
convRowInfo x = ("unexpected result",(show x, False))
