{-# LANGUAGE TemplateHaskell, StandaloneDeriving, FlexibleContexts #-}
module Yanagi.Template.Database (DbRow(..)
                                ,internalSave, internalFindDb
                                ,internalFindDb1
                                ,makeFunctions
                                ,SaveType(..)) where
-- module Yanagi.Database.Th where

import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Convertible (Convertible)
import Language.Haskell.TH
import Data.List (isPrefixOf)
import Data.Char (toUpper)
import Database.HDBC (IConnection, SqlValue(..)
                     ,getTables, quickQuery', fromSql, toSql
                     ,prepare, execute)
-- import System.Time (ClockTime(..))
-- deriving instance Data ClockTime
import Data.Time

deriving instance Data Day
deriving instance Data TimeOfDay
deriving instance Data LocalTime


class DbRow a where
    save :: (IConnection conn) => conn -> a -> SaveType -> IO Integer
    -- update :: (IConnection conn) => conn -> a -> IO Bool
    toList :: a -> [(String, String)]

type ColType = (String, (String, Bool))

data SaveType = UPDATE | INSERT
                deriving (Show)

{-
  import Yanagi.Template.Database
  import App.TablesInfo

  makeFunctions colsInfo
-}
makeFunctions :: [(String, [ColType])] -> Q [Dec]
makeFunctions info = return $ concat $ map makeFunction info

makeFunction :: (String, [ColType]) -> [Dec]
makeFunction (table, cols) = 
    makeTableDec table cols ++ initInstances table cols



internalSave :: (DbRow a, Show a, IConnection conn) =>
                String -> [String] ->
                (a -> [SqlValue]) -> conn -> a -> SaveType -> IO Integer
internalSave table cols builder conn x typ = do
  let xlist = toList x
      xSqlVals = builder x
      specPkey = case snd $ head xlist of
                   "0" -> False -- Auto Increment
                   _   -> True
      insertCols = if specPkey then cols else tail cols
      insertSqlVals = if specPkey then xSqlVals else tail xSqlVals
      insert = "INSERT INTO " ++ table ++ " ("++ colsStrI insertCols ++ ")"
               ++ " VALUES (" ++ num insertCols ++ ")"
      update = "UPDATE " ++ table ++ " SET " ++ colsStrU (tail cols)
               ++ " WHERE " ++ head cols ++ " = ? "
      (queryStr,params) = case typ of
                            INSERT -> (insert, insertSqlVals)
                            UPDATE -> (update, (tail xSqlVals)++[head xSqlVals])
      -- プライマリキー：snd $ head xlist
  stmt <- prepare conn queryStr
  ret <- execute stmt params
  return ret
      where colsStrI [] = ""
            colsStrI [c] = c
            colsStrI (c:cs) = c ++ ", " ++ colsStrI cs
            num [] = ""
            num [_] = "?"
            num (_:cs) = "?," ++ num cs
            colsStrU [] = ""
            colsStrU [c] = c ++ " = ? "
            colsStrU (c:cs) = c ++ " = ?, " ++ colsStrU cs



internalFindDb :: (DbRow a, IConnection conn) =>
                String -> [String] ->
                ([SqlValue] -> a) -> conn -> IO [a]
internalFindDb table cols builder conn = do
  let queryStr = "select * from " ++ table
  ret <- quickQuery' conn queryStr []
  return $ map builder ret

internalFindDb1 :: (DbRow a, IConnection conn, Convertible b SqlValue) =>
                String -> [String] ->
                ([SqlValue] -> a) -> conn -> b -> IO (Maybe a)
internalFindDb1 table cols builder conn x = do
  let queryStr = "select * from " ++ table ++ whereStr
      whereStr = " where " ++ head cols ++ " = ?"
      param = [toSql x]
  ret <- quickQuery' conn queryStr param
  if null ret
     then return Nothing
     else return $ Just (builder $ head ret)


initInstances :: String -> [ColType] -> [Dec]
initInstances table cols =
  [InstanceD [] (AppT
                 (ConT $ mkName "Yanagi.Template.Database.DbRow")
                 (ConT $ ucName table))
   [saveFuncDec table cols
   ,toListDec table cols
   ]
  ] ++ findFuncDec table cols'
    where cols' = map fst cols


-- find<table> :: (IConnection conn) => conn -> IO [<table>]
-- find<table>1 :: (IConnection conn) => conn -> Int -> IO (Maybe <table>)
findFuncDec :: String -> [String] -> [Dec]
findFuncDec table cols =
  [SigD func (ForallT [PlainTV conn]
                      [ClassP ''IConnection [VarT conn]]
                      (AppT (AppT ArrowT (VarT conn))
                            (AppT (ConT ''IO)
                                  (AppT ListT
                                        (ConT $ ucName table)))))

  ,FunD func
           [Clause [VarP conn]
            (NormalB (appListE
                      [VarE $ mkName $ "internalFindDb"
                      ,LitE (StringL table)
                      ,colsExp cols
                      ,buildExp table cols
                      ,VarE conn
                      ])
            ) []]
  ,SigD func1 (ForallT [PlainTV conn, PlainTV b]
                      [ClassP ''IConnection [VarT conn]
                      ,ClassP ''Convertible [VarT b, ConT ''SqlValue]
                      ]
                      (AppT (AppT ArrowT (VarT conn))
                            (AppT (AppT ArrowT
                                        (VarT b)
                                  )
                                  (AppT (ConT ''IO)
                                        (AppT (ConT ''Maybe)
                                              (ConT $ ucName table))))))
   ,FunD func1
           [Clause [VarP conn,VarP arg]
            (NormalB (appListE
                      [VarE $ mkName $ "internalFindDb1"
                      ,LitE (StringL table)
                      ,colsExp cols
                      ,buildExp table cols
                      ,VarE conn
                      ,VarE arg
                      ])
            ) []]
  ]
      where conn = mkName "conn"
            b = mkName "b"
            arg = mkName "primaryKey"
            func = mkName $ findStr table
            func1 = mkName (findStr table ++ "1")



saveFuncDec :: String -> [ColType] -> Dec
saveFuncDec table cols =
  FunD (mkName "save")
           [Clause [VarP conn,VarP self, VarP saveType]
             (NormalB (appListE [VarE $ mkName "internalSave"
                                ,LitE (StringL table)
                                ,colsExp (map fst cols)
                                ,saveExp table cols
                                ,VarE conn
                                ,VarE self
                                ,VarE saveType
                                ]
                      )
            ) []]
      where conn = mkName "conn"
            self = mkName "self"
            saveType = mkName "type"

-- save function :: a -> [SqlValue]
saveExp :: String -> [ColType] -> Exp
saveExp table cols =
    LamE [VarP dbrow]
         (ListE (map (toSqlValueExp table dbrow) cols))
        where dbrow = mkName "dbrow"


appListE :: [Exp] -> Exp
appListE [a,b] = AppE a b
appListE (a1:a2:as) = appListE ((AppE a1 a2):as)
appListE _ = undefined

-- builder function :: [SqlValue] -> b
buildExp :: String -> [String] -> Exp
buildExp table cols =
    LamE [ListP colsP]
         (appListE
          ([ConE $ ucName table] ++ (colsE cols) -- ++ [pKeyE]
          )
         )
        where colsP = map (VarP . mkName) cols
              colsE [] = []
              colsE (c1:cs) =
                  AppE (VarE 'fromSql) (VarE $ mkName c1):colsE cs
              -- pKeyE = AppE (VarE 'fromSql) (VarE $ mkName pKey)
              -- pKey = head cols

colsExp :: [String] -> Exp
colsExp cols = ListE $ map list cols
    where list c = LitE (StringL c)

toListDec :: String -> [ColType] -> Dec
toListDec table cols =
  FunD (mkName "toList")
           [Clause [VarP self]
             (NormalB (ListE $ map (toTupleExp table self) cols)
             ) []]
      where self = mkName "self"


{- (DbRow a) => a -> [(String, String)] -}
toTupleExp :: String -> Name -> ColType -> Exp
toTupleExp table self c@(col,_) =
    TupE [LitE (StringL $ camelCase col)
         ,toAny table c self justToStr (LitE (StringL ""))
         ]

{- (DbRow a) => a -> [SqlValue] -}
toSqlValueExp :: String -> Name -> ColType -> Exp
toSqlValueExp table self c = 
    toAny table c self justToSqlValue (ConE 'SqlNull)


toAny :: String -> ColType -> Name -> (ColType -> Exp -> Exp) -> Exp -> Exp
toAny table c@(col,(_,nn)) self justToAny nothingE =
    if nn then justToAny c argE else maybeToAny c argE justToAny nothingE
        where argE = colValE table col self

colValE :: String -> String -> Name -> Exp
colValE table col name = AppE (VarE $ mkName $ camelCase $ table ++ "'" ++ col)
                         (VarE name)
maybeToAny :: ColType -> Exp -> (ColType -> Exp -> Exp) -> Exp -> Exp
maybeToAny col colVal justToAny nothingE =
    CaseE colVal
              [Match (ConP just [VarP $ mkName "col"])
                         (NormalB (justToAny col (VarE $ mkName "col"))) []
              ,Match (ConP nothing [])
                         (NormalB (nothingE)) []
              ]
        where just = mkName "Just"
              nothing = mkName "Nothing"

justToStr :: ColType -> Exp -> Exp
justToStr (_,(typ,_)) a | typ == "text" || isPrefixOf "varchar" typ = a
                           | otherwise = AppE (VarE 'show) a

justToSqlValue :: ColType -> Exp -> Exp
justToSqlValue _ a = AppE (VarE 'toSql) a


makeTableDec :: String -> [(String,(String,Bool))] -> [Dec]
makeTableDec table cols =
  [DataD [] (ucName table) []
   [RecC (ucName table) (map (makeProp table makeMaybeType) cols
                         -- ++ [(pKeyName, NotStrict, makeMaybeType pKey)]
                        )
   ]
   [''Show, ''Eq, ''Typeable, ''Data]
  ]
    -- where pKeyName = mkName $ camelCase table ++ "'pKey"
    --       pKey = f (head cols)
    --       f (_,(a,_)) = (a, False)


makeProp :: String -> ((String, Bool) -> Type) -> ColType
         -> (Name, Strict, Type)
makeProp table f (name,typ) =
    ((mkName $ camelCase table ++ "'" ++ camelCase name),IsStrict,f typ)

makeType :: String -> Type
makeType a
    | a == "integer"         = ConT ''Int
    | a == "text"            = ConT ''String
    | a == "bool"            = ConT ''Bool
    | a == "numeric"         = ConT ''Int
    | a == "time"            = ConT ''LocalTime
    | a == "timestamp"       = ConT ''LocalTime
    | a == "date"            = ConT ''LocalTime
    | isPrefixOf "varchar" a = ConT ''String
    | isPrefixOf "int"     a = ConT ''Int
    | isPrefixOf "float"   a = ConT ''Double
    | isPrefixOf "double"  a = ConT ''Double
    | otherwise = error a

makeMaybeType :: (String, Bool) -> Type
makeMaybeType (t,nn) = if nn then makeType t
                             else AppT (ConT ''Maybe) (makeType t)


ucfirst :: String -> String
ucfirst [] = []
ucfirst (a:as) = toUpper a:as

ucName :: String -> Name
ucName = mkName . ucfirst . camelCase

findStr :: String -> String
findStr = ("find" ++) . ucfirst . camelCase

camelCase :: String -> String
camelCase [] = []
camelCase ('_':a:as) = toUpper a:camelCase as
camelCase (a:as) = a:camelCase as

