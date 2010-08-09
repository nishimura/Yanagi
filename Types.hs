{-# Language FlexibleInstances, UndecidableInstances, IncoherentInstances #-}
module Yanagi.Types where

import Control.Monad (liftM, foldM, (>=>))
import Data.Maybe (fromMaybe)

import Yanagi.Template.Database (DbRow(..))

type Results = [(String, Result)]
data Result = Text String -- | Html String
            | Loop [Results]
            | Ifvar Bool
              deriving Show

type EntryPoints = [(String, Act)]

data Context = Context { request  :: Request
                       , inputs   :: Req
                       , pathInfo :: [String]
                       , results  :: Results
                       , templateName :: TmplName
                       , outputType   :: OutputType
                       } deriving Show
type Act = Context -> IO Context

data Request = Request { requestMethod :: String
                       , pathInfoStr   :: String
                       , pathInfoList  :: [String]
                       , serverName    :: String
                       , scriptName    :: String
                       } deriving Show
type Req = [(String, String)]

data AppConfig = AppConfig { templateDir       :: String
                           , defaultOutputType :: OutputType
                           , templateExtension :: String
                           , notFoundTemplate  :: TmplName
                           , entryPoints       :: EntryPoints
                           , contentType       :: String
                           , pathInfoFilter    :: (String -> String)
                           , defaultActName    :: ActName
                           , defaultAct        :: Act
                           }
data OutputType = TemplateOutput
                | PrintOutput
                | JsonOutput
                  deriving Show

newtype ActName = ActName { unActName :: String } deriving Show
newtype TmplName = TmplName { unTmplName :: String } deriving Show

-- default config
appConfig :: AppConfig
appConfig = AppConfig { templateDir       = "../template/"
                      , defaultOutputType = TemplateOutput
                      , templateExtension = ".html"
                      , notFoundTemplate  = TmplName "404.html"
                      , entryPoints       = []
                      , contentType       = "text/html"
                      , pathInfoFilter    = id
                      , defaultActName    = ActName "Top"
                      , defaultAct        = return . id
                      }
pathInfoToActName :: (String -> String) -> ActName -> String -> ActName
pathInfoToActName f def ('/':as) = pathInfoToActName f def as
pathInfoToActName f def as = case f as of
                               "" -> def
                               a  -> ActName a


-- helper functions
plusResults :: Context -> Results -> Context
plusResults c r = c { results = results c ++ r }


replaceResults :: (String, Result) -> Results -> Results
replaceResults _ [] = []
replaceResults r@(n1,v1) (a@(n2,v2):as)
    | n1 == n2  = (n1,v1):replaceResults r as
    | otherwise = a:replaceResults r as

replaceLoop :: (Results -> Results) -> [String] -> Results -> Results
replaceLoop f [] r = f r
replaceLoop f (a:as) r =
    case lookup a r of
      Just (Loop inner) ->
          replaceResults (a,Loop $ map (replaceLoop f as) inner) r
      _ -> r

insertLoop :: String -> Int -> (String, Result) -> Results -> Results
insertLoop a num new res =
    case lookup a res of
      Just (Loop l) -> replaceResults (a, Loop $ f num l) res
      _             -> res
    where f :: Int -> [Results] -> [Results]
          f 0 (r:rs) = (new:r):rs
          f _ []     = []
          f n (r:rs) = r:f (n-1) rs

insertLoops :: [String] -> [Int] -> (String, Result) -> Results -> Results
insertLoops [] _ _ r = r
insertLoops _ [] _ r = r
insertLoops [a] (b:bs) new res = insertLoop a b new res
insertLoops (a:as) [b] new res = insertLoop a b new res
insertLoops (a:as) (b:bs) new res =
    case lookup a res of
      Just (Loop l) -> replaceResults (a, Loop $ f b l) res
      _             -> res
    where f 0 (r:rs) = (insertLoops as bs new r):rs
          f _ []     = []
          f n (r:rs) = r:f (n-1) rs

-- -- Test: insertLoops ["lp1","lp2"] [0, 1] ("piyo", Text "piyo") a
-- a = [("hoge", Text "hoge")
--     ,("fuga", Loop [[("foo", Text "foo1"),("bar", Text "bar1")]
--                    ,[("foo", Text "foo2"),("bar", Text "bar2")]
--                    ])
--     ,("lp1", Loop [[("lp2", Loop [[("lp3", Text "lp3v1")]
--                                  ,[("lp3", Text "lp3v2")]
--                                  ]
--                     )]]
--      )
--     ]


pathInfoFilter1 :: String -> String
pathInfoFilter1 [] = []
pathInfoFilter1 ('/':_) = []
pathInfoFilter1 (a:as) = a:pathInfoFilter1 as

pathInfoFilter2 :: String -> String
pathInfoFilter2 [] = []
pathInfoFilter2 ('/':as) = pathInfoFilter1 as
pathInfoFilter2 (_:as) = pathInfoFilter2 as


reqToStr :: Req -> String -> String
reqToStr r k = case lookup k r of
                 Just x  -> x
                 Nothing -> ""

actToTmpl :: ActName -> TmplName
actToTmpl = TmplName . unActName


act :: (Req -> IO Results) -> Act
act f c@(Context { inputs = req }) =
    f req >>= return . (\res -> c { results = res })

mapAct :: Act
mapAct c@(Context { inputs = i }) =
    return $ c { results = hashToResults i }

-- testing...
-- filterAct => Acts
-- type MaybeAct = Context -> IO (Maybe Context)
filterAct :: [Context -> IO (Maybe Context)] -> Context -> IO Context
filterAct fs a =
    liftM (\x -> fromMaybe a x) (foldM foldImpl (Just a) fs)
        where foldImpl :: Maybe Context -> (Context -> IO (Maybe Context))
                       -> IO (Maybe Context)
              foldImpl a f = case a of
                               Just a -> f a
                               Nothing -> return Nothing


resultsFilterAct :: IO Results -> Act
resultsFilterAct plus c@(Context { results = res })
    = plus >>= return . (\x -> c { results = res ++ x })
resultsFilterEp :: IO Results -> EntryPoints -> EntryPoints
resultsFilterEp _ [] = []
resultsFilterEp plus ((k,f):as) =
    (k,resultsFilterAct plus >=> f):resultsFilterEp plus as

rowsToResults :: (DbRow a) => [a] -> [Results]
rowsToResults = map rowToResults

rowToResults :: (DbRow a) => a -> Results
rowToResults = hashToResults . toList

hashToResults :: [(String, String)] -> Results
hashToResults [] = []
hashToResults ((x,y):as) = (x, Text y):hashToResults as

class Resultsable a where
    toResults :: a -> Results

instance (DbRow a) => Resultsable a where
    toResults = hashToResults . toList

instance Resultsable [(String, String)] where
    toResults = hashToResults


class Loopable a where
    toLoop :: [a] -> Result

instance (DbRow a) => Loopable a where
    toLoop as = Loop $ rowsToResults as

