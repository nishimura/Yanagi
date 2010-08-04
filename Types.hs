{-# Language FlexibleInstances, UndecidableInstances, IncoherentInstances #-}
module Yanagi.Types where

import Monad (liftM)

import Yanagi.Template.Database (DbRow(..))

type Results = [(String, Result)]
data Result = Text String | Html String
            | Loop [Results]
            | Ifvar Bool
              deriving Show

type EntryPoints = [(String, Act)]
newtype Act = Act { runAct :: Request -> IO ViewData }

data ViewData = ViewData { templateName :: Maybe TmplName
                         , outputType   :: Maybe OutputType
                         , results      :: Results
                         } deriving Show

data Request = Request { requestMethod :: String
                       , pathInfoStr   :: String
                       , pathInfoList  :: [String]
                       , serverName    :: String
                       , inputs        :: Req
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
                      , defaultAct        = emptyAct
                      }
pathInfoToActName :: (String -> String) -> ActName -> String -> ActName
pathInfoToActName f def ('/':as) = pathInfoToActName f def as
pathInfoToActName f def as = case f as of
                               "" -> def
                               a  -> ActName a


-- helper functions
viewPlusResults :: ViewData -> Results -> ViewData
viewPlusResults a b = a { results = results a ++ b }


insertLoop :: String -> Int -> (String, Result) -> Results -> Results
insertLoop a num new res =
    case lookup a res of
      Just (Loop l) -> (a, Loop $ f num l):deleteInLoop a res
      _             -> res
    where f :: Int -> [Results] -> [Results]
          f 0 (r:rs) = (new:r):rs
          f _ []     = []
          f n (r:rs) = r:f (n-1) rs
deleteInLoop a [] = []
deleteInLoop a (r@(name,_):rs)
    | a == name = deleteInLoop a rs
    | otherwise = r:deleteInLoop a rs


insertLoops :: [String] -> [Int] -> (String, Result) -> Results -> Results
insertLoops [] _ _ r = r
insertLoops _ [] _ r = r
insertLoops [a] (b:bs) new res = insertLoop a b new res
insertLoops (a:as) [b] new res = insertLoop a b new res
insertLoops (a:as) (b:bs) new res =
    case lookup a res of
      Just (Loop l) -> (a, Loop $ f b l):deleteInLoop a res
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


viewData :: Results -> ViewData
viewData = ViewData Nothing Nothing

ioViewData :: IO Results -> IO ViewData
ioViewData = liftM viewData

emptyAct :: Act
emptyAct = Act (return . viewData . const [])

act :: (Request -> Results) -> Act
act f = Act (return . viewData . f)
ioAct :: (Request -> IO Results) -> Act
ioAct f = Act (ioViewData . f)

mapAct :: Act
mapAct = act $ hashToResults . inputs

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


filterAllAct :: Results -> EntryPoints -> EntryPoints
filterAllAct res eps = map (f1 res) eps
    where f1 res (a, act) = (a, filterAct res act)

filterAct :: Results -> Act -> Act
filterAct res a = Act (\req -> do orig <- (runAct a) req
                                  let res' = results orig
                                  return $ orig { results = res ++ res' }
                      )


-- pathinfo 1
subAct :: (Request -> String) -> Act -> EntryPoints -> Act
subAct f empty as = Act (\req -> case lookup (f req) as of
                                   Just x  -> runAct x req
                                   Nothing -> runAct empty req
                        )


subAct1 :: Act -> EntryPoints -> Act
subAct1 = subAct (\(Request { pathInfoList = l }) -> f l)
    where f (_:a:_) = a
          f _ = ""
