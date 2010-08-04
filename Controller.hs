module Yanagi.Controller (run) where

import Control.Concurrent
import qualified Network.FastCGI as CGI
-- import Monad (liftM, liftM3)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as BS
import Codec.Binary.UTF8.String (decodeString)

import Yanagi.Types
import qualified Yanagi.View as View

-- Framework Main --

run :: AppConfig -> IO ()
run = CGI.runFastCGIConcurrent' forkIO 100 . router

router :: AppConfig -> CGI.CGI CGI.CGIResult
router cfg = CGI.setHeader "Content-type" (contentType cfg)
             >> request >>= CGI.liftIO . assignEpView cfg >>= CGI.output

assignEpView :: AppConfig -> Request -> IO String
assignEpView cfg req = do
  vdata  <- runAct (findAct cfg name) req
  let tName = findTemplateName name vdata
  tmpl <- templateData cfg tName
  return $ View.parseTemplate tName tmpl $ results vdata
      where name :: ActName
            name = findActName cfg req

findTemplateName :: ActName -> ViewData -> TmplName
findTemplateName a (ViewData {templateName = t}) =
    case t of
      Just x  -> x
      Nothing -> actToTmpl a

findAct :: AppConfig -> ActName -> Act
findAct cfg name =
    fromMaybe (defaultAct cfg) $ lookup (unActName name) $ entryPoints cfg

findActName :: AppConfig -> Request -> ActName
findActName cfg req = pathInfoToActName (pathInfoFilter cfg)
                  (defaultActName cfg) (pathInfoStr req)


request :: (CGI.MonadCGI m) => m Request
request = do
  r <- CGI.requestMethod
  p <- CGI.pathInfo
  let pl = split '/' (case p of
                        ('/':ps) -> ps
                        ps       -> ps)
  s <- CGI.serverName
  rq <- CGI.getInputs
  return $ Request r p pl s (decode rq)
      where split sep xs = case (break (== sep) xs) of
                             (ys, [])       -> [ys]
                             (ys, (sep':zs)) -> [ys] ++ split sep' zs
            decode req = ((map (\(n,v) -> (n, decodeString v))) req)



templateData :: AppConfig -> TmplName -> IO String
templateData c n = catch (BS.readFile (fullTemplate c n) >>= return . BS.unpack)
                   (\_ -> notFoundData c)
fullTemplate :: AppConfig -> TmplName -> String
fullTemplate c n = templateDir c ++ unTmplName n ++ templateExtension c

notFoundData :: AppConfig -> IO String
notFoundData cfg = catch (BS.readFile (notFound cfg) >>= return . BS.unpack )
                   (\_ -> return $ "Please save "
                          ++ unTmplName (notFoundTemplate cfg)
                                 ++ " for not found error.")
notFound :: AppConfig -> String
notFound cfg = templateDir cfg
               ++ unTmplName (notFoundTemplate cfg)

