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
             >> makeRequests >>= CGI.liftIO . assignEpView cfg >>= CGI.output

assignEpView :: AppConfig -> (Request, Req) -> IO String
assignEpView cfg (req,inp) = do
  newCtx <- (findAct cfg name) defaultContext
  let tName = templateName newCtx
  tmpl <- templateData cfg tName
  return $ View.parseTemplate tName tmpl $ results newCtx
      where name :: ActName
            name = findActName cfg req
            defaultContext = Context { request  = req
                                     , inputs   = inp
                                     , pathInfo = pathInfoList req
                                     , results  = []
                                     , templateName = actToTmpl name
                                     , outputType = defaultOutputType cfg
                                     }

findAct :: AppConfig -> ActName -> Act
findAct cfg name =
    fromMaybe (defaultAct cfg) $ lookup (unActName name) $ entryPoints cfg

findActName :: AppConfig -> Request -> ActName
findActName cfg req = pathInfoToActName (pathInfoFilter cfg)
                  (defaultActName cfg) (pathInfoStr req)


makeRequests :: (CGI.MonadCGI m) => m (Request, Req)
makeRequests = do
  r <- CGI.requestMethod
  p <- CGI.pathInfo
  let pl = split '/' (case p of
                        ('/':ps) -> ps
                        ps       -> ps)
  sv <- CGI.serverName
  rq <- CGI.getInputs
  sc <- CGI.scriptName
  return $ (Request { requestMethod = r
                    , pathInfoStr   = p
                    , pathInfoList  = pl
                    , serverName    = sv
                    , scriptName    = sc
                    } , decode rq)
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

