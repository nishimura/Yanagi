module Yanagi.View (parseTemplate) where

import Yanagi.Types
import Text.ParserCombinators.Parsec as P
import Network.FastCGI()
import Codec.Binary.UTF8.String (encodeString)

type YanagiParser a = CharParser Results a

data Filter = Normal | Br | Html

parseTemplate :: TmplName -> String -> Results -> String
parseTemplate (TmplName name) tmpl res = parseYanagi name tmpl res
--


parseYanagi :: String -> String -> Results -> String
parseYanagi name tmpl res = case runParser document res name tmpl of
                              Right ret -> ret
                              Left  err -> show err

document :: YanagiParser String
document = do
  ret <- many body
  eof
  return $ concat ret

body :: YanagiParser String
body = try(loop) <|> try(ifvar) <|> try(variable) <|> many1 text


ifvar :: YanagiParser String
ifvar = do
  name <- between (string "{if:") (string "}") (varName) <?> "ifvar"
  st <- getState
  let ifRes = case lookup name st of
                Just (Ifvar a) -> a
                _              -> False
  val <- if ifRes then concat `fmap` many body else emptyBody
  elval <- (try(elsevar) >>= (\x -> if not ifRes then return x else emptyBody))
           <|> return ""
  endToken
  return $ val ++ elval

elsevar :: YanagiParser String
elsevar = do { string "{else:}" >> concat `fmap` many body } <?> "elsevar"


endToken :: YanagiParser ()
endToken = do { string "{end:}" >> return () } <?> "endToken"

loop :: YanagiParser String
loop = do
  name <- loopToken
  loopbody <- lookupLoop name
  endloopToken
  return loopbody

loopToken :: YanagiParser String
loopToken = between (string "{loop:") (string "}") (varName) <?> "loopToken"
endloopToken :: YanagiParser ()
endloopToken = do { string "{endloop:}" >> return () } <?> "endLoopToken"

lookupLoop :: String -> YanagiParser String
lookupLoop n = do
  st <- getState
  case lookup n st of
    Just (Loop [])-> emptyBody
    Just (Loop a) -> do
                     input <- P.getInput
                     ret <- mapM (runLoop st input n) (zip [0..] a)
                     setState st
                     return $ concat ret
    _             -> emptyBody

emptyBody :: YanagiParser String
emptyBody = many body >> return ""

runLoop :: Results -> String -> String -> (Int, Results) -> YanagiParser String
runLoop r inp n (c,res) = do setState (res ++ [(n ++ ".loopCounter",
                                                Text $ show c)]
                                       ++ r
                                      )
                             setInput inp
                             b <- many body
                             return $ concat b

variable :: YanagiParser String
variable = do
  v <- between (char '{') (char '}') (varFilter)
  st   <- getState
  return $ lookupVar v st

varFilter :: YanagiParser (String, Filter)
varFilter = do
  v <- varName
  f <- option Normal parseFilter
  return (v, f)

parseFilter :: YanagiParser Filter
parseFilter = do
  char ':'
  f <- many1 alphaNum
  case f of
    "h" -> return Html
    "b" -> return Br
    _   -> return Normal

varName :: YanagiParser String
varName = many1 (alphaNum <|> oneOf "_." <?> "varName")

lookupVar :: (String, Filter) -> Results -> String
lookupVar (n,f) r = case lookup n r of
                  Just (Text a) -> choiceFilter $ encodeString a
                  Just (Loop _) -> "" -- todo?
                  _             -> ""
    where choiceFilter :: String -> String
          choiceFilter = case f of
                           Normal -> escape
                           Html   -> id
                           Br     -> nl2br . escape

nl2br :: String -> String
nl2br [] = []
nl2br ('\r':'\n':as) = "<br>\n"++nl2br as
nl2br ('\r':as) = nl2br as
nl2br ('\n':as) = "<br>\n"++nl2br as
nl2br (a:as)    = a:nl2br as

escape :: String -> String
escape [] = []
escape (a:as) | a == '<'  = "&lt;" ++ escape as
              | a == '>'  = "&gt;" ++ escape as
              | otherwise = a:escape as

text :: YanagiParser Char
text = try(do {
             notFollowedBy (char '{')
           ; anyChar
           }) <?> "text"

