
module Main where

import Happstack.Server
import qualified Data.ByteString.Lazy.Char8 as B


import Data.Aeson (decode, encode)

import qualified Data.List as List
import Data.Maybe (catMaybes)

import Control.Monad.IO.Class
import Control.Monad

setMime :: String -> B.ByteString -> Response
setMime mt txt =
  (toResponse "")
    { rsBody=txt,
      rsHeaders=(mkHeaders [("Content-Type", mt)])} 


pageNotFound :: String -> ServerPart Response
pageNotFound s = do
  liftIO $ putStrLn $ "page not found: " ++ s
  ok $ toResponse "page not found"


buildRoute :: String -> String
buildRoute = ("webroot/" ++)

main :: IO ()
main = do
  d3 <-  B.readFile $ buildRoute "js/d3.v3.min.js"
  jq <-  B.readFile $ buildRoute "js/jquery-1.11.0.min.js"

  simpleHTTP nullConf $ do
    txt <- liftIO $ B.readFile $ buildRoute "html/index.html"

    liftIO (putStrLn "Hi there!")


    msum [ dir "index.html" $ return (setMime "text/html" txt),
           dir "d3.v3.min.js" $ ok (setMime "text/javascript" d3),
           dir "jquery-1.11.0.min.js" $ ok (setMime "text/javascript" jq),
           dir "html" $ serveDirectory DisableBrowsing [] $ buildRoute "html",
           dir "js" $ serveDirectory DisableBrowsing [] $ buildRoute "js",
           dir "css" $ serveDirectory DisableBrowsing [] $ buildRoute "css",
           uriRest pageNotFound ]
