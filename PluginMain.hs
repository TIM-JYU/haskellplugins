{-#LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables, DataKinds, MultiParamTypeClasses, FlexibleInstances, PolyKinds, DeriveFunctor, FlexibleContexts#-}
-- This module provides a standard main function for running plugins in tim.it.jyu.fi. It is not necessary
-- for plugins used in functional-programming.it.jyu.fi
module PluginMain where

import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T
import Snap.Core
import Snap.Util.FileServe
import System.FilePath
import UtilityPrelude
import PluginType

import Data.IORef
import Data.HashSet (HashSet)
import Snap.Http.Server
import qualified Data.Text.Lazy as LT


-- | Serve a plugin
serve :: forall m renderP updateP output. 
         (MonadSnap m, Available TimRender renderP, Available TimUpdate updateP,Reply TimResult output) => 
         Plugin renderP updateP output -> m ()
serve plugin = route  
        [
        ("html/", method POST $ do
            req :: Value <- getBody
            runArM (getIt (TimRender req) :: AR TimRender renderP)
                   (liftIO  . render plugin >=> writeLazyText)
        ),
        ("reqs/", method GET $ do
            writeLBS . encode $ 
                object [
                        "js"  .= [x | JS  x <- requirements plugin]
                       ,"css" .= [x | CSS x <- requirements plugin]
                       ,"angularModule" .= [x | NGModule x <- requirements plugin]
                       ,"type" .= ("embedded"::T.Text)
                       ]
        ),
        ("answer/", method PUT $ do
           req      <- getBody
           runArM (getIt (TimUpdate req) :: AR TimUpdate updateP)
                   $ \ps -> do
                      tims  <- liftIO (update plugin ps)
                      reply <- liftIO (putIt (TR []) tims)
                      writeLBS . encode $ reply                      
        )
        ]
        <|> serveStaticFiles "." plugin
    where 
     runArM :: forall s a. MonadSnap m => AR s a -> (a-> m ()) -> m ()
     runArM arAction f = do
           eps <- liftIO $ runAR arAction
           case eps of
                Left err -> modifyResponse (setContentType "text/plain" . setResponseCode 400) >> 
                            writeLazyText "Unable to parse required parameters" >>
                            writeText (T.pack err)

                Right v  -> f v

-- Quick helper for building objects
ins :: ToJSON a => T.Text -> First a -> [Aeson.Pair] -> [Aeson.Pair]
ins _   (First Nothing)  x = x 
ins key (First (Just v)) x = (key.=v):x 

serveStaticFiles :: MonadSnap m => FilePath -> Plugin renderP updateP outputP -> m ()
serveStaticFiles from plugin = do
        let locals = [file | CSS file <- requirements plugin]
                     ++ [file | JS  file <- requirements plugin]
                     ++ map T.pack (additionalFiles plugin)
        proposedPath <- getSafePath
        when (T.pack proposedPath`elem`locals)
             (serveFile (from</>proposedPath))

-- | `experiment` is used to test plugins without the whole server environment. Either compile this to 
-- a binary or run it from ghci.
experiment :: forall m renderP updateP output a. 
         (MonadSnap m, Available TimRender renderP, Available TimUpdate updateP,Reply TimResult output) => 
         Plugin renderP updateP output -> a -> Int -> m ()
experiment plugin markup' port = do 
    state  <- newIORef (Nothing)
    markup <- newIORef markup' 
    blackboard <- newIORef (mempty :: HashSet T.Text)
    let context = do
             m  <- readIORef markup 
             st <-  readIORef state
             pg <- case st of
                    Just s -> LT.toStrict <$> render plugin (m,s)
                    Nothing
             return $ \kw ->
                     case kw of
                        "port"   -> T.pack (show port)
                        "plugin" -> pg
                        "moduleDeps" -> T.pack . show $ [x | NGModule x <- requirements plugin]
                        "scripts"    -> T.unlines 
                                                $ ["<script src='"<>x<>"'></script>" 
                                                  | JS x <- requirements plugin]
                        "styles"       ->T.unlines 
                                                $ ["<link rel='stylesheet' type='text/css' href='"<>x<>"'>" 
                                                  | CSS x <- requirements plugin]
                        "app"    -> "MCQ"
                        x        -> "??"<>x<>"??"
        routes :: Snap ()
        routes = route [
          ("/index.html", method GET $ liftIO context >>= writeText . defaultPage 
          )
--          ("testPlugin/answer/", method PUT $ do
--             req <- getBody
--             stateVal <- liftIO $ readIORef state
--             tims :: TIMCmd state output <- liftIO $ do
--                            m <- readIORef markup
--                            s <- readIORef state
--                            update plugin (m, s, fromPlainInput req)
--             liftIO $ maybe (return ())
--                            (writeIORef state)
--                            (getFirst (_save tims))
--             writeLBS . encode . object $
--               [] & ins "web" (_web tims)
--               -- ["web" .= _web tims] 
--          )
          ] <|> serveDirectory "." <|> serveStaticFiles "." plugin
--
    httpServe (setPort port mempty)
              (routes)
    where
     defaultPage m = " \
 \    <!DOCTYPE html> \
 \     <html lang='en'> \
 \     <head> <meta charset='utf-8'> \
 \                 <script src='https://ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.17/angular.min.js'></script>\
 \                 "<>m "scripts"<>"\
 \                 "<>m "styles"<>"\
 \                 <script> \
 \                  var mainModule = angular.module('testApp',"<>m "moduleDeps"<>");\
 \                 </script> \
 \    </head> \
 \    \
 \     <body id='home' ng-app='testApp'> \
 \     <h1>Test</h1> \
 \     <div id='testPlugin' data-plugin='http://localhost:"<>m "port"<>"'>\
 \      "<>m "plugin"<>"\
 \     </div> \
 \     </body> \
 \    </html> "


---- | Plain input is used to extract `{"input":..}` messages that the experimentation
----   mode needs to be able to catch so it can pretend to be TIM.

fromJSON' :: FromJSON a => Value -> a
fromJSON' a = case fromJSON a of
    Error s   -> error s
    Success b -> b

-- | Extract a JSON value from the request body
getBody :: (MonadSnap m, FromJSON a) => m a
getBody = do
    f <- readRequestBody 100000
    case decode f of
        Nothing -> error $ "Could not decode input parameters:"++show f
        Just a  -> return a

