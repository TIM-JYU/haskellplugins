{-#LANGUAGE DeriveGeneric
  , OverloadedStrings
  , ScopedTypeVariables
  , DataKinds
  , MultiParamTypeClasses
  , GADTs
  , FlexibleInstances
  , TypeFamilies
  , DeriveFunctor
  , FlexibleContexts
  , RecordWildCards#-}
-- This module provides a standard main function for running plugins in tim.it.jyu.fi. It is not necessary
-- for plugins used in functional-programming.it.jyu.fi
module PluginMain where

import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Snap.Core
import Snap.Util.FileServe
import System.FilePath
import UtilityPrelude
import PluginType
import Data.Monoid
import Data.Dynamic -- Bad Ville

import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Snap.Http.Server
import qualified Data.Text.Lazy as LT
import SimpleScripts
import Data.Readable


bsToText = T.unpack . T.decodeUtf8

-- | Serve a plugin

serve :: forall m renderP updateP output. 
         (MonadSnap m, Available TimRender renderP, Available TimUpdate updateP,Reply TimResult output) => 
         FilePath -> Plugin renderP updateP output -> m ()
serve resPath plugin = route  
        [
        ("html/", method POST $ do
            req :: Value <- getBody
            liftIO $ print req
            _ <- runArM (getIt (TimRender req) :: AR TimRender renderP)
                   (liftIO  . render plugin >=> writeLazyText)
            return ()
        ),
        ("multihtml/", method POST $ do
            reqs :: [Value] <- getBody
            xs <- mapM (\req -> runArM 
                        (getIt (TimRender req) :: AR TimRender renderP)
                        (liftIO  . render plugin))
                       reqs 
            writeLBS (encode xs)
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
           _ <- runArM (getIt (TimUpdate req) :: AR TimUpdate updateP)
                   $ \ps -> do
                      tims  <- liftIO (update plugin ps)
                      reply <- liftIO (putIt (TR []) tims)
                      writeLBS . encode $ reply                      
           return () 
        )
        ]
        <|> serveStaticFiles resPath plugin
    where 
     runArM :: forall s a x. MonadSnap m => AR s a -> (a-> m x) -> m (Maybe x)
     runArM arAction f = do
           eps <- liftIO $ runAR arAction
           case eps of
                Left err -> do
                            modifyResponse (setContentType "text/plain" . setResponseCode 400) 
                            writeLazyText "Unable to parse required parameters" 
                            writeText (T.pack err) 
                            return Nothing
                Right v  -> do {r<-f v; return (Just r)}

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
        if (T.pack proposedPath`elem`locals)
             then (serveFile (from</>proposedPath))
             else empty 

-- | `experiment` is used to test plugins without the whole server environment. Either compile this to 
-- a binary or run it from ghci.
data Experiment m s = ES {markup::m, state::s} deriving (Eq,Ord,Show)
data ExperimentWithInput m s i = ESI {experimented::Experiment m s, input::i} deriving (Eq,Ord,Show)
instance Available (Experiment m s)            (State s)  where getIt es = pure (State  (state es))
instance Available (ExperimentWithInput m s i) (State s)  where getIt es = pure (State  (state (experimented es)))
instance Available (ExperimentWithInput m s i) (TaskID)   where getIt _  = pure (TID "experiment_task")
instance Available (ExperimentWithInput m s i) (Maybe User)   where getIt _  = pure (Just (User "experiment_user"))
instance Available (Experiment m s) (Maybe User)   where getIt _  = pure (Just (User "experiment_user"))
instance Available (Experiment m s)            (TaskID)   where getIt _  = pure (TID "experiment_task")
instance Available (Experiment m s)            (Markup m) where getIt es = pure (Markup (markup es))
instance Available (ExperimentWithInput m s i) (Markup m) where getIt es = pure (Markup (markup (experimented es)))
instance Available (ExperimentWithInput m s i) (Input  i) where getIt es = pure (Input (input es))

data ExperimentOutput = EO {toWeb :: First Value, toState::First Dynamic} deriving (Show)
instance Monoid (ExperimentOutput) where
     (EO a b) `mappend` (EO c d) = EO (mappend a c) (mappend b d)
     mempty = EO mempty mempty
instance ToJSON c => Reply (ExperimentOutput) (Web c) where
    putIt eo (Web a) = return $ eo `mappend` (EO (First (Just (toJSON a))) mempty)
instance Typeable a => Reply (ExperimentOutput) (Save a) where
    putIt eo (Save a) = return $ eo `mappend` (EO mempty (First . Just . toDyn $ a))
instance  Reply (ExperimentOutput) Log where
    putIt eo (LogMsg a) = T.putStrLn a >> return eo

instance Reply (ExperimentOutput) (TimInfo a) where
    putIt eo (TimInfo _) = return eo -- $ eo `mappend` (EO mempty (First . Just . toDyn $ a))
instance Reply (ExperimentOutput) (BlackboardOut) where
    putIt eo (BlackboardOut _) = return eo -- $ eo `mappend` (EO mempty (First . Just . toDyn $ a))

type family WebInput a where
        WebInput (Input a)   = a
        WebInput (Input a,b) = a
        WebInput (Input a,b,c) = a
        WebInput (Input a,b,c,d) = a
        WebInput (Input a,b,c,d,e) = a
        WebInput (a,b)       = WebInput b
        WebInput (a,b,c)     = WebInput (b,c)
        WebInput (a,b,c,d)   = WebInput (b,c,d)
        WebInput (a,b,c,d,e) = WebInput (b,c,d,e)
        
wrap :: forall m s renderP updateP output. 
    (Available (Experiment m s) renderP
    ,Available (ExperimentWithInput m s (WebInput updateP)) updateP
    ,FromJSON (WebInput updateP)

    ,Reply ExperimentOutput output
    ) =>
        Plugin renderP updateP output 
        -> Plugin (Experiment m s) (ExperimentWithInput m s LBS.ByteString) ExperimentOutput
wrap plugin = Plugin 
    (\st -> runAR (getIt st) >>= \r -> case r of
                Left  e -> error $ "Could not render:" <> e
                Right r -> render plugin r)
    (\sti ->  case eitherDecode (input sti) of
                Left  e -> error $ "Could not update:" <> e <> "\n Input was: "<>show (input sti)
                Right (PlainInput r) -> runAR (getIt (sti{input=r::WebInput updateP})) >>= \r -> case r of
                              Left err  -> error err
                              Right val -> do
                                tims <- update plugin (val::updateP)
                                putIt mempty tims
    )
    (requirements plugin)
    (additionalFiles plugin)
    (additionalRoutes plugin)
 
experiment ::
         Typeable state => 
         Plugin (Experiment markup state) 
                (ExperimentWithInput markup state LBS.ByteString) 
                ExperimentOutput
          -> markup -> state -> FilePath -> FilePath -> Int -> IO ()
experiment plugin markup' state' resDir stDir port = do 
    experimentData  <- newIORef ES{markup=markup',state=state'}
    let context = do
             st  <- readIORef experimentData
             pg  <- render plugin st
             return $ \kw ->
                     case kw of
                        "port"   -> T.pack (show port)
                        "plugin" -> LT.toStrict pg
                        "moduleDeps" -> T.pack . show $ [x | NGModule x <- requirements plugin]
                        "scripts"    -> T.unlines 
                                                  ["<script src='"<>x<>"'></script>" 
                                                  | JS x <- requirements plugin]
                        "styles"       ->T.unlines 
                                                  ["<link rel='stylesheet' type='text/css' href='"<>x<>"'>" 
                                                  | CSS x <- requirements plugin]
                        "app"    -> "MCQ"
                        x        -> "??"<>x<>"??"
        routes :: Snap ()
        routes = route [
          ("/index.html", method GET $ liftIO context >>= writeText . defaultPage)
          ,("/stdlib.js", method GET $ writeText allJS)
          ,("/plugins/testPluginType/testPlugin/answer/", method PUT $ do
               liftIO $ putStrLn "Update called"
               req  <- readRequestBody (15000000) -- TODO: Sensible limit..
               st  <- liftIO $ readIORef experimentData
               let 
                 ctx = ESI st req
               us <- liftIO (update plugin ctx)
               maybe (return ()) (writeLBS . encode . \x -> object ["web" .= x]) (getFirst . toWeb $ us)
               liftIO $ case getFirst (toState us) of
                        Nothing ->  return ()
                        Just val -> writeIORef experimentData (ES (markup st) (fromDyn val (error "Bad dynamic!")))
                
           )
          ,("/plugins/testPluginType/:fn", method GET $ do
                Just fn <- getParam "fn"
                serveFile (resDir ++ "/" ++  bsToText fn)
            )
          ] <|> serveDirectory resDir <|> serveStaticFiles stDir plugin 
            <|> (modifyResponse (setResponseStatus 404 "No such service")
                >>writeText "No such service\n\n">> getRequest >>= writeText . T.pack . show)
--
    httpServe (setPort port mempty)
              routes
    where
     -- runArM :: forall s a. MonadSnap m => AR s a -> (a-> m ()) -> m ()
     runArM arAction f = do
           eps <- liftIO $ runAR arAction
           case eps of
                Left _ -> error "Not supported"
                Right v  -> f v
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
 \     <div id='testPlugin' data-plugin='plugins/testPluginType"<>"'>\
 \      "<>m "plugin"<>"\
 \     </div> \
 \     </body> \
 \    </html> "




fromJSON' :: FromJSON a => Value -> a
fromJSON' a = case fromJSON a of
    Error s   -> error s
    Success b -> b

-- | Extract a JSON value from the request body
getBody :: (MonadSnap m, FromJSON a) => m a
getBody = do
    f <- readRequestBody 1000000
    case decode f of
        Nothing -> error $ "Could not decode input parameters:"++show f
        Just a  -> return a

