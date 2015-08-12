{-#LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables, DataKinds, MultiParamTypeClasses, FlexibleInstances, PolyKinds, DeriveFunctor, FlexibleContexts#-}
module PluginType where

import GHC.Generics

import Data.Aeson 
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.Builder as LT
import Data.Hashable
import HTMLEntities.Builder
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List
import Control.Monad.Trans.Either

import Snap.Core
import Snap.Util.Readable

import UtilityPrelude

-- | Simplified interface for TIM plugins. 
-- The plugin has three parameters - the information it requires for rendering 
-- itself as Html (`renderP`), the information required to update the itself
-- according to user actions (`updateP`) and finally the information it outputs
-- back to the web services (`outputP`). 
-- 
-- To actually use the resulting plugin, the `renderP` and the `updateP` types must
-- belong to `Available TimRender` and `Available TimUpdate` classes respectively.
-- The most common members of these classes are:
--
-- Type                        | Available TimRender | Available TimUpdate | Use
-- ()                          | X                   | X                   | Indicates unnecessary argument
-- 2-4 tuples                  | X                   | X                   | Free
-- TaskID                      | X                   | X                   | Identifier of the current plugin
-- To/FromJson x a => State a  | X                   | X                   | Stored plugin state
-- To/FromJson x a => Markup a | X                   | X                   | Converted from the document markup 
-- To/FromJson x a => Input a  |                     | X                   | Input received from the web (javascript+user)
-- User                        | X                   | X                   | Identifier of user viewing the plugin
-- Also, all used fields must be instances of From/ToJson classes from Aeson package.
-- 
-- The `outputP` describes the result of the plugin call. It must be in `Reply TimResult` class, which
-- contains the following instances:
--
-- Type                                       | Use
-- ()                                         | Indicates no reply needed
-- 2-4 tuples                                 | Indicate multiple results, such as sending something to the browser
--                                            | and saving it to a plugin state
-- (ToJSON a) => Reply TimResult (Save a)     | The result saved to db. Fetched by `State´ in update/reply
-- (ToJSON a) => Reply TimResult (Web a)      | The result sent to the web browser. Must be json encodeable
-- (ToJSON a) => Reply TimResult (TimInfo a)  | The result sent to the surrounding system. Used to indicate
--                                            | exercise scores and such (not implemented fully yet)
--
-- See `https://tim.it.jyu.fi/view/tim/TIMin%20kehitys/Plugin-speksi` for
-- more information.
data Plugin renderP updateP outputP = Plugin 
        { render  :: renderP -> IO LT.Text -- ^Convert render parameters into HTML
        , update  :: updateP -> IO outputP -- ^Produce output on user action
        , requirements :: [Requirement]    -- ^Which scripts/css/angular modules are required?
        , additionalFiles :: [FilePath]    -- ^Additional files such as angular templates
        , additionalRoutes :: Snap ()}     -- ^Anything else that is 'global' to this kind of a plugin?

-- Stolen from Control.Lens
(&) :: a -> (a -> b) -> b
a & f = f a
{-# INLINE (&) #-}


-- Blackboard specialization
data BlackboardCommand = Put T.Text | Delete T.Text deriving (Eq,Ord,Show)

keyOf :: BlackboardCommand -> T.Text
keyOf (Put t)    = t
keyOf (Delete t) = t

instance FromJSON BlackboardCommand where
    parseJSON (String x) 
        | "!" `T.isPrefixOf` x = pure $ Delete (T.tail x)
        | otherwise            = pure $ Put x
    parseJSON e = fail ("Expected String, got "++show e)

instance ToJSON BlackboardCommand where
    toJSON (Put t) = String t
    toJSON (Delete t) = String (T.cons '!' t)

execBBCs :: HashSet T.Text -> [BlackboardCommand] -> HashSet T.Text
execBBCs hm bbcs = foldl' execBBC hm bbcs

execBBC :: HashSet T.Text -> BlackboardCommand -> HashSet T.Text
execBBC hm (Put t)    = HashSet.insert t hm
execBBC hm (Delete t) = HashSet.delete t hm


-- Generating Angular directives with embedded json

ngDirective :: ToJSON a => LT.Text -> a -> LT.Text
ngDirective tag content = LT.toLazyText $ 
                             "<"<>LT.fromLazyText tag<>" data-content='"
                             <>text (LT.toStrict . LT.decodeUtf8 . encode $ content)
                             <>"'></"<>LT.fromLazyText tag<>">"

noRoutes :: Snap ()    
noRoutes = return ()

-- | Various requirements that the plugin might have.
data Requirement = JS T.Text
                 | CSS T.Text
                 | NGModule T.Text
                 deriving (Show,Generic) 

instance ToJSON Requirement where
    toJSON (JS  t)      = object ["js"  .= t]
    toJSON (CSS t)      = object ["css" .= t]
    toJSON (NGModule t) = object ["angularModule" .= t]

data Stage = Render | Update deriving (Eq,Show)

data AR (s::k) a = AR (EitherT String IO a) deriving Functor
instance Monad (AR s)  where
    return = pure
    (AR eit) >>= fb = AR $ do
                        a <- eit
                        let AR b = fb a
                        b

                         
instance Applicative (AR s)  where
    pure x = AR (pure x)
    (AR a)<*>(AR b) = AR (a<*>b) 

instance Alternative (AR s) where
    empty = AR $ left "empty"
    AR a<|>AR b = AR $ a<|> b

runAR :: forall (t :: k) a. AR t a -> IO (Either String a)
runAR (AR x) = runEitherT x

ar :: forall (t :: k) c b. AR t b -> (String -> IO c) -> (b -> IO c) -> IO c
ar (AR x) toLeft toRight = eitherT toLeft toRight x

class Available s a where
    getIt :: s -> AR s a

instance Available x () where
    getIt _ = pure ()

instance (Available x a, Available x b) => Available x (a,b) where
    getIt v = (,) <$> getIt v <*> getIt v

instance (Available x a, Available x b, Available x c) => Available x (a,b,c) where
    getIt v = (,,) <$> getIt v <*> getIt v <*> getIt v

instance (Available x a, Available x b, Available x c, Available x d) => Available x (a,b,c,d) where
    getIt v = (,,,) <$> getIt v <*> getIt v <*> getIt v <*> getIt v

class Reply s a where
    putIt :: s -> a -> IO s

instance Reply x () where
    putIt s _ = pure s

instance (Reply x a, Reply x b) => Reply x (a,b) where
    putIt s (v1,v2) = putIt s v1 >>= flip putIt v2

instance (Reply x a, Reply x b, Reply x c) => Reply x (a,b,c) where
    putIt s (v1,v2,v3) = putIt s v1 >>= flip putIt v2 >>= flip putIt v3

instance (Reply x a, Reply x b, Reply x c, Reply x d) => Reply x (a,b,c,d) where
    putIt s (v1,v2,v3,v4) = putIt s v1 >>= flip putIt v2 >>= flip putIt v3 >>= flip putIt v4

newtype State  a = State a deriving (Eq,Show)
newtype Markup a = Markup a deriving (Eq,Show)
newtype Input  a = Input a deriving (Eq,Show)
newtype User   = User T.Text deriving (Eq,Show)
newtype Blackboard = Blackboard (HashSet T.Text) deriving (Eq,Show)
instance Monoid Blackboard where
    mempty = Blackboard mempty
    mappend (Blackboard a) (Blackboard b) = Blackboard (mappend a b)

newtype TaskID  = TID {getTaskID :: T.Text} deriving (Eq,Show)
instance Hashable TaskID where
    hashWithSalt s (TID d) = hashWithSalt (s*7) d
instance Readable TaskID where
    fromBS = return . TID . T.decodeUtf8
instance FromJSON TaskID where
  parseJSON (String s) = pure (TID s)
  parseJSON x = fail $ "Expected taskID, got "++show x

instance ToJSON TaskID where
    toJSON (TID x) = String x

newtype Save a     = Save a deriving (Eq,Show)
newtype Web a      = Web a deriving (Eq,Show)
newtype TimInfo a  = TimInfo a deriving (Eq,Show)
newtype BlackboardOut = BlackboardOut [BlackboardCommand]  deriving (Eq,Show)

newtype TimResult  = TR [(T.Text,Value)] deriving Show
instance ToJSON TimResult where
    toJSON (TR a) = object a

instance (ToJSON a) => Reply TimResult (Save a) where
    putIt (TR x) (Save v) = return $ TR (("save".=v):x)

instance (ToJSON a) => Reply TimResult (Web a) where
    putIt (TR x) (Web v) = return $ TR (("web".=v):x)

instance (ToJSON a) => Reply TimResult (TimInfo a) where
    putIt (TR x) (TimInfo v) = return $ TR (("tim_info".=v):x)

instance Reply TimResult BlackboardOut where
    putIt (TR x) (BlackboardOut bc) = return $ TR (("bb".=bc):x)

-- PluginSpecific
newtype TimRender = TimRender Value
newtype TimUpdate = TimUpdate Value

instance (Monoid a, FromJSON a) => Available TimRender (State a) where
    getIt (TimRender x) = (State <$> getField "state" x)
                          <|> pure (State mempty)
instance (Monoid a, FromJSON a) => Available TimUpdate (State a) where
    getIt (TimUpdate x) = (State <$> getField "state" x)
                          <|> pure (State mempty)
instance FromJSON a => Available TimRender (Markup a) where
    getIt (TimRender x) = Markup <$> getField "markup" x 
instance FromJSON a => Available TimUpdate (Markup a) where
    getIt (TimUpdate x) = Markup <$> getField "markup" x

instance Available TimRender TaskID where
    getIt (TimRender x) = TID <$> getField "taskID" x 
instance Available TimUpdate TaskID where
    getIt (TimUpdate x) = TID <$> getField "taskID" x

instance FromJSON a => Available TimUpdate (Input a) where
    getIt (TimUpdate x) = Input <$> getField "input" x 

getField :: forall (s :: k) a. FromJSON a => T.Text -> Value -> AR s a
getField f (Object v) = case HashMap.lookup f v of
                    Nothing -> AR $ left ("No key '"++show f++"' in "++show (Object v))
                    Just s  -> case fromJSON s of
                        Error e   -> AR $ left e
                        Success a -> AR $ right a
getField _ x = AR $ left ("Expected object, got "++show x)
