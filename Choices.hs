{-#LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards, DeriveGeneric, CPP, ParallelListComp#-}
module Choices where

import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import Data.Ratio
import Data.Maybe
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Text.Pandoc
import qualified Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import Debug.Trace

import PluginType

data MC
data MMC

data MCQMarkup mckind choice 
    = MCM {stem    :: T.Text
          ,choices :: [choice]
          ,onTry   :: Maybe T.Text
          ,headerText  :: Maybe T.Text
          ,buttonText  :: Maybe T.Text
          } 
      deriving (Show,Generic)

instance ToJSON a => ToJSON (MCQMarkup x a) where

parsingOptions = defaultOptions{omitNothingFields=True}
instance FromJSON a => FromJSON (MCQMarkup x a) where
    parseJSON = genericParseJSON parsingOptions


data Choice = Choice {text :: T.Text
                     ,correct :: Maybe Bool
                     ,reason :: Maybe T.Text
                     } deriving (Show,Generic)

instance FromJSON Choice where
    parseJSON = genericParseJSON parsingOptions

instance ToJSON   Choice where

newtype Blind = Blind T.Text deriving (Show, Generic)

instance ToJSON Blind where
    toJSON (Blind t) = object ["text" .= t]

instance FromJSON Blind where
    parseJSON (Object v) = Blind <$> v .: "text"
    parseJSON invalid = typeMismatch "Blind" invalid

blind :: MCQMarkup mckind Choice -> MCQMarkup mckind Blind
blind MCM{..} = MCM{choices=map hide choices,..}

hide :: Choice -> Blind
hide choice = Blind (text choice) -- Blind . text

data SemiBlind = Blinded Blind | Open Choice deriving (Show,Generic)
-- (a,b) = foo()
instance ToJSON SemiBlind where
    toJSON (Open c) = toJSON c
    toJSON (Blinded b) = toJSON b

blindSemi :: MCQMarkup a Choice -> [Bool] -> MCQMarkup a SemiBlind
blindSemi MCM{..} toHide = MCM{choices=[if h then Blinded (hide c) else Open c | h <- toHide | c <- choices],..}


mdOpts = def{writerHTMLMathMethod=MathJax
              "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"}
formatMarkdown :: T.Text -> T.Text
#if MIN_VERSION_pandoc(1,4,0)
formatMarkdown =
    either
        (LT.toStrict . renderHtml . Text.Blaze.Html.toHtml . show)
        (LT.toStrict . renderHtml . writeHtml mdOpts) .
    readMarkdown def . T.unpack
#else
formatMarkdown =
    LT.toStrict . renderHtml . writeHtml mdOpts . readMarkdown def . T.unpack
#endif

class Typesettable a where
   typeset :: a -> a

instance Typesettable a =>
         Typesettable (MCQMarkup x a) where
  typeset MCM {..} =
    MCM
    { stem = formatMarkdown stem
    , headerText = fmap formatMarkdown headerText
    , choices = map typeset choices
    , buttonText = fmap formatMarkdown buttonText
    , ..
    }

instance Typesettable Choice where
  typeset (Choice t c r) = Choice (formatMarkdown t) c (formatMarkdown <$> r)

instance Typesettable Blind where
  typeset (Blind t) = Blind (formatMarkdown t)

instance Typesettable (SemiBlind) where
  typeset (Open a) = Open (typeset a)
  typeset (Blinded a) = Blinded (typeset a)


countChoices :: (MCQMarkup mmc Choice) -> Integer
countChoices MCM {..} =
  fromIntegral
    (length
       [ ()
       | c <- choices 
       , isJust (correct c) 
       ])

countSuccessSingle :: (MCQMarkup mmc Choice) -> SMCAnswer -> Integer
countSuccessSingle _ Blank = 0
countSuccessSingle MCM {..} answer =
  fromIntegral $
  length
    [ ()
    | (n, c) <- zip [0 ..] choices 
    , Selection n == answer 
    , correct c == Just True ]

countSuccess :: (MCQMarkup mmc Choice) -> [Maybe Bool] -> Integer
countSuccess MCM {..} es =
  fromIntegral $
  length
    [ ()
    | (Just a, c) <- zip es choices 
    , Just a == correct c ]

countSuccessSemi :: (MCQMarkup mmc SemiBlind) -> [Maybe Bool] -> Integer
countSuccessSemi MCM{..} es = fromIntegral $ length [()|(Just a,c) <- zip es choices, Just a==choiceIsCorrect c]

choiceIsCorrect :: SemiBlind -> Maybe Bool
choiceIsCorrect (Open c)    = correct c
choiceIsCorrect (Blinded _) = Nothing


multipleMultipleChoice :: Plugin (Markup (MCQMarkup MMC Choice), State (Maybe [Maybe Bool])) 
                                 (Markup (MCQMarkup MMC Choice), TaskID, Input ([Maybe Bool])) 
                                 (Save (Maybe [Maybe Bool])
                                 ,Web Value
                                 ,TimInfo Value  
                                 ,BlackboardOut)
multipleMultipleChoice =
    Plugin
    { ..
    }
  where
    requirements = [JS "SimpleDirective.js", JS "script2.js", NGModule "MCQ"]
    additionalFiles = ["MMCQTemplate.html"]
    typesetSemi mcm inputs = typeset (blindSemi mcm (map isNothing inputs++repeat True))
    update (Markup mcm, TID taskID, Input i) =
        return $
        ( Save (Just i)
        , Web (object ["state" .= i, "question" .= typesetSemi mcm i ])
        , TimInfo
              (object
                   [ "vars" .=
                     object
                         [ "correct" .= countSuccess mcm i
                         , "count"   .= countChoices mcm]
                   , "points" .= countSuccess mcm i])
        , BlackboardOut (catMaybes [fmap Put (onTry mcm), Just (Put taskID)]))
    render (Markup mcm, State state) =
        return $
        case state of
            Just i ->
                ngDirective "mmcq" $
                object ["question" .= typesetSemi mcm i
                       , "state" .= Just i]
            Nothing ->
                ngDirective "mmcq" $
                object
                    [ "question" .= typeset (blindSemi mcm (repeat True))
                    , "state" .= (Nothing :: Maybe ())]
    additionalRoutes = noRoutes
                                

data SMCAnswer = Blank | Selection Int deriving (Eq,Ord,Show,Generic)
instance Monoid SMCAnswer where
    mempty = Blank
    mappend a b = b
instance ToJSON SMCAnswer where
    toJSON Blank = toJSON (Nothing::Maybe Int)
    toJSON (Selection i) = toJSON (Just i)
instance FromJSON SMCAnswer where
    parseJSON x = do
        p <- parseJSON x
        case p of
            Nothing -> return Blank 
            Just i  -> return (Selection i)

simpleMultipleChoice :: Plugin (Markup (MCQMarkup MC Choice), State SMCAnswer) 
                                 (Markup (MCQMarkup MC Choice),TaskID, Input SMCAnswer) 
                                 (Save SMCAnswer,Web Value,BlackboardOut,TimInfo Value)
simpleMultipleChoice =
  Plugin
  { ..
  }
  where
    requirements = [JS "SimpleDirective.js", JS "script2.js", NGModule "MCQ"]
    additionalFiles = ["MCQTemplate.html"]
    update (Markup mcm, TID taskID, Input i) =
      return $
      ( Save i
      , Web (object ["state" .= i, "question" .= typeset mcm])
      , BlackboardOut (catMaybes [fmap Put (onTry mcm), Just (Put taskID)])
      , TimInfo
              (object
                   [ "vars" .=
                     object
                         [ "correct" .= countSuccessSingle mcm i
                         , "count"   .= length (choices mcm)]
                   , "points" .= countSuccessSingle mcm i]))
    render (Markup mcm, State state) =
      return $
      case state of
        Selection i -> 
          ngDirective "mcq" $
          object ["question" .= typeset mcm, "state" .= state]
        Blank -> traceShow ("NoState!")
          ngDirective "mcq" $
          object
            [ "question" .= typeset (blind mcm)
            , "state" .= Blank]
    additionalRoutes = noRoutes

--testQ :: MCQMarkup MC Choice
--testQ = MCM "Valitse kissa" [Choice "Koira" False "Piti valita kissa"
--                            ,Choice "Kissa" True  "Kissat Rulez"
--                            ,Choice "Kani"  False "Kissa voi syödä kanin"]
--
--testMQ :: MCQMarkup MMC Choice
--testMQ = MCM "Valitse eläin" [Choice "Koira" True "Joo"
--                            ,Choice "Kissa" True  "On"
--                            ,Choice "Perl"  False "Ei oo"]
