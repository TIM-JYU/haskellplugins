{-#LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards, DeriveGeneric#-}
module Main where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LT
import Snap.Http.Server

import PluginType

data NoteMarkup = NM {width,height::Int,prompt::T.Text} deriving (Show,Generic)
data NoteCommands = SaveNote T.Text | SubmitNote T.Text deriving (Show,Generic)
instance FromJSON NoteMarkup where
instance ToJSON NoteMarkup where

shortNote :: Plugin (Markup NoteMarkup, State T.Text)
                    (Markup NoteMarkup, Input T.Text)
                    (Save T.Text, Web Value)
shortNote = Plugin{..}
  where 
    requirements = [JS "shortNote.js"
                   ,NGModule "Note"]
    additionalFiles = ["ShortNoteTemplate.html"]
    initial = ""
    update (Markup markup,Input i) = return $ (Save i, Web (object ["content" .= encodeState markup i]))
    encodeState markup state =  object ["width"   .= width markup
                                       ,"height"  .= height markup
                                       ,"prompt"  .= prompt markup
                                       ,"content" .= state]
    render (Markup markup,State state) = return $ ngDirective "shortNote" $ encodeState markup state
    additionalRoutes = noRoutes
                                
main :: IO ()
main = quickHttpServe $ serve shortNote

testNote :: NoteMarkup 
testNote = NM 80 15 "Kerro kissasta"
