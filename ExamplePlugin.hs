{-#LANGUAGE OverloadedStrings, RecordWildCards#-}
module Example where

import PluginType
import PluginMain
import Data.Aeson
import Data.Monoid
import qualified Data.Text.Lazy as LT

-- You can test this in ghci with:
-- > :set -XOverloadedStrings
-- > experiment (wrap toyPlugin) ("kissa"::LT.Text) ("koira"::LT.Text) 8000
--
-- To see the rendered plugin then open localhost:8000/index.html in browser.
-- When done, Ctrl-C will quit the plugin and return you to ghci. 

toyPlugin :: Plugin (State LT.Text) (Input LT.Text) (Web Value,Save LT.Text)
toyPlugin = let
    render (State txt) = return ("<p>This is a plugin</p><span id='thetextfield'>"<>txt<>"</span>\
                               \<button onclick=\"refresh(this,'kissa')\">Change</button>")
    update (Input txt) = do
        new <- render (State txt)
        return (Web (object ["html" .= new])
             ,Save txt)
    requirements = [JS "/stdlib.js",JS "https://code.jquery.com/jquery-2.1.4.min.js"]
    additionalFiles = []
    additionalRoutes = return ()
    in Plugin{..}
