{-#LANGUAGE OverloadedStrings#-}
module SimpleScripts where
import qualified Data.Text as T
import Data.Monoid

allJS :: T.Text
allJS = pluginWrapper<>pluginAddress<>answer<>refresh

pluginWrapper :: T.Text
pluginWrapper = "function pluginWrapper(domObject){\
                \var parent=domObject;\
                \while(!('data-plugin' in parent.attributes)&&parent !== null)\
                  \{parent=parent.parentElement};\
                \return parent;}\n"

pluginAddress :: T.Text
pluginAddress = "function pluginAddress(domObject){\
                \var parent=pluginWrapper(domObject);\
                \return parent.attributes['data-plugin'].nodeValue+'/'+parent.id;}\n"

refresh :: T.Text
refresh = "function refresh(domObject,data){\
            \var pluginUrl = pluginAddress(domObject);\
            \var pluginWrap = pluginWrapper(domObject);\
            \console.log([pluginUrl,pluginWrap]);\
            \$.ajax({url:'/'+pluginUrl+'/answer/'\
                   \,type:'PUT'\
                   \,data:JSON.stringify({input:data})\
                   \,contentType: 'application/json; charset=utf-8'\
                   \,dataType: 'json'\
                   \,success:function(r){console.log(['PW',r]); $(pluginWrap).html(r.web.html)}});}\n"

answer :: T.Text
answer = "function answer(domObject,data,handler){\
            \var pluginUrl = pluginAddress(domObject);\
            \$.ajax({url:'/'+pluginUrl+'/answer/'\
                   \,type:'PUT'\
                   \,data:JSON.stringify({input:data})\
                   \,contentType: 'application/json; charset=utf-8'\
                   \,dataType: 'json'\
                   \,success:handler});}\n"
