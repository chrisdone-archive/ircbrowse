{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ircbrowse.View.Template where

import Ircbrowse.View

template name inner = do
  docType
  html $ do
    head $ do link ! rel "stylesheet" ! type_ "text/css" ! href "/css/bootstrap.min.css"
              link ! rel "stylesheet" ! type_ "text/css" ! href "/css/bootstrap-responsive.css"
              link ! rel "stylesheet" ! type_ "text/css" ! href "/css/ircbrowse.css"
              meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
    body !# name $ do
      inner
      preEscapedText "<script type=\"text/javascript\"> var _gaq = _gaq \
                     \|| []; _gaq.push(['_setAccount', 'UA-38975161-1']);\
                     \ _gaq.push(['_trackPageview']); (function() {var ga\
                     \ = document.createElement('script'); ga.type = 'tex\
                     \t/javascript'; ga.async = true; ga.src = ('https:' \
                     \== document.location.protocol ? 'https://ssl' : \
                     \'http://www') + '.google-analytics.com/ga.js'; var\
                     \ s = document.getElementsByTagName('script')[0]; \
                     \s.parentNode.insertBefore(ga, s);})(); </script>"

showCount :: (Show n,Integral n) => n -> String
showCount = reverse . foldr merge "" . zip ("000,00,00,00"::String) . reverse . show where
  merge (f,c) rest | f == ',' = "," ++ [c] ++ rest
                   | otherwise = [c] ++ rest
