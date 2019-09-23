--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import           Data.Monoid (mappend)
import           Hakyll
import Data.String.Interpolate (i)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "comments/**/*" $ compile $ getResourceBody >>= (withItemBody (return . escapeHtml)) 

    match "*.markdown" $ do
        route $setExtension "html"
        compile $ do
            path <- getUnderlying
            comments <- chronological =<< loadAll [i|comments/#{path}/*|]
            let fullCtx =
                    listField "comments" postCtx (return comments) `mappend`
                    defaultContext

            pandocCompiler
                >>= applyAsTemplate fullCtx
                >>= loadAndApplyTemplate "templates/default.html" fullCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    dateField "hour" "%R"`mappend`
    defaultContext