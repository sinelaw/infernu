--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "less/**" $
        compile getResourceBody

    create ["css/main.css"] $ do
        route idRoute
        compile $ do
            getMatches "less/**"
            loadBody "less/all.less"
                >>= makeItem
                >>= withItemBody
                  (unixFilter "lessc" ["-"])

    match (fromList ["about.rst", "README.md"]) $ do -- , "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match (fromGlob "posts/*.md") $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll (fromGlob "posts/*.md")
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls



    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- loadAll (fromGlob "posts/*.md")
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
--                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
--    dateField "date" "%B %e, %Y" `mappend`
--    constField "date" "2015-01-01" `mappend`
    defaultContext
