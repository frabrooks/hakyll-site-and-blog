--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Web.Sass (sassCompiler)
import qualified Data.Text as T

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "js/*" $ do
      route idRoute
      compile copyFileCompiler

    match "fonts/*" $ do
      route idRoute
      compile copyFileCompiler

  
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "CV/cv.pdf" $ do
      route $ customRoute $ (\ _ -> "pdfs/cv.pdf")
      compile copyFileCompiler
        
    match "pdfs/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromRegex "scss/[^_]*.scss|scss/*/[^_]*.scss") $ do
        let css = (T.pack "css")
        let scss = (T.pack "scss")
        let replace = T.replace scss css
        route $ customRoute $ T.unpack  . replace . T.pack  . toFilePath
        compile sassCompiler


    match "scss/pages/*.scss" $ do
        let css = (T.pack "css")
        let scss = (T.pack "scss")
        let replace = T.replace scss css
        route $ customRoute $ T.unpack  . replace . T.pack  . toFilePath
        compile sassCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/root.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/root.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/root.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/root.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
