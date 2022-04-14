--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Text.Regex
import           System.FilePath
import           Data.List (groupBy)


config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "CV.pdf" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "projects/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
  
    match "jobs/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["projects.html"] $ do
        route cleanRoute
        compile $ do
            projects <- fmap groupArticles $ recentFirst =<< loadAll "projects/*"
            let projectCtx = listField "years" (
                    field "year" (return . fst . itemBody) <>
                        listFieldWith "articles" articleCtx
                            (return . snd . itemBody)
                    )
                    (mapM (\(y, is) -> makeItem (show y, is))
                                                      projects) `mappend` constField "yearlen" (show (length projects)) `mappend` defaultContext


            makeItem ""
                >>= loadAndApplyTemplate "templates/project-list.html" projectCtx
                >>= loadAndApplyTemplate "templates/default.html"      projectCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            jobs <- recentFirst =<< loadAll "jobs/*"
            let aboutCtx =
                    listField "jobs" jobCtx (return jobs) `mappend`
                    defaultContext
            getResourceBody
                >>= applyAsTemplate aboutCtx
                >>= loadAndApplyTemplate "templates/default.html" aboutCtx
                >>= relativizeUrls

    match "contact.html" $ do
        route cleanRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
jobCtx :: Context String
jobCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
  
articleCtx :: Context String
articleCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

-- Groups article items by year (reverse order).
groupArticles :: [Item String] -> [(Int, [Item String])]
groupArticles = fmap merge . group . fmap tupelise
    where
        merge :: [(Int, [Item String])] -> (Int, [Item String])
        merge gs   = let conv (year, acc) (_, toAcc) = (year, toAcc ++ acc)
                     in  foldr conv (head gs) (tail gs)

        group ts   = groupBy (\(y, _) (y', _) -> y == y') ts
        tupelise i = let path = (toFilePath . itemIdentifier) i
                     in  case (articleYear . takeBaseName) path of
                             Just year -> (year, [i])
                             Nothing   -> error $
                                              "[ERROR] wrong format: " ++ path

-- Extracts year from article file name.
articleYear :: FilePath -> Maybe Int
articleYear s = read . head <$> matchRegex articleRx s

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                            where p = toFilePath ident

---

articleRx :: Regex
articleRx = mkRegex "^([0-9]{4})\\-([0-9]{2})\\-([0-9]{2})\\-(.+)$"
