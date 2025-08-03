--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Text.Regex
import           System.FilePath
import           Data.List (groupBy)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Yaml (Value(..), decodeThrow)
import qualified Data.Yaml as Yaml
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS


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

    match "healthz" $ do
        route   idRoute
        compile copyFileCompiler
        
    match "papers/*.pdf" $ do
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
                                                      projects) `mappend` 
                    constField "yearlen" (show (length projects)) `mappend` 
                    constField "title" "Projects" `mappend`
                    defaultContext


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

    -- Recipe pages
    match "recipes/*" $ do
        route cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/recipe-item.html" recipeCtx
            >>= loadAndApplyTemplate "templates/default.html" recipeCtx
            >>= relativizeUrls

    -- Recipe listing page
    create ["recipes.html"] $ do
        route cleanRoute
        compile $ do
            recipes <- recentFirst =<< loadAll "recipes/*"
            let recipeListCtx = 
                    listField "recipes" recipeCtx (return recipes) `mappend`
                    constField "title" "Recipes" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/recipe-list.html" recipeListCtx
                >>= loadAndApplyTemplate "templates/default.html" recipeListCtx
                >>= relativizeUrls


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

recipeCtx :: Context String
recipeCtx =
    dateField "date" "%B %e, %Y" <>
    listFieldWith "ingredients" defaultContext getIngredients <>
    listFieldWith "instructions" defaultContext getInstructions <>
    cleanUrlField "url" <>
    metadataField <>
    defaultContext
  where
    getIngredients :: Item a -> Compiler [Item String]
    getIngredients = getListField "ingredients"
    
    getInstructions :: Item a -> Compiler [Item String]
    getInstructions = getListField "instructions"
    
    getListField :: String -> Item a -> Compiler [Item String]
    getListField field item = do
        metadata <- getMetadata (itemIdentifier item)
        case lookupStringList field metadata of
            Nothing -> return []
            Just xs -> mapM makeItem xs

-- Helper function to create clean URLs without index.html
cleanUrlField :: String -> Context a
cleanUrlField key = field key $ \item -> do
    route <- getRoute (itemIdentifier item)
    return $ case route of
        Nothing -> "/"
        Just url -> toAbsolutePath $ cleanIndexUrl url
  where
    cleanIndexUrl url
        | "/index.html" `T.isSuffixOf` T.pack url = 
            T.unpack (T.dropEnd 11 (T.pack url)) ++ "/"
        | otherwise = url
    
    toAbsolutePath url
        | "/" `T.isPrefixOf` T.pack url = url
        | otherwise = "/" ++ url

-- Groups article items by year (reverse order).
groupArticles :: [Item String] -> [(Int, [Item String])]
groupArticles = fmap merge . group . fmap tupelise
    where
        merge :: [(Int, [Item String])] -> (Int, [Item String])
        merge [] = error "merge: empty list"  -- This should never happen in practice
        merge (g:gs) = let conv (year, acc) (_, toAcc) = (year, toAcc ++ acc)
                       in  foldr conv g gs

        group ts   = groupBy (\(y, _) (y', _) -> y == y') ts
        tupelise i = let path = (toFilePath . itemIdentifier) i
                     in  case (articleYear . takeBaseName) path of
                             Just year -> (year, [i])
                             Nothing   -> error $
                                              "[ERROR] wrong format: " ++ path

-- Extracts year from article file name.
articleYear :: FilePath -> Maybe Int
articleYear s = case matchRegex articleRx s of
    Just matches -> case matches of
        (year:_) -> Just (read year)
        _        -> Nothing
    Nothing -> Nothing

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                            where p = toFilePath ident

---

articleRx :: Regex
articleRx = mkRegex "^([0-9]{4})\\-([0-9]{2})\\-([0-9]{2})\\-(.+)$"
