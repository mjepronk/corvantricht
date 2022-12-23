{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------------------
import           Data.List (isInfixOf, intercalate)
import           Data.Char (toLower)
import           Hakyll

import           GHC.Generics
import           Control.Applicative (empty)
import           System.FilePath ((</>), dropExtension, makeRelative, splitFileName)
import           Data.Yaml
-- import qualified Text.Jasmine as JS

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.Lazy.Char8 as CL
--------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith siteConfig $ do
    match "static/img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "static/fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "static/js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "static/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "static/scss/corvantricht.scss" $ do
        route   $ gsubRoute "static/scss/" (const "static/css/") `composeRoutes` setExtension "css"
        compile $ scssCompiler "static/scss/"

    match "cgi-bin/*" $ do
        route   idRoute
        compile copyFileCompiler

    match ("gallery/*.jpg" .&&. complement "gallery/*.carousel.jpg") $
      version "normal" $ do
        route $ setExtension "normal.jpg"
        compile imageCompiler

    match ("gallery/*.jpg" .&&. complement "gallery/*.carousel.jpg") $
      version "thumbnail" $ do
        route $ setExtension "thumb.jpg"
        compile thumbnailCompiler

    match "gallery/*.carousel.jpg" $ do
        route   idRoute
        compile copyFileCompiler

    match "pages/index.html" $ do
        route $ customRoute pageRoute
        compile $ do
            paintings <- paintingsCompiler (fromFilePath "gallery.yaml")
            let paintings' = filter (carousel . itemBody) paintings
            let indexCtx = listField "paintings" paintingCtx (return paintings')
                           <> constField "title" "verkoop van schilderijen uit eigen atelier"
                           <> defaultContext
            getResourceBody
              >>= applyAsTemplate indexCtx
              >>= loadAndApplyTemplate "templates/default.html" indexCtx
              >>= relativizeUrls
              >>= removeIndexHtml

    create ["pages/schilderijen.html"] $ do
        route $ customRoute pageRoute
        compile $ do
          paintings <- paintingsCompiler (fromFilePath "gallery.yaml")
          let pageContext = listField "paintings" paintingCtx (return paintings)
                          <> constField "section-paintings" ""
                          <> defaultContext
          getResourceBody
            >>= applyAsTemplate pageContext
            >>= loadAndApplyTemplate "templates/paintings.html" pageContext
            >>= relativizeUrls
            >>= removeIndexHtml

    create [ "pages/de-kunstschilder.md"
           , "pages/de-pottenbakker.md"
           , "pages/de-restaurateur.md"
           ] $ do
        route $ customRoute pageRoute
        let pageContext = constField "section-about" ""
                          <> defaultContext
        compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/about.html" pageContext
          >>= loadAndApplyTemplate "templates/default.html" pageContext
          >>= relativizeUrls
          >>= removeIndexHtml

    create ["pages/contact.md"] $ do
        route $ customRoute pageRoute
        let pageContext = constField "section-contact" ""
                          <> defaultContext
        compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/page.html" pageContext
          >>= loadAndApplyTemplate "templates/default.html" pageContext
          >>= relativizeUrls
          >>= removeIndexHtml

    create ["pages/404.md"] $ do
        route $ constRoute "404.html"
        compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/page.html" defaultContext
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= removeIndexHtml

    match "templates/*" $ compile templateCompiler

    match "gallery.yaml" $ compile getResourceLBS


-- Hakyll config ---------------------------------------------------------------

siteConfig :: Configuration
siteConfig = defaultConfiguration
             { deployCommand = "rsync -av _site/ www@wilkes:/srv/www/corvantricht.nl/" }


-- Images ----------------------------------------------------------------------

imageCompiler :: Compiler (Item BL.ByteString)
imageCompiler = getResourceLBS >>= withItemBody resize
    where resize = unixFilterLBS "convert" ["jpg:-", "-colorspace", "sRGB", "-resize", "1400x1400",
                                            "-quality", "95", "jpg:-"]

thumbnailCompiler :: Compiler (Item BL.ByteString)
thumbnailCompiler = getResourceLBS >>= withItemBody resize
    where resize = unixFilterLBS "convert" ["jpg:-", "-colorspace", "sRGB", "-resize", "200x200",
                                            "-quality", "90", "jpg:-"]


-- Paintings -------------------------------------------------------------------

escapeHtmlEntities :: String -> String
escapeHtmlEntities []     = ""
escapeHtmlEntities (c:cs) = case c of
    '<'  -> "&lt;"   ++ escapeHtmlEntities cs
    '>'  -> "&gt;"   ++ escapeHtmlEntities cs
    '&'  -> "&amp;"  ++ escapeHtmlEntities cs
    '"'  -> "&quot;" ++ escapeHtmlEntities cs
    '\'' -> "&#39;"  ++ escapeHtmlEntities cs
    x    -> x : escapeHtmlEntities cs

slugify :: String -> String
slugify s = C8.unpack . C8.intercalate "-" . filter isEmpty . C8.splitWith isPunct
            $ C8.pack (fmap toLower s)
  where isPunct c = c `C8.elem` ("\t !\"#$%&'()*-\\/<=>?@[]^_`{}|,." :: C8.ByteString)
        isEmpty b = b /= C8.empty

data Painting = Painting
    { title :: String
    , image :: String
    , description :: String
    , carousel :: Bool
    } deriving (Show, Generic)

instance FromJSON Painting

paintingCtx :: Context Painting
paintingCtx = field "title" (return . escapeHtmlEntities . title . itemBody)
              <> field "image" (return . image . itemBody)
              <> field "description" (return . nl2comma . description . itemBody)
              <> field "slug" (return . slugify . title . itemBody)
              <> paintingImageField "imageNormal" (Just "normal") image
              <> paintingImageField "imageThumbnail" (Just "thumbnail") image
              <> carouselImageField "imageCarousel" image
  where nl2comma = intercalate ", " . lines

paintingImageField :: String -> Maybe String -> (Painting -> String) -> Context Painting
paintingImageField key ver f =
    field key $ fmap (maybe empty toUrl) . getRoute . identifier' . f . itemBody
  where identifier' i = setVersion ver $ fromFilePath $ "gallery" </> i

carouselImageField :: String -> (Painting -> String) -> Context Painting
carouselImageField key f =
    field key $ fmap (maybe empty toUrl) . getRoute . identifier' . f . itemBody
  where identifier' i = fromFilePath $ "gallery" </> (dropExtension i ++ ".carousel.jpg")

paintingsCompiler :: Identifier -> Compiler [Item Painting]
paintingsCompiler identifier = do
    r <- load identifier
    case decode' $ itemBody r :: Either ParseException [Painting] of
      Right p -> mapM makeItem p
      Left e  -> fail (show e)
  where decode' = decodeEither' . B.concat . BL.toChunks


-- Routes ----------------------------------------------------------------------

pageRoute :: Identifier -> FilePath
pageRoute ident
    | relPath == "index.html" = "index.html"
    | otherwise               = dropExtension relPath  </> "index.html"
  where relPath = makeRelative "pages/" (toFilePath ident)

removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ withUrls removeIndexStr <$> item
  where
    removeIndexStr :: String -> String
    removeIndexStr url = case splitFileName url of
        (dir, "index.html") | isLocal dir -> dir
        _                                 -> url
        where isLocal uri = not ("://" `isInfixOf` uri)


-- Compilers -------------------------------------------------------------------

-- minifyJSCompiler :: Compiler (Item String)
-- minifyJSCompiler = do
--   bs <- getResourceLBS
--   return $ itemSetBody (CL.unpack . JS.minify . itemBody $ bs) bs


scssCompiler :: FilePath -> Compiler (Item String)
scssCompiler loadpath = do
  s <- getResourceString
  item <- withItemBody (unixFilter "sassc"
      ["--stdin", "--load-path", loadpath]) s
  return $ compressCss <$> item
