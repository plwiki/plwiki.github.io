{-# LANGUAGE QuasiQuotes #-}

module Main where

import GHC.Generics
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Data.Default
import Data.List
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Aeson qualified as A
import Data.Yaml.Aeson qualified as Y
import Text.Blaze.Html.Renderer.Utf8 as H
import Text.Hamlet qualified as H
import Text.Pandoc qualified as P
import Text.Pandoc.Builder qualified as P
import Text.Pandoc.Citeproc qualified as P
import System.Directory
import System.FilePath
import Development.Shake
import Development.Shake.FilePath
import Network.URI qualified as U

-- Route
data Route
  = RIndex
  | RMeta String
  | RWiki String
  | RMainCss
  deriving Show

relativeUrl :: Route -> FilePath
relativeUrl RIndex = ""
relativeUrl (RMeta title) = "meta" </> title ++ ".html"
relativeUrl (RWiki title) = "wiki" </> title ++ ".html"
relativeUrl RMainCss = "css/main.css"

renderUrl :: H.Render Route
renderUrl r _ = T.pack ("/" ++ relativeUrl r)

-- Metadata
data Metadata = Metadata
  { title :: T.Text
  , categories :: [T.Text]
  , license :: T.Text
  }
  deriving (Generic, Show)

instance A.FromJSON Metadata

-- HTML templates
mathScript :: H.Html
mathScript =
  [H.shamlet|
    <script>
      MathJax = {
        loader: {
          load: ['[tex]/bussproofs', '[custom]/xypic.js'],
          paths: {custom: 'https://cdn.jsdelivr.net/gh/sonoisa/XyJax-v3@3.0.1/build/'}
        },
        tex: {
          packages: {'[+]': ['bussproofs', 'xypic']}
        }
      };
    <script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js">
  |]

headerTemplate :: H.Html
headerTemplate =
  [H.hamlet|
    <header>
      <div class="content-box">
        <a id="site-title" href=@{ RIndex }> PL wiki
  |] renderUrl

categoriesTemplate :: [T.Text] -> H.Html
categoriesTemplate cs
  | null cs   = [H.shamlet| |]
  | otherwise =
    [H.hamlet|
      <div class="categories-list">
        <ul>
          #{ list }
    |] renderUrl
  where
    list = [ [H.hamlet| <li> #{ c } |] renderUrl | c <- cs ]

wikiTemplate :: Metadata -> H.Html -> H.Html
wikiTemplate (Metadata title categories _) content =
  [H.hamlet|
    $doctype 5
    <html>
      <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title> #{ title }
        <link rel="stylesheet" href=@{ RMainCss }>
        #{ mathScript }
      <body>
        #{ headerTemplate }
        <main>
          <article class="content-box">
            <h1 id="document-title"> #{ title }
            #{ categoriesTemplate categories }
            #{ content }
  |] renderUrl

indexTemplate :: [String] -> [String] -> H.Html
indexTemplate metas wikis =
  [H.hamlet|
    $doctype 5
    <html>
      <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title> PL wiki
        <link rel="stylesheet" href=@{ RMainCss }>
      <body>
        #{ headerTemplate }
        <main>
          <article class="content-box">
            PL wiki는 프로그래밍 언어론을 중심으로 컴퓨터 과학, 논리학, 수학, 철학에 대한 정보를 정리하기 위한 위키입니다.
            <h1> 색인
            <h2> 메타
            <ul id="meta-index"> #{ metalinks }
            <h2> 문서
            <ul id="wiki-index"> #{ wikilinks }
  |] renderUrl
  where
    metalinks = [ [H.hamlet|
                    <li>
                      <a href=@{ RMeta title }>
                        #{ title }
                  |] renderUrl
                | title <- sort metas
                ]
    wikilinks = [ [H.hamlet|
                    <li>
                      <a href=@{ RWiki title }>
                        #{ title }
                  |] renderUrl
                | title <- sort wikis
                ]

sitemapTemplate :: [String] -> String
sitemapTemplate locs = unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<urlset"
  , "    xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\""
  , "    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
  , "    xsi:schemaLocation="
  , "        \"http://www.sitemaps.org/schemas/sitemap/0.9"
  , "          http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd\">"
  , urls
  , "</urlset>"
  ]
  where
    urls = unlines ["<url>" ++ "<loc>" ++ loc ++ "</loc>" ++ "</url>" | loc <- locs]

-- document translator
readMarkdown :: P.PandocMonad m => T.Text -> m P.Pandoc
readMarkdown = P.readMarkdown options
  where
    options = def
      { P.readerExtensions = P.githubMarkdownExtensions
                          <> P.extensionsFromList
                              [ P.Ext_yaml_metadata_block
                              , P.Ext_tex_math_single_backslash
                              , P.Ext_tex_math_dollars
                              , P.Ext_citations
                              ]
      }

writeHtml :: P.PandocMonad m => P.Pandoc -> m H.Html
writeHtml = P.writeHtml5 options
  where
    options = def
      { P.writerHTMLMathMethod = P.MathJax P.defaultMathJaxURL
      }

translateDocument :: P.PandocMonad m => Metadata -> T.Text -> m BL.ByteString
translateDocument meta content = do
  ast0 <- readMarkdown content
  let ast1 = ( P.setMeta (T.pack "link-citations") True
             . P.setMeta (T.pack "csl") (T.pack "src/association-for-computing-machinery.csl")
             -- TODO : fix heading number for reference section title
             . P.setMeta (T.pack "reference-section-title") (T.pack "참고문헌")
             . P.setMeta (T.pack "bibliography") (T.pack "src/bibliography.bib")
             ) ast0
  ast2 <- P.processCitations ast1
  html <- wikiTemplate meta <$> writeHtml ast2
  pure (H.renderHtml html)

runPandocPure :: P.PandocPure a -> P.CommonState -> P.PureState -> Either P.PandocError a
runPandocPure m cs ps = ( flip evalState ps
                        . flip evalStateT cs
                        . runExceptT
                        . P.unPandocPure
                        ) m

-- Build system
handleError :: Either P.PandocError a -> Action a
handleError (Left e)  = fail (T.unpack (P.renderError e))
handleError (Right r) = pure r

readWikiFile :: MonadIO m => FilePath -> m (Metadata, T.Text)
readWikiFile path = liftIO $ do
  s <- BS.readFile path
  case BS.stripPrefix (fromString "---\n") s of
    Nothing -> error "no yaml header begin"
    Just s' -> do
      let (as, bs) = BS.breakSubstring (fromString "---\n") s'
      case BS.stripPrefix (fromString "---\n") bs of
        Nothing -> error "yaml header end"
        Just bs' -> do
          meta <- Y.decodeThrow as
          let content = T.decodeUtf8 bs'
          return (meta, content)

writeByteString :: MonadIO m => FilePath -> BL.ByteString -> m ()
writeByteString name x = liftIO $ do
  createDirectoryIfMissing True (takeDirectory name)
  BL.writeFile name x

domain :: String
domain = "https://plwiki.github.io"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ mconcat
  [ want ["all"]

  , "all" ~> do
      metaSrcs <- getDirectoryFiles "src/meta" ["//*.md"]
      wikiSrcs <- getDirectoryFiles "src/wiki" ["//*.md"]
      cssSrcs  <- getDirectoryFiles "src/css"  ["*.css"]
      need $ ["site/index.html", "site/sitemap.xml", "site/google4d4f29e2cef004bd.html"]
          ++ ["site/meta" </> f -<.> "html" | f <- metaSrcs]
          ++ ["site/wiki" </> f -<.> "html" | f <- wikiSrcs]
          ++ ["site/css"  </> f             | f <- cssSrcs]

  , "clean" ~> do
      putInfo "Cleaning..."
      removeFilesAfter "site" ["//"]

  , "site/index.html" %> \out -> do
        metaSrcs <- getDirectoryFiles "src/meta" ["//*.md"]
        wikiSrcs <- getDirectoryFiles "src/wiki" ["//*.md"]
        need $ ["src/meta" </> f | f <- metaSrcs]
            ++ ["src/wiki" </> f | f <- wikiSrcs]
        putInfo "Generating site/index.html"
        let result = H.renderHtml (indexTemplate (map dropExtension metaSrcs) (map dropExtension wikiSrcs))
        writeByteString out result

  , "site/sitemap.xml" %> \out -> do
        metaSrcs <- getDirectoryFiles "src/meta" ["//*.md"]
        wikiSrcs <- getDirectoryFiles "src/wiki" ["//*.md"]
        need $ ["src/meta" </> f | f <- metaSrcs]
            ++ ["src/wiki" </> f | f <- wikiSrcs]
        putInfo "Generating site/sitemap.txt"
        let result = sitemapTemplate $ [domain </> ""]
                                    ++ [domain </> "meta" </> U.escapeURIString U.isUnescapedInURI f -<.> "html" | f <- metaSrcs]
                                    ++ [domain </> "wiki" </> U.escapeURIString U.isUnescapedInURI f -<.> "html" | f <- wikiSrcs]
        writeFile' out result

  , "site/meta//*.html" %> \out -> do
        let src = replaceDirectory1 out "src" -<.> "md"
        need [src, "src/bibliography.bib", "src/association-for-computing-machinery.csl"]
        putInfo ("Generating " ++ out)
        (meta, content) <- readWikiFile src
        files <- liftIO $ foldM P.addToFileTree mempty ["src/bibliography.bib", "src/association-for-computing-machinery.csl"]
        result <- handleError $ runPandocPure (translateDocument meta content) def (def {P.stFiles = files})
        writeByteString out result

  , "site/wiki//*.html" %> \out -> do
        let src = replaceDirectory1 out "src" -<.> "md"
        need [src, "src/bibliography.bib", "src/association-for-computing-machinery.csl"]
        putInfo ("Generating " ++ out)
        (meta, content) <- readWikiFile src
        files <- liftIO $ foldM P.addToFileTree mempty ["src/bibliography.bib", "src/association-for-computing-machinery.csl"]
        result <- handleError $ runPandocPure (translateDocument meta content) def (def {P.stFiles = files})
        writeByteString out result

  , "site/css/*.css" %> \out -> do
        let src = replaceDirectory1 out "src"
        putInfo ("Copying " ++ out)
        copyFile' src out

  , "site/google4d4f29e2cef004bd.html" %> \out -> do
        let src = replaceDirectory1 out "src"
        putInfo ("Copying " ++ out)
        copyFile' src out
  ]
