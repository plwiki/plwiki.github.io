{-# LANGUAGE QuasiQuotes #-}

module Main where

import GHC.Generics
import Control.Monad.IO.Class
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

-- Route
data Route
  = RIndex
  | RMeta String
  | RWiki String
  | RMainCss
  deriving Show

relativeUrl :: Route -> FilePath
relativeUrl RIndex = "index.html"
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
            <h1> #{ title }
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

-- document translator
readMarkdown :: T.Text -> P.PandocIO P.Pandoc
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

writeHtml :: P.Pandoc -> P.PandocIO H.Html
writeHtml = P.writeHtml5 options
  where
    options = def
      { P.writerHTMLMathMethod = P.MathJax P.defaultMathJaxURL
      }

translateDocument :: Metadata -> T.Text -> P.PandocIO BL.ByteString
translateDocument meta content = do
  ast0 <- readMarkdown content
  let ast1 = ( P.setMeta (T.pack "link-citations") True
             . P.setMeta (T.pack "bibliography") (T.pack "src/bibliography.bib")
             ) ast0
  ast2 <- P.processCitations ast1
  html <- wikiTemplate meta <$> writeHtml ast2
  pure (H.renderHtml html)

-- Build system
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

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ mconcat
  [ want ["all"]

  , "all" ~> do
      metaSrcs <- getDirectoryFiles "src/meta" ["*.md"]
      wikiSrcs <- getDirectoryFiles "src/wiki" ["*.md"]
      cssSrcs  <- getDirectoryFiles "src/css"  ["*.css"]
      need $ ["site/index.html"]
          ++ ["site/meta" </> f -<.> "html" | f <- metaSrcs]
          ++ ["site/wiki" </> f -<.> "html" | f <- wikiSrcs]
          ++ ["site/css"  </> f             | f <- cssSrcs]

  , "site/index.html" %> \out -> do
        metaSrcs <- getDirectoryFiles "" ["src/meta/*.md"]
        wikiSrcs <- getDirectoryFiles "" ["src/wiki/*.md"]
        need $ metaSrcs ++ wikiSrcs
        let result = H.renderHtml (indexTemplate (map takeBaseName metaSrcs) (map takeBaseName wikiSrcs))
        writeByteString out result

  , "site/meta/*.html" %> \out -> do
        let src = "src/meta" </> takeBaseName out <.> "md"
        need [src, "src/bibliography.bib"]
        (meta, content) <- liftIO $ readWikiFile src
        result <- liftIO $ P.runIOorExplode (translateDocument meta content)
        writeByteString out result

  , "site/wiki/*.html" %> \out -> do
        let src = "src/wiki" </> takeBaseName out <.> "md"
        need [src, "src/bibliography.bib"]
        (meta, content) <- readWikiFile src
        result <- liftIO $ P.runIOorExplode (translateDocument meta content)
        writeByteString out result

  , "site/css/*.css" %> \out -> do
        let src = replaceDirectory1 out "src"
        copyFile' src out

  ]
