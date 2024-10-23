{-# LANGUAGE QuasiQuotes #-}

module Main where

import GHC.Generics
import Control.Applicative
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
import Options.Applicative qualified as O
import System.Directory
import System.FilePath

-- Option parser
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

data Config = Config
  { route :: Route
  , input :: FilePath
  , output :: FilePath
  }
  deriving Show

config :: O.Parser Config
config = Config
   <$> ( O.flag' RIndex (O.long "index")
     <|> (RMeta <$> O.strOption (O.long "meta" <> O.metavar "TITLE"))
     <|> (RWiki <$> O.strOption (O.long "wiki" <> O.metavar "TITLE")))
   <*> O.strOption
        (O.short 'i' <> O.metavar "INPUT")
   <*> O.strOption
        (O.short 'o' <> O.metavar "OUTPUT")

getConfig :: IO Config
getConfig = O.execParser (O.info (config <**> O.helper) O.fullDesc)

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

-- main
readWikiFile :: FilePath -> IO (Metadata, T.Text)
readWikiFile path = do
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

main :: IO ()
main = do
  Config route input output <- getConfig
  case route of
    RIndex -> do
      metas <- map (dropExtension) <$> listDirectory (input </> "meta")
      wikis <- map (dropExtension) <$> listDirectory (input </> "wiki")
      let result = H.renderHtml (indexTemplate metas wikis)
      createDirectoryIfMissing True output
      BL.writeFile (output </> "index.html") result
    RMeta title -> do
      (meta, content) <- readWikiFile (input </> "meta" </> title ++ ".md")
      result <- P.runIOorExplode (translateDocument meta content)
      createDirectoryIfMissing True (output </> "meta")
      BL.writeFile (output </> "meta" </> title ++ ".html") result
    RWiki title -> do
      (meta, content) <- readWikiFile (input </> "wiki" </> title ++ ".md")
      result <- P.runIOorExplode (translateDocument meta content)
      createDirectoryIfMissing True (output </> "wiki")
      BL.writeFile (output </> "wiki" </> title ++ ".html") result
    RMainCss -> error "unreachable"
