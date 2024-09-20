{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Generics
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Data.Coerce
import Data.Default
import Data.String
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Text.Blaze.Html.Renderer.Utf8 as H
import Text.Hamlet qualified as H
import Text.Pandoc qualified as P
import Text.Pandoc.Shared qualified as P
import Options.Applicative qualified as O

-- Option parser
data Config = Config
  { input :: FilePath
  , output :: FilePath
  }
  deriving Show

config :: O.Parser Config
config = Config
   <$> O.strOption
        (O.short 'i' <> O.metavar "INPUT")
   <*> O.strOption
        (O.short 'o' <> O.metavar "OUTPUT")

getConfig :: IO Config
getConfig = O.execParser (O.info (config <**> O.helper) O.fullDesc)

-- Metadata
data MathMethod = MathJax | KaTeX
  deriving (Generic, Show)

data Metadata = Metadata
  { title :: T.Text
  , categories :: [T.Text]
  , mathMethod :: MathMethod
  }
  deriving (Generic, Show)

instance A.FromJSON MathMethod
instance A.FromJSON Metadata

newtype AMetaValue = AMetaValue P.MetaValue
newtype AMeta = AMeta P.Meta

instance A.ToJSON AMetaValue where
  toJSON (AMetaValue v) =
    case v of
      P.MetaMap m     -> A.toJSON (coerce m  :: M.Map T.Text AMetaValue)
      P.MetaList xs   -> A.toJSON (coerce xs :: [AMetaValue])
      P.MetaBool b    -> A.toJSON b
      P.MetaString s  -> A.toJSON s
      P.MetaInlines s -> A.toJSON (P.stringify s)
      P.MetaBlocks s  -> A.toJSON (P.stringify s)

instance A.ToJSON AMeta where
  toJSON (AMeta (P.Meta m)) = A.toJSON (coerce m :: M.Map T.Text AMetaValue)

getMetadata :: P.Pandoc -> P.PandocIO Metadata
getMetadata (P.Pandoc meta _) =
  case A.fromJSON (A.toJSON (coerce meta :: AMeta)) of
    A.Error e   -> throwError (P.PandocSomeError (fromString e))
    A.Success m -> pure m

-- Math support
pandocMath :: MathMethod -> P.HTMLMathMethod
pandocMath MathJax = P.MathJax P.defaultMathJaxURL
pandocMath KaTeX   = P.KaTeX P.defaultKaTeXURL

mathScript :: MathMethod -> H.Html
mathScript MathJax =
  [H.shamlet|
    <script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js">
    <script>
      MathJax = {
        loader: {
          load: ['[custom]/xypic.js'],
          paths: {custom: 'https://cdn.jsdelivr.net/gh/sonoisa/XyJax-v3@3.0.1/build/'}
        },
        tex: {
          packages: {'[+]': ['xypic']}
        }
      };
  |]
mathScript KaTeX =
  [H.shamlet|
    <link rel="stylesheet"
          href="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css"
          integrity="sha384-nB0miv6/jRmo5UMMR1wu3Gz6NLsoTkbqJghGIsx//Rlm+ZU03BU6SQNC66uf4l5+"
          crossorigin="anonymous">
    <script defer
            src="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js"
            integrity="sha384-7zkQWkzuo3B5mTepMUcHkMB5jZaolc2xDwL6VFqjFALcbeS9Ggm/Yr2r3Dy4lfFg"
            crossorigin="anonymous">
    <script>
      document.addEventListener("DOMContentLoaded", () => {
        [...document.getElementsByClassName("math display")].forEach( element =>
          katex.render(element.textContent, element, {throwOnError: false, displayMode: true})
        );
        [...document.getElementsByClassName("math inline")].forEach( element =>
          katex.render(element.textContent, element, {throwOnError: false, displayMode: false})
        );
      });
  |]

-- HTML templates
page :: Metadata -> H.Html -> H.Html
page (Metadata title categories mathMethod) content =
  [H.shamlet|
    $doctype 5
    <html>
      <head>
        <meta charset="utf-8">
        <title> #{ title }
        #{ mathScript mathMethod }
      <body>
        <main>
          <h1> #{ title }
          #{ content }
  |]

-- document translator
readMarkdown :: T.Text -> P.PandocIO P.Pandoc
readMarkdown = P.readMarkdown options
  where
    options = def
      { P.readerExtensions = P.githubMarkdownExtensions
                          <> P.extensionsFromList
                              [ P.Ext_yaml_metadata_block
                              , P.Ext_tex_math_single_backslash
                              ]
      }

writeHtml :: Metadata -> P.Pandoc -> P.PandocIO H.Html
writeHtml (Metadata _ _ mathMethod) = P.writeHtml5 options
  where
    options = def
      { P.writerHTMLMathMethod = pandocMath mathMethod
      }

translateDocument :: T.Text -> P.PandocIO BL.ByteString
translateDocument content = do
  ast <- readMarkdown content
  meta <- getMetadata ast
  html <- page meta <$> writeHtml meta ast
  pure (H.renderHtml html)

-- main
main :: IO ()
main = do
  Config input output <- getConfig
  content <- T.readFile input
  result <- P.runIOorExplode (translateDocument content)
  BL.writeFile output result
