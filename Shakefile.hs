module Main where

import Development.Shake
import Development.Shake.FilePath

translator :: String
translator = "bin/translator"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["all"]

    "all" ~> do
      metaSrcs <- getDirectoryFiles "src/meta" ["*.md"]
      wikiSrcs <- getDirectoryFiles "src/wiki" ["*.md"]
      cssSrcs  <- getDirectoryFiles "src/css"  ["*.css"]
      need $ ["site/index.html"]
          ++ ["site/meta" </> f -<.> "html" | f <- metaSrcs]
          ++ ["site/wiki" </> f -<.> "html" | f <- wikiSrcs]
          ++ ["site/css"  </> f             | f <- cssSrcs]

    translator %> \out -> do
        translatorSrcs <- getDirectoryFiles "" ["translator/**.hs"]
        need translatorSrcs
        cmd_ "mkdir" "-p" "bin"
        cmd_ "ghc" "translator/Main.hs" "-Wall" "-O3" "-outputdir=_build/translator" "-o" out

    "site/index.html" %> \_ -> do
        metaSrcs <- getDirectoryFiles "" ["src/meta/*.md"]
        wikiSrcs <- getDirectoryFiles "" ["src/wiki/*.md"]
        need $ [translator]
            ++ metaSrcs 
            ++ wikiSrcs
        cmd_ translator "--index" "-i" "src" "-o" "site"

    "site/meta/*.html" %> \out -> do
        let src = "src/meta" </> takeBaseName out <.> "md"
        need [src, "src/bibliography.bib", translator]
        cmd_ translator "--meta" [takeBaseName src] "-i" "src" "-o" "site"

    "site/wiki/*.html" %> \out -> do
        let src = "src/wiki" </> takeBaseName out <.> "md"
        need [src, "src/bibliography.bib", translator]
        cmd_ translator "--wiki" [takeBaseName src] "-i" "src" "-o" "site"

    "site/css/*.css" %> \out -> do
        let src = replaceDirectory1 out "src"
        need [src]
        cmd_ "mkdir" "-p" "site/css"
        cmd_ "cp" src out
