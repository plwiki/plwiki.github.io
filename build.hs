import Development.Shake
import Development.Shake.FilePath

translator :: String
translator = "bin/translator"

main :: IO ()
main = shakeArgs shakeOptions $ do
    want ["all"]

    "all" ~> do
      metaSrcs <- getDirectoryFiles "src/meta" ["*.md"]
      wikiSrcs <- getDirectoryFiles "src/wiki" ["*.md"]
      cssSrcs  <- getDirectoryFiles "src/css"  ["*.css"]
      need $ ["docs/index.html"]
          ++ ["docs/meta" </> f -<.> "html" | f <- metaSrcs]
          ++ ["docs/wiki" </> f -<.> "html" | f <- wikiSrcs]
          ++ ["docs/css"  </> f             | f <- cssSrcs]

    translator %> \out -> do
        need ["translator/Main.hs"]
        cmd_ "mkdir" "-p" "bin"
        cmd_ "ghc" "translator/Main.hs" "-Wall" "-O3" "-o" out

    "docs/index.html" %> \_ -> do
        metaSrcs <- getDirectoryFiles "" ["src/meta/*.md"]
        wikiSrcs <- getDirectoryFiles "" ["src/wiki/*.md"]
        need $ [translator]
            ++ metaSrcs 
            ++ wikiSrcs
        cmd_ translator "--index" "-i" "src" "-o" "docs"

    "docs/meta/*.html" %> \out -> do
        let src = "src/meta" </> takeBaseName out <.> "md"
        need [src, translator]
        cmd_ translator "--meta" [takeBaseName src] "-i" "src" "-o" "docs"

    "docs/wiki/*.html" %> \out -> do
        let src = "src/wiki" </> takeBaseName out <.> "md"
        need [src, translator]
        cmd_ translator "--wiki" [takeBaseName src] "-i" "src" "-o" "docs"

    "docs/css/*.css" %> \out -> do
        let src = replaceDirectory1 out "src"
        need [src]
        cmd_ "mkdir" "-p" "docs/css"
        cmd_ "cp" src out
