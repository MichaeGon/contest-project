module Templates (
    srcBodyQ,
    defaultContentsQ
) where

import Language.Haskell.TH

srcBodyQ :: ExpQ
srcBodyQ = LitE . StringL <$> runIO (readFile "src/contest.hsfiles")

defaultContentsQ :: ExpQ
defaultContentsQ = LitE . StringL <$> runIO (readFile "src/defaultContents.hs")