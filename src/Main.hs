{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Exception
import Options
import System.Directory
import System.IO
import System.Process (callProcess)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data OptionData = OptionData {
    optNumber :: Int,
    optStart :: String
} deriving (Show, Eq)

instance Options OptionData where
    defineOptions = OptionData
        <$> simpleOption "number" 4 "Number of questions"
        <*> simpleOption "start" "a" "Name of the first question"

main :: IO ()
main = runCommand createProject

createProject :: OptionData -> [String] -> IO ()
createProject opt (dist : _) = bracketOnError createTemplate removeTemplate initializeStack
    where
        createTemplate = openTempFile "." "contest.hsfiles"

        removeTemplate (file, h) = hClose h
                                >> removeFile file

        initializeStack (file, h) = T.hPutStr h contents
                                >> hClose h
                                >> callProcess "stack" ["new", dist, file]
                                >> removeFile file

        contents = makeHsfiles opt 

createProject _ _ = error "no project name applied"

makeHsfiles :: OptionData -> T.Text
makeHsfiles OptionData {optNumber = n, optStart = (c : _)} = T.unlines $ header : mainis c : srcs n c
makeHsfiles _ = error "Empty question name"

header :: T.Text
header = T.unlines 
    [ "{-# START_FILE Setup.hs #-}"
    , "import Distribution.Simple"
    , "main = defaultMain"
    , ""
    , "{-# START_FILE README.md #-}"
    , "# {{name}}"
    , ""
    , "Template for Programming Contest in Haskell "
    , ""
    , "{-# START_FILE LICENSE #-}"
    , ""
    , "{-# START_FILE {{name}}.cabal #-}"
    , "name:                {{name}}"
    , "version:             0.1.0.0"
    , "homepage:            https://github.com/{{github-username}}{{^github-username}}githubuser{{/github-username}}/{{name}}#readme"
    , "license:             BSD3"
    , "license-file:        LICENSE"
    , "author:              {{author-name}}{{^author-name}}Author name here{{/author-name}}"
    , "maintainer:          {{author-email}}{{^author-email}}example@example.com{{/author-email}}"
    , "copyright:           {{copyright}}{{^copyright}}{{year}}{{^year}}2019{{/year}} {{author-name}}{{^author-name}}Author name here{{/author-name}}{{/copyright}}"
    , "category:            {{category}}{{^category}}Web{{/category}}"
    , "build-type:          Simple"
    , "cabal-version:       >=1.10"
    , "extra-source-files:  README.md"
    , ""
    , "executable {{name}}"
    , "  hs-source-dirs:      src"
    , "  default-language:    Haskell2010"
    , "  build-depends:       base >= 4.7 && < 5"
    --, "  main-is: a.hs"
    --, ""
    ]

mainis :: Char -> T.Text
mainis c = T.unlines
    [ T.concat ["  main-is: ", T.pack [c], ".hs"]
    , ""
    ]

srcs :: Int -> Char -> [T.Text]
srcs n c = map mf $ take n [c..]
    where
        mf x = T.unlines $ srcHeader x : srcContents
        srcHeader x = T.concat ["{-# START_FILE src/", T.pack [x], ".hs #-}"]

srcContents :: [T.Text]
srcContents =
    [ "import Control.Arrow"
    , "import Control.Monad"
    , "import Data.List"
    , "import qualified Data.ByteString.Char8 as B"
    , ""    
    , "main :: IO ()"
    , "main = undefined "
    , ""
    , "solve :: Int"
    , "solve = undefined"
    , ""
    , "readLine :: IO [Int]"
    , "readLine = unfoldr (\\s -> second (maybe B.empty snd . B.uncons) <$> B.readInt s) <$> B.getLine"
    , ""
    , "readContents :: Int -> IO [[Int]]"
    , "readContents = flip replicateM readLine"
    , ""
    ]
