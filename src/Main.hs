{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Exception
import Options
import System.Directory
import System.IO
import System.Process (callProcess)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data EmptyOption = EmptyOption
    deriving (Show)

instance Options EmptyOption where
    defineOptions = pure EmptyOption

main :: IO ()
main = runSubcommand
    [ subcommand "new" createProject
    , subcommand "save" saveMain
    , subcommand "load" loadMain
    ]

createProject :: EmptyOption -> EmptyOption -> [String] -> IO ()
createProject _ _ [dist] = bracketOnError createTemplate removeTemplate initializeStack
    where
        createTemplate = openTempFile "." "contest.hsfiles"

        removeTemplate (file, h) = hClose h
                                >> removeFile file

        initializeStack (file, h) = T.hPutStr h contents
                                >> hClose h
                                >> callProcess "stack" ["new", dist, file]
                                >> removeFile file

        contents = T.unlines $ header : srcContents

createProject _ _ _ = error "a project name must be applied, USAGE: contest-project init <project name>"

saveMain :: EmptyOption -> EmptyOption -> [String] -> IO ()
saveMain _ _ [dist]
    | dist /= "Main" = readFile "src/Main.hs" >>= writeFile path
    | otherwise = saveMain EmptyOption EmptyOption []
    where
        path = concat ["src/", dist, ".hs"]
saveMain _ _ _ = error "a filename must be applied (not Main), USAGE: contest-project save <filename>"

loadMain :: EmptyOption -> EmptyOption -> [String] -> IO ()
loadMain _ _ [dist]
    | dist /= "Main" = doesFileExist path >>= editFiles 
    | otherwise = loadMain EmptyOption EmptyOption []
    where 
        editFiles res 
            | res = readFile path >>= writeFile "src/Main.hs"
            | otherwise = T.writeFile path defaultContents >> T.writeFile "src/Main.hs" defaultContents
        path = concat ["src/", dist, ".hs"]
        defaultContents = T.unlines srcContents

loadMain _ _ _ = error "a filename must be applied (not Main), USAGE: contest-project load <filename>"

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
    , "  build-depends:       base >= 4.7 && < 5, bytestring"
    , "  main-is: Main.hs"
    , ""
    , "{-# START_FILE src/Main.hs #-}"
    ]

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
