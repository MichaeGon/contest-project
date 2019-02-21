{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Exception
import Language.Haskell.TH 
import Options
import System.Directory
import System.IO
import System.Process (callProcess)
import qualified Data.Text as T

import Templates

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

        initializeStack (file, h) = hPutStr h srcBody
                                >> hClose h
                                >> callProcess "stack" ["new", dist, file]
                                >> removeFile file

createProject _ _ _ = error "a project name must be applied, USAGE: contest init <project name>"

saveMain :: EmptyOption -> EmptyOption -> [String] -> IO ()
saveMain _ _ [dist]
    | dist /= "Main" = readFile "src/Main.hs" >>= writeFile path
    | otherwise = saveMain EmptyOption EmptyOption []
    where
        path = concat ["src/", dist, ".hs"]
saveMain _ _ _ = error "a filename must be applied (not Main), USAGE: contest save <filename>"

loadMain :: EmptyOption -> EmptyOption -> [String] -> IO ()
loadMain _ _ [dist]
    | dist /= "Main" = doesFileExist path >>= editFiles 
    | otherwise = loadMain EmptyOption EmptyOption []
    where 
        editFiles res 
            | res = readFile path >>= writeFile "src/Main.hs"
            | otherwise = writeFile path defaultContents >> writeFile "src/Main.hs" defaultContents
        path = concat ["src/", dist, ".hs"]

loadMain _ _ _ = error "a filename must be applied (not Main), USAGE: contest load <filename>"

srcBody :: String
srcBody = $( srcBodyQ )

defaultContents :: String
defaultContents = $( defaultContentsQ )
