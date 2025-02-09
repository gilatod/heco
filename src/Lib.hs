module Lib (
    FsError(..),
    FileSystem(..),
    readFile,
    writeFile,
    runFileSystemPure
) where

import Prelude hiding (lookup, writeFile, readFile)
import Data.Map (Map, insert, lookup)

import Effectful (Effect, type (:>), Eff)
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.State.Static.Local (evalState, gets, modify)
import Effectful.TH (makeEffect)

newtype FsError = FsError String
    deriving (Show, Eq)

data FileSystem :: Effect where
    ReadFile :: FilePath -> FileSystem m String
    WriteFile :: FilePath -> String -> FileSystem m ()
makeEffect ''FileSystem

runFileSystemPure ::
    (Error FsError :> es)
    => Map FilePath String
    -> Eff (FileSystem : es) a
    -> Eff es a
runFileSystemPure file_contents = reinterpret (evalState file_contents) $ \_ -> \case
    ReadFile path -> gets (lookup path) >>= \case
        Just content -> pure content
        Nothing -> throwError . FsError $ "File not found: " ++ show path
    WriteFile path content -> modify $ insert path content