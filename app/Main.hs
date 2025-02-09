{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO as TLIO

import Effectful ( type (:>), Eff, runEff )
import Effectful.Writer.Dynamic ( Writer, execWriterLocal, tell )
import Effectful.Environment
    ( getArgs, getEnvironment, runEnvironment, Environment )
import Effectful.FileSystem ( runFileSystem )

text :: Writer TLB.Builder :> es => TLB.Builder -> Eff es ()
text = tell 

value :: (Show a, Writer TLB.Builder :> es) => a -> Eff es ()
value = tell . TLB.fromString . show

handler
    :: (Environment :> es,
        Writer TLB.Builder :> es)
    => Eff es ()
handler = do
    text "Content-Type: text/plain\n\n"
    text "=== Env ===\n"
    getEnvironment >>= value
    text "\n\n=== Args ===\n"
    getArgs >>= value
    pure ()

main :: IO ()
main = do
    response <- runEff . execWriterLocal @TLB.Builder . runFileSystem . runEnvironment $ handler
    TLIO.putStrLn $ TLB.toLazyText response
