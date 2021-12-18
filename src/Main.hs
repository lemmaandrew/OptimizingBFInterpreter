{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Main module for running the interpreter
module Main where

import Control.Monad      ( void )
import Data.Interpreter   ( runProgram )
import Data.List          ( isSuffixOf )
import Data.Machine       ( initialize )
import Data.Proxy         ( Proxy (Proxy) )
import System.Environment ( getArgs )
import System.IO
    ( BufferMode (NoBuffering), hSetBuffering, stdin, stdout )
import Text.Parser        ( processProgram )

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filepath]
            | ".bf" `isSuffixOf` filepath || ".b" `isSuffixOf` filepath ->
                run filepath
        _ -> print usage
  where
    run fp = do
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        program <- processProgram <$> readFile fp
        case program of
            Left err -> print err
            Right pgm -> do
                machine <- initialize (Proxy @64)
                void (runProgram machine pgm)
    usage =
        "Usage: stack run [filepath]\nfilepath : a brainfuck file (ending in .bf or .b)"
