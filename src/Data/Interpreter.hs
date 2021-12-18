{-# LANGUAGE LambdaCase #-}

-- |
-- This module provides the interpretation functions of the program
-- In other words, it's the program's driver
module Data.Interpreter
    ( runProgram
    ) where

import Control.Monad           ( foldM )
import Control.Monad.Primitive ( RealWorld )
import Data.Char               ( chr, ord )
import Data.Machine
    ( Machine (pointer), adjustPVal, movePointer, pval, setPVal )
import Util                    ( Command (..), Program )

runCommand :: Machine RealWorld -> Command -> IO (Machine RealWorld)
runCommand machine =
    \case
        Add n -> adjustPVal (+ n) machine
        Shift n -> movePointer n machine
        Copy shiftAdds -> do
            let startPointer = pointer machine
            -- copying startPVal * add to each of the copy items
            startPVal <- pval machine
            machine' <- foldM (runShiftAdd startPVal) machine shiftAdds
            let endPointer = pointer machine'
            -- returning to the original pointer and clearing it
            machine'' <- movePointer (startPointer - endPointer) machine'
            setPVal 0 machine''
          where
            runShiftAdd pv mech (shift, add) = do
                mech' <- movePointer shift mech
                adjustPVal (\p -> p + pv * add) mech'
        Input -> do
            val <- fromIntegral . ord <$> getChar
            setPVal val machine
            return machine
        Output -> do
            pChr <- chr . fromIntegral <$> pval machine
            putChar pChr
            return machine
        Clear -> setPVal 0 machine
        loop@(Loop loopCmds) -> do
            pv <- pval machine
            if pv == 0
                then return machine
                else do
                    machine' <- runProgram machine loopCmds
                    runCommand machine' loop

runProgram :: Machine RealWorld -> Program -> IO (Machine RealWorld)
runProgram = foldM runCommand
