-- |
-- Module for useful datatypes
module Util
    ( Word8
    , BFToken(..)
    , Command(..)
    , Program
    ) where

import Data.Word ( Word8 )

-- | Valid Brainfuck tokens
data BFToken
    = Plus
    | Minus
    | Period
    | Comma
    | LBracket
    | RBracket
    | LAngleBracket
    | RAngleBracket
    deriving (Eq, Ord, Show)

-- | A valid operation in Brainfuck
-- Addition and subtraction commands have been simplified to @Add n@ and @Add (255 - n)@ for some @n@
-- Left and right shift commands have been simplified to shifting @-n@ for left shifts and @+n@ for right shifts
data Command
    = Add Word8 -- ^ Addition and subtraction command
    | Shift Int -- ^ Left (-n) and right (+n) shift command
    | Copy [(Int, Word8)]
    -- ^ For some pointer @p@ with value @pval@ and @Copy [(shift, mul)]@, with each @shift_i > 0@,
    -- performs @memory[p + shift_i] += pval * mul_i@ for each @(shift_i, mul_i)@.
    -- Then, performs @Clear@ on @p@
    | Input -- ^ Takes a single character of input
    | Output -- ^ Outputs a single character of data
    | Clear -- ^ Sets the value of the current cell to 0
    | Loop [Command] -- ^ Loops commands until current cell value = 0
    deriving (Eq, Show)

-- | A complete list of @Command@s which constitutes a program
type Program = [Command]
