{-# LANGUAGE LambdaCase #-}

-- |
-- Module for parsing brainfuck code
module Text.Parser
    ( processProgram
    ) where

import Control.Monad   ( guard )
import Data.Maybe      ( mapMaybe )
import Data.Void       ( Void )
import Text.Megaparsec
    ( MonadParsec (eof, try, label)
    , ParseErrorBundle
    , Parsec
    , between
    , choice
    , many
    , parse
    , single
    , some
    , (<|>)
    )
import Util            ( BFToken (..), Command (..), Program )

-- | Parser for a stream of @BFToken@
type Parser = Parsec Void [BFToken]

-- | Converts a character to a @BFToken@
toBFToken :: Char -> Maybe BFToken
toBFToken =
    \case
        '+' -> Just Plus
        '-' -> Just Minus
        '.' -> Just Period
        ',' -> Just Comma
        '[' -> Just LBracket
        ']' -> Just RBracket
        '<' -> Just LAngleBracket
        '>' -> Just RAngleBracket
        _   -> Nothing

-- | Converts a string to a list of @BFToken@s, removing all invalid characters
-- Equivalent to removing comments from the code
stringToBFTokens :: String -> [BFToken]
stringToBFTokens = mapMaybe toBFToken

-- | Parses a single @Command@ from a stream of @BFToken@s
parseCommand :: Parser Command
parseCommand = choice [add, shift, input, output, try copy, loop]
  where
    plus = Add . fromIntegral . length <$> some (single Plus)

    minus = Add . negate . fromIntegral . length <$> some (single Minus)

    add = do
        additions <- some (plus <|> minus)
        return . Add $ sum [x | Add x <- additions]

    lShift, rShift :: Parser Command
    lShift = Shift . negate . length <$> some (single LAngleBracket)

    rShift = Shift . length <$> some (single RAngleBracket)

    shift = do
        shifts <- some (label "does right" rShift <|> label "does left" lShift)
        return . Shift $ sum [x | Shift x <- shifts]

    input = Input <$ single Comma

    output = Output <$ single Period

    loop = Loop <$> between (single LBracket) (single RBracket) parseCommands

    copy = do
        single LBracket
        body <- (single Minus *> copyBody) <|> (copyBody <* single Minus)
        single RBracket
        return body

    copyBody = do
        shiftAdds <-
            some $ try $ do
                Shift s <- shift
                guard (s > 0)
                Add n <- add
                return (s, n)
        Shift n <- shift
        guard (n < 0)
        guard (abs n == sum (map fst shiftAdds))
        return $ Copy shiftAdds

-- | Parses many @Command@s from a stream of @BFToken@s
parseCommands :: Parser [Command]
parseCommands = many parseCommand

-- | Parses an entire stream of @BFToken@s into a @Program@, failing if any part of the parsing fails
parseProgram :: Parser Program
parseProgram = parseCommands <* eof

-- | Performs program optimizations that could not be done in the parsing stage
optimizeProgram :: Program -> Program
optimizeProgram originalProgram =
    let newProgram = optimize originalProgram
    in if newProgram == originalProgram
            then newProgram
            else optimizeProgram newProgram
  where
    optimize [] = []
    optimize (cmd:cmds) =
        case cmd of
            Loop [] -> optimize cmds
            Loop [Add x]
                | x == 1 || x == 255 -> Clear : optimize cmds
            Loop xs -> Loop (optimize xs) : optimize cmds
            Add 0 -> optimize cmds
            Shift 0 -> optimize cmds
            _ -> cmd : optimize cmds

-- | Parses a @String@ into a @Program@ and optimizes the result
processProgram :: String -> Either (ParseErrorBundle [BFToken] Void) Program
processProgram = fmap optimizeProgram . parse parseProgram "" . stringToBFTokens
