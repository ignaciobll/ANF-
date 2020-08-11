module Data.SAT.Smt where

import           Smtlib.Parsers.CommandsParsers ( parseSource )
import           Smtlib.Syntax.Syntax           ( Source )
import           Text.Parsec                    ( ParseError
                                                , parse
                                                )

parseSmtFile :: String -> IO (Either ParseError Source)
parseSmtFile filename = readFile filename >>= (pure . parseSmt)

parseSmt :: String -> Either ParseError Source
parseSmt = parse parseSource ""
