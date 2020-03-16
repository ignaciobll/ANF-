module Utils.Options
  (
    parseArgs,
    runInput,
    Input,
  ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Input = FileInput FilePath | StdInput

parseArgs :: IO Input
parseArgs = execParser opts
  where
    opts = info (input <**> helper)
      (fullDesc
       <> progDesc "Currently it only parses a .sat file and pretty print it "
       <> header "satanf - In progress SAT ANF/CNF solver" )

fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input file" )

stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> help "Read from stdin" )

input :: Parser Input
input = fileInput <|> stdInput

runInput :: Input -> IO String
runInput StdInput             = getContents
runInput (FileInput filename) = readFile filename
