module IOSH.Options
  ( Options (..),
    execOptionsParser,
  )
where

import Data.Kind
import Options.Applicative

type Options :: Type
data Options = Options
  { tunProcCmd :: String,
    interactive :: Bool,
    execPath :: FilePath,
    execArgs :: [String]
  }

optsInfo :: Parser Options
optsInfo =
  Options
    <$> strOption
      ( long "tunnel"
          <> short 't'
          <> metavar "COMMAND"
          <> help "Command acting as tunnel to ioshd"
      )
    <*> switch
      ( long "interactive"
          <> short 'i'
          <> help "Whether to run PTY-backed session"
      )
    <*> argument str (metavar "PATH")
    <*> many
      ( argument str (metavar "ARGUMENTS")
      )

parserInfo :: ParserInfo Options
parserInfo =
  info
    (helper <*> optsInfo)
    ( fullDesc
        <> progDesc "Tell daemon to execute PROCESS with ARGUMENTS via COMMAND"
        <> header "Standard IO shell client"
        <> noIntersperse
    )

execOptionsParser :: IO Options
execOptionsParser = execParser parserInfo
