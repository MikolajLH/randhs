module Main where

import Primes
import qualified System.Random as R
import Options.Applicative


data Command
    = PrimeCmd PrimeOptions
    | StringCmd StringOptions


data PrimeOptions = PrimeOptions
    { seed :: Int,
      kbits :: Int }

primeCmdParser :: Parser Command
primeCmdParser = PrimeCmd <$> (PrimeOptions
    <$> option auto
        (  long "seed"
        <> metavar "INT"
        <> value 0
        <> showDefault
        <> help "rng seed value" )
    <*> option auto
        (  long "kbits"
        <> metavar "INT"
        <> help "how many bits should prime number have"))

data StringOptions = StringOptions
    { sseed :: Int,
      alphabet :: String }


stringCmdParser :: Parser Command
stringCmdParser = StringCmd <$> (StringOptions
    <$> option auto
        (  long "seed"
        <> short 's'
        <> value 0
        <> showDefault
        <> help "rng seed value")
    <*> strArgument
        (  metavar "STRING"
        <> help "Alphabet"))

commandParser :: Parser Command
commandParser = hsubparser
    (  command "prime" (info primeCmdParser (progDesc "generate random prime number"))
    <> command "string" (info stringCmdParser (progDesc "generate random string"))
    )


main :: IO ()
main = do
    cmd <- execParser (info commandParser fullDesc)
    case cmd of 
        PrimeCmd opts -> putStrLn "prime generator :)"
        StringCmd opts -> putStrLn "string generator"