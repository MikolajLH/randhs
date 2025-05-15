module Main where

import Primes
import qualified System.Random as R
import Options.Applicative

data Options = Options
    { name :: String,
      seed :: Int,
      number :: Int }

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption
        (  long "Name"
        <> metavar "NAME"
        <> help "Your name")
    <*> option auto
        (  long "seed"
        <> help "rng seed value"
        <> showDefault
        <> value 1
        <> metavar "INT" )
    <*> option auto
        (  long "num"
        <> help "number to check"
        <> showDefault
        <> value 1
        <> metavar "INT" )

main :: IO ()
main = do
    opts <- execParser optsParser
    putStrLn $ "Hello, " ++ name opts ++ "!, seed is " ++ show (seed opts)
    let g = R.mkStdGen (seed opts)
    putStrLn $ "Number " ++ show (number opts) ++ " is " ++ show (millerRabin g 10 (number opts))
    where
        optsParser = info (optionsParser <**> helper) ( fullDesc <> progDesc "A simple CLI to greet the user" <> header "greet-cli - A basic Haskell CLI example")