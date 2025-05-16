module Main where

import Primes
import qualified System.Random as R
import Options.Applicative
import Numeric (showHex)


data Command
    = PrimeCmd PrimeOptions
    | StringCmd StringOptions


data PrimeOptions = PrimeOptions
    { seed :: Int,
      hex :: Bool,
      kbits :: Int }

primeCmdParser :: Parser Command
primeCmdParser = PrimeCmd <$> (PrimeOptions
    <$> option auto
        (  long "seed"
        <> metavar "INT"
        <> value 0
        <> showDefault
        <> help "rng seed value" )
    <*> switch
        (  long "hex"
        <> short 'h'
        <> help "output in hex format" )
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
        PrimeCmd opts -> 
            let g = R.mkStdGen (seed opts) in 
                putStrLn $ "prime generator: " ++ (let p = randomkbitsPrime g (kbits opts) in
                    if hex opts then 
                        show $ fmap (`showHex` "") p
                    else show p)
        StringCmd opts -> putStrLn "string generator"