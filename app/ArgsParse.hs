module ArgsParse where


import Options.Applicative


data GlobalOptions = GlobalOptions
    { seed :: Int
    , count :: Int
    }


data Command
    = PrimeCmd PrimeOptions
    | StringCmd StringOptions


data PrimeOptions = PrimeOptions
    { kbits :: Int
    , hex :: Bool
    }

data StringOptions = StringOptions
    { alphabet :: String
    , wordLength :: Int
    }

data Options = Options
    { globalOpts :: GlobalOptions
    , cmd :: Command
    }

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = GlobalOptions
    <$> option auto
        (  long "seed"
        <> short 's'
        <> metavar "INT"
        <> help "seed for RNG, if not provided system entropy source will be used"
        <> value (-1)
        )
    <*> option auto
        (  long "count"
        <> short 'c'
        <> metavar "INT"
        <> help "how many samples to generate"
        <> value 1
        )


primeCmdParser :: Parser Command
primeCmdParser = PrimeCmd <$> (PrimeOptions
    <$> option auto
        (  long "kbits"
        <> metavar "INT"
        <> short 'k' )
    <*> switch
        (  long "hex"
        <> short 'h' ))


stringCmdParser :: Parser Command
stringCmdParser = StringCmd <$> (StringOptions
    <$> strArgument
        (  metavar "STRING"
        <> help "alphabet from which the word string will be sampled")
    <*> option auto
        (  long "length"
        <> short 'n' 
        <> metavar "INT"
        <> help "length of sampled string"))


commandParser :: Parser Command
commandParser = hsubparser
    (  command "prime"  (info primeCmdParser  (progDesc "generate random prime number"))
    <> command "string" (info stringCmdParser (progDesc "generate random string"))
    )


optionsParser :: Parser Options
optionsParser = Options
    <$> globalOptionsParser
    <*> commandParser