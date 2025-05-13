module Main where


import Options.Applicative

data Options = Options
    { name :: String }

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption
        (long "Name"
        <> metavar "NAME"
        <> help "Your name")

main :: IO ()
main = do
    opts <- execParser optsParser
    putStrLn $ "Hello, " ++ name opts ++ "!"
    where
        optsParser = info (optionsParser <**> helper) ( fullDesc <> progDesc "A simple CLI to greet the user" <> header "greet-cli - A basic Haskell CLI example")