module Main where

import qualified System.Random as R
import Options.Applicative
import Numeric (showHex)
import Data.Maybe
import System.Entropy (getEntropy)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (unpack)
import Data.Word (Word8)
import Data.Bits (shiftL, (.|.))

import Primes
import Words



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
        <> value (-1)
        )
    <*> option auto
        (  long "count"
        <> short 'c' 
        <> value 1
        )


primeCmdParser :: Parser Command
primeCmdParser = PrimeCmd <$> (PrimeOptions
    <$> option auto
        (  long "kbits"
        <> short 'k' )
    <*> switch
        (  long "hex"
        <> short 'h' ))


stringCmdParser :: Parser Command
stringCmdParser = StringCmd <$> (StringOptions
    <$> strArgument
        (  metavar "STRING"
        <> help "Alphabet")
    <*> option auto
        (  long "length"
        <> short 'n' ))


commandParser :: Parser Command
commandParser = hsubparser
    (  command "prime"  (info primeCmdParser  (progDesc "generate random prime number"))
    <> command "string" (info stringCmdParser (progDesc "generate random string"))
    )


optionsParser :: Parser Options
optionsParser = Options
    <$> globalOptionsParser
    <*> commandParser


byteStringToInt :: ByteString -> Int
byteStringToInt bs = foldl (\acc w -> acc `shiftL` 8 .|. fromIntegral w) 0 (BS.unpack bs)


main :: IO ()
main = do
    opts <- execParser (info optionsParser fullDesc)
    let seed' = seed $ globalOpts opts
    let count' = count $ globalOpts opts
    entropy <- getEntropy 4
    let g = R.mkStdGen $ if seed' < 0 then byteStringToInt entropy else seed'
    case cmd opts of 
        PrimeCmd opts -> 
            let kbits' = kbits opts
                hex' = hex opts
                pairFlip = \(a,b) -> (b, a)
                prime g' = let 
                    p = randomkbitsPrime g' kbits'
                    p' = pairFlip <$> p
                    ps = if hex' then fmap (fmap (`showHex` "")) p' else (fmap show) <$> p'
                    in fromMaybe (g', "") ps
                primes = fmap snd $ tail $ take (count' + 1) $ iterate (\(g', _) -> prime g') (g, "")
            in putStr (unlines primes)
        StringCmd opts ->
            let alphabet' = alphabet opts
                wordLength' = wordLength opts
                wordGenPairMaybies = 
                    take count' $ 
                    iterate (>>= \(_, g') -> randomWord g' alphabet' wordLength') $
                    randomWord g alphabet' wordLength'
                wordMaybies = map (fmap fst) wordGenPairMaybies
                words = map (fromMaybe "") wordMaybies
            in putStr (unlines words)
