module Main where

import qualified System.Random as R
import Options.Applicative
import Numeric (showHex)
import Data.Maybe
import System.Entropy (getEntropy)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bits (shiftL, (.|.))

import Primes
import Words
import ArgsParse




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
        PrimeCmd (PrimeOptions kbits' hex') -> 
            let 
                pairFlip (a,b) = (b, a) 
                -- Pair is a functor with respect to second element
                -- and since we will be dealing with values of type Just (Integer, g)
                -- flipping them to obtain Just (g, Integer) will allow us to transformt the Integer value inside Just,
                -- by using fmap two times - firstly to get inside Maybe, and secondly to tranformt only the second element of Pair.
                prime :: (R.RandomGen g) => g -> (String, g)
                prime g' =
                    let 
                        p = randomkbitsPrime g' kbits' -- :: Just (Integer, g)
                        p' = pairFlip <$> p -- :: Just (g, Integer)
                        toString :: Integer -> String
                        toString = 
                            if hex' then
                                (`showHex` "")
                            else
                                show
                        -- first promote toString with fmap to be able to go inside Maybe values
                        -- and then apply it with <$> to only the second value of Just
                        ps = fmap toString <$> p'
                    in
                        pairFlip $ fromMaybe (g', "") ps
                primes = 
                    fmap fst $ -- :: [Integer]
                    take count' $ -- :: [(Integer, g)]
                    iterate (\(_, g') -> prime g') (prime g)
            in 
                putStr (unlines primes)
        StringCmd (StringOptions alphabet' wordLength') ->
            let 
                wordGenPairMaybies = 
                    take count' $ 
                    iterate (>>= \(_, g') -> randomWord g' alphabet' wordLength') $
                    randomWord g alphabet' wordLength'
                wordMaybies = map (fmap fst) wordGenPairMaybies
                randomWords = map (fromMaybe "") wordMaybies
            in 
                putStr (unlines randomWords)
