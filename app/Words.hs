module Words where

import qualified System.Random as R
import Data.Maybe (isJust, fromJust)

-- | returns random uniform element of given list wrapped in Just or Nothing if given list is empty
randomListElement :: R.RandomGen g => g -> [a] -> Maybe (a, g)
randomListElement g [] = Nothing
randomListElement g xs = 
    let 
        firstListIndex = 0
        lastListIndex = length xs - 1
        (randomIndex, g') = R.randomR (firstListIndex, lastListIndex) g
        randomElement = xs !! randomIndex
    in
        Just (randomElement, g')




-- | returns random uniform word from given alphabet,
--   i.e. samples random element of given alphabet [a], count times,
--   this function also returns the `RandomGen` that was used
--   The resulting pair is wrapped in Maybe.
--   Function returns `Nothing` if it was provided empty alphabet, or negative wordLength
randomWord :: (R.RandomGen g) => g -> [a] -> Int -> Maybe ([a], g)
randomWord g [] _ = Nothing
randomWord g alphabet wordLength 
    | wordLength <= 0 = Nothing
    | otherwise = 
        let 
            elemGenPairMaybies = -- :: [Maybe ([a], g)]
                take wordLength $
                iterate 
                    (>>= \(elem, g') -> randomListElement g' alphabet) $ -- in order to propagate the generator g, monadic nature of maybe is used
                    randomListElement g alphabet -- generate first element of word
        in 
            -- since we already checked that alphabet is not empty this is just for fun,
            -- in practice we could just call `fromJust <$> elemGenPairMaybies` in the block above,
            -- because it's certain that there won't be any `Nothing` values since alphabet is not empty, so the randomListElement will not fail
            if all isJust elemGenPairMaybies then
                let 
                    elemGenPairs = fromJust <$> elemGenPairMaybies 
                    word = fst <$> elemGenPairs
                    g' = snd $ last elemGenPairs
                in Just (word, g')
            else
                Nothing