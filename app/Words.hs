module Words where

import qualified System.Random as R

-- | returns random uniform element of given list wrapped in Just or Nothing if given list is empty
randomListElement :: R.RandomGen g => g -> [a] -> Maybe (a, g)
randomListElement _ [] = Nothing
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
randomWord _ [] _ = Nothing
randomWord g alphabet wordLength 
    | wordLength <= 0 = Nothing
    | otherwise = 
        -- All the shenanigans with list of Maybies (elemGenPairMaybies) are purely for fun,
        -- since the alphabet is not empty in this part of function which makes randomListElement always return Just (a, g)
        -- so it would be completly safe to call fromJust on the result of the randomListElement
        let 
            elemGenPairMaybies = -- :: [Maybe ([a], g)]
                take wordLength $
                iterate 
                    (>>= \(_, g') -> randomListElement g' alphabet) $ -- in order to propagate the generator g, monadic nature of maybe is used
                    randomListElement g alphabet -- generate first element of word
        in 
            foldl -- foldl with list accumulator will return the word in reverse, but since the word is random it doesn't matter
                (liftA2 $ \(acc, _) (c, g') -> (c:acc, g'))  -- accumulate word and keep track of last RandomGen, liftA2 allow this lambda to operate inside Maybies
                (Just ([], g))
                elemGenPairMaybies
            