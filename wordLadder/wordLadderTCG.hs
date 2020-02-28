import Control.Monad
import System.Random
import Data.List
-- import Data.List.Utils (replace)

updateString :: String -> Int -> Char -> String
updateString str index newChar = do
    let spTuple = splitAt index str
    fst spTuple ++ newChar : tail (snd spTuple)

generateRandomStrings :: String -> [Int] -> IO String
generateRandomStrings initialString [] = pure ""
generateRandomStrings initialString (i:rest) = do
    g <- getStdGen
    let (newChar, g') = randomR ('a', 'z') g
    whichIndexToChange <- randomRIO (0, length initialString - 1)
    let toBeChanged = initialString !! whichIndexToChange
    if newChar == toBeChanged
        then generateRandomStrings initialString (i:rest)
        else do
            newStr <- generateRandomStrings (updateString initialString whichIndexToChange newChar) rest
            return (initialString ++ "\n" ++ newStr)

main :: IO ()
main = do
    let initialString = "ishan"
    val <- generateRandomStrings initialString [1..15]
    let testCase = lines val
    print testCase
    return ()
