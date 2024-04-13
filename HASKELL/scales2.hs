import Control.Monad (replicateM)
import Data.List (nub, sort)

type Note = Int
type Scale = [Note]

-- Function to generate all possible scales given the number of notes
generateScales :: Int -> [Scale]
generateScales n = filter isValidScale $ replicateM n [0..11]

-- Function to check if a scale is valid (no repeated notes or enharmonic equivalents)
isValidScale :: Scale -> Bool
isValidScale scale = let sortedScale = sort scale
                     in sortedScale == nub sortedScale

-- Function to convert a note to a Roman numeral
toRomanNumeral :: Note -> String
toRomanNumeral n = case n `mod` 12 of
    0 -> "1"
    1 -> "b2"
    2 -> "2"
    3 -> "b3"
    4 -> "3"
    5 -> "4"
    6 -> "b5"
    7 -> "5"
    8 -> "b6"
    9 -> "6"
    10 -> "b7"
    11 -> "7"
    _ -> error "Invalid note"

-- Function to convert a scale to Roman numeral representation
scaleToRomanNumeral :: Scale -> String
scaleToRomanNumeral = unwords . map toRomanNumeral

-- Function to transpose a scale to all 12 keys using note names
transposeToAllKeys :: Scale -> [String]
transposeToAllKeys scale = do
    rootNote <- [0..11]
    let transposedScale = map (\note -> (note + rootNote) `mod` 12) scale
    return $ noteToNoteName rootNote ++ ". " ++ scaleToNoteNames transposedScale

-- Function to convert a note to note name (C, C#, D, D#, E, F, F#, G, G#, A, A#, B)
noteToNoteName :: Note -> String
noteToNoteName note = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"] !! note

-- Function to convert a scale to note names (C, C#, D, D#, E, F, F#, G, G#, A, A#, B)
scaleToNoteNames :: Scale -> String
scaleToNoteNames = unwords . map noteToNoteName

main :: IO ()
main = do
    putStrLn "Enter the number of notes in your scale:"
    numNotes <- readLn :: IO Int
    let allScales = generateScales numNotes
    let filteredScales = filter (\scale -> head (scaleToRomanNumeral scale) == '1') allScales
    putStrLn "List of all possible scales in Roman numeral system starting with '1':"
    mapM_ (\(idx, scale) -> putStrLn $ show idx ++ ". " ++ scaleToRomanNumeral scale) (zip [1..] filteredScales)
    putStrLn "Enter the index of the scale you want to see in all 12 keys:"
    scaleIndex <- readLn :: IO Int
    let selectedScale = filteredScales !! (scaleIndex - 1)
    putStrLn $ "Selected scale: " ++ scaleToRomanNumeral selectedScale
    putStrLn "Scale represented in all 12 keys:"
    mapM_ putStrLn $ transposeToAllKeys selectedScale
