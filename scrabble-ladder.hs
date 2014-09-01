import Data.Char
import Data.Functor
import Data.Foldable (foldrM)
import Data.List
import Data.Tree
import Data.Maybe
import Control.Monad


type Word = String
type ForestDictionary = Forest Char
type Word4List = [Word]
type ReverseDictionaries = [(Int, ForestDictionary)]
type Ladder = [Word]
type Tiles = [Char]

findLadder :: (Word4List, ReverseDictionaries, Tiles) -> Int -> Either Ladder Ladder
-- Calculate a ladder of height 'rung'
findLadder context 0 = Right []
findLadder context rung = 
  addRung context Nothing =<< findLadder context (rung - 1)

addRung :: (Word4List, ReverseDictionaries, Tiles) -> Maybe Word -> Ladder -> Either Ladder Ladder
addRung context@(word4List, revDicts, allTiles) lastWord lad =
  -- Can we find a word that fits this rung?
  case findWord word4List revDicts (tt allTiles lad) (take 3 lad) lastWord of
    Just word -> Right (word:lad) -- Yes we can!
    Nothing -> -- Nope. revise the ladder we've been given.
      if lad == []
      then Left []
      else
        case addRung context (Just (head lad)) (tail lad) of
          Left lad'   -> Left lad -- Admit defeat
          Right lad'  -> addRung context Nothing lad'

findWord :: Word4List -> ReverseDictionaries -> Tiles -> Ladder -> Maybe Word -> Maybe Word
-- TODO: somehow save viableWords instead of passing around lastWord?
findWord word4List revDicts tiles lad (Just lastWord) =
  findWord (filter (lastWord <) word4List) revDicts tiles lad Nothing
findWord word4list revDicts tiles ([]) Nothing = 
  listToMaybe $ filter (tilesAllow tiles) word4list
findWord word4List revDicts tiles l Nothing =
  let { getDict = (\wordLen -> fromJust $ lookup wordLen revDicts) :: Int -> ForestDictionary
      ; l2c =                getSuffixCond (getDict 4) 2 (getSuffix 2 l)
      ; l1c = case length l of
                1         -> getSuffixCond (getDict 3) 1 (getSuffix 1 l)
                otherwise -> getSuffixCond (getDict 4) 1 (getSuffix 1 l)
      ; l0c = case length l of
                1         -> getSuffixCond (getDict 2) 0 (getSuffix 0 l)
                2         -> getSuffixCond (getDict 3) 0 (getSuffix 0 l)
                otherwise -> getSuffixCond (getDict 4) 0 (getSuffix 0 l)
      ; suffixConds = (:) (tilesAllow tiles) <$> sequence [l0c, l1c, l2c] :: Maybe [(Word -> Bool)]
      ; viableWords = foldr filter word4List <$> suffixConds :: Maybe [Word]
      }
  in listToMaybe =<< viableWords

getSuffix :: Int -> Ladder -> String
getSuffix letterNumber ladder =
  -- TODO: simplify with foldr??
  foldl (\s (i, w) -> (w !! i):s) [] $ zip [3,2,1] $ reverse $ take (3 - letterNumber) ladder

getSuffixCond :: ForestDictionary -> Int -> String -> Maybe (Word -> Bool)
getSuffixCond revDict letterNumber revSuffix =
  (\chars word -> elem (word !! letterNumber) chars) <$> getLettersRev revDict revSuffix

getLettersRev :: ForestDictionary -> [Char] -> Maybe [Char]
-- Given the last n letters of a word, find the possible n+1th letters
getLettersRev revDict letters =
  map rootLabel <$> foldM lookupSuffixes revDict (reverse letters)

lookupSuffixes :: ForestDictionary -> Char -> Maybe ForestDictionary
-- Search the forest for char and return the relevant subforest
lookupSuffixes dict char =
  subForest <$> find ((char ==) . rootLabel) dict

tilesAllow :: Tiles -> Word -> Bool
tilesAllow tiles word =
  [] == (word \\ tiles)

tt :: Tiles -> Ladder -> Tiles
tt tiles ladder =
  (\\) tiles $ concat ladder

getWord4List :: [String] -> [Word]
getWord4List =
  filter ((4==) . length)

addTree :: ForestDictionary -> String -> ForestDictionary
addTree dict (h:t) =
  case partition ((h ==) . rootLabel) dict of
    ([],      othDict) -> ((Node h (addTree []            t)):othDict)
    ((n:[]),  othDict) -> ((Node h (addTree (subForest n) t)):othDict)
    otherwise          -> error "duplicate trees"
addTree dict [] = dict

getReverseDicts :: [String] -> [(Int, ForestDictionary)]
getReverseDicts sortedWords =
  [(i, foldl addTree [] $ map reverse $ filter ((4==) . length)  sortedWords) | i <- [2..4]]

printLadderM :: Tiles -> Either Ladder Ladder -> IO()
printLadderM allTiles (Left ladder) = do
  putStrLn "Didn't get the ladder you wanted"
  putStrLn "Can you make do with a shorter ladder?"
  putStr $ prettyShowLadder ladder
  putStrLn "\nLeftover tiles:"
  putStrLn $ tt allTiles ladder
printLadderM allTiles (Right ladder) = do
  putStr $ prettyShowLadder ladder
  putStrLn "\nLeftover tiles:"
  putStrLn $ tt allTiles ladder

prettyShowLadder :: Ladder -> String
prettyShowLadder ladder = do
  unlines $ reverse [take 80 $ drop (mod (-i) 25) $ cycle (w ++ replicate 21 ' ') 
                      | (i, w) <- zip [0..] $ reverse ladder]


main = do
  contents <- getContents
  let sortedWords = sort $ lines $ map toUpper contents
      word4List = getWord4List sortedWords
      revDicts = getReverseDicts sortedWords
      allTiles = "AAAAAAAAABBCCDDDDEEEEEEEEEEEEFFGGGHHIIIIIIIIIJKLLLLMMNNNNNNOOOOOOOOPPQRRRRRRSSSSTTTTTTUUUUVVWWXYYZ"
  printLadderM allTiles $ findLadder (word4List, revDicts, allTiles) 24



-- TODO List
-- switch Word from data to type list??
-- Internally, have the ladder as the filtered word4list, and we just take the head of it at the end
-- Get ladder top working
-- blanks??