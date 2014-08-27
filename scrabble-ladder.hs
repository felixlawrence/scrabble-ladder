import Data.Char
import Data.Functor
import Data.Foldable (foldrM)
import Data.List
import Data.Tree
import Data.Maybe
import Control.Monad

data Word = Word
  { l1 :: Char
  , l2 :: Char
  , l3 :: Char
  , l4 :: Char
  } deriving (Eq, Ord)

instance Show Word where
  show w = map (\x -> toUpper $ x w) [l1, l2, l3, l4]

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
  let { l3c =                getSuffixCond revDicts 4 l3 [(l4 (l !! 0))]
      ; l2c = case length l of
                1         -> getSuffixCond revDicts 3 l2 [(l3 (l !! 0))]
                otherwise -> getSuffixCond revDicts 4 l2 [(l4 (l !! 1)), (l3 (l !! 0))]
      ; l1c = case length l of
                1         -> getSuffixCond revDicts 2 l1 [(l2 (l !! 0))]
                2         -> getSuffixCond revDicts 4 l2 [(l4 (l !! 1)), (l3 (l !! 0))]
                otherwise -> getSuffixCond revDicts 4 l1 [(l4 (l !! 2)), (l3 (l !! 1)), (l2 (l !! 0))]
      ; suffixConds = (:) (tilesAllow tiles) <$> sequence [l1c, l2c, l3c] :: Maybe [(Word -> Bool)]
      ; viableWords = foldr filter word4List <$> suffixConds :: Maybe [Word]
      }
  in listToMaybe =<< viableWords

getSuffixCond :: ReverseDictionaries -> Int -> (Word -> Char) -> String -> Maybe (Word -> Bool)
getSuffixCond revDicts wordLen lN revSuffix =
  let revDict = fromJust $ lookup wordLen revDicts
  in (\chars word -> elem (lN word) chars) <$> getLettersRev revDict revSuffix
  --case getLettersRev revSuffix of
  --  Nothing -> Nothing
  --  Just lets -> Just ((\l w -> elem (lN w) l) lets)

getLettersRev :: ForestDictionary -> [Char] -> Maybe [Char]
-- Given the first n letters of a word, find the possible n+1th letters
getLettersRev revDict letters =
  map rootLabel <$> foldM lookupSuffixes revDict letters

lookupSuffixes :: ForestDictionary -> Char -> Maybe ForestDictionary
-- Search the forest for char and return the relevant subforest
lookupSuffixes dict char =
  subForest <$> find ((char ==) . rootLabel) dict

tilesAllow :: Tiles -> Word -> Bool
tilesAllow tiles word =
  [] == ((show word) \\ tiles)

tt :: Tiles -> Ladder -> Tiles
tt tiles ladder =
  (\\) tiles $ concat $ map show ladder

getWord4List :: [String] -> [Word]
getWord4List sortedWords =
  [Word a b c d | (a:b:c:d:[]) <- sortedWords]

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
  putStrLn "Can you make do with a X rung ladder?"
  putStr $ prettyShowLadder ladder
  putStrLn "\nLeftover tiles:"
  putStrLn $ tt allTiles ladder
printLadderM allTiles (Right ladder) = do
  putStr $ prettyShowLadder ladder
  putStrLn "\nLeftover tiles:"
  putStrLn $ tt allTiles ladder


prettyShowLadder :: Ladder -> String
prettyShowLadder ladder = do
  unlines $ reverse [take 80 $ drop (mod (-i) 25) $ cycle (show w ++ replicate 21 ' ') 
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
-- Get ladder top working
