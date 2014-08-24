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
  } deriving (Eq, Show, Ord)

type ForestDictionary = Forest Char
--revDict = [Node 'a' []] :: ForestDictionary

--word4list :: [Word]
--word4list = []

type Ladder = [Word]

findLadder :: [Word] -> ForestDictionary -> Int -> Either Ladder Ladder
-- Calculate a ladder of height 'rung'
findLadder word4list revDict 0 = Right []
findLadder word4list revDict rung = 
  addRung word4list revDict Nothing =<< findLadder word4list revDict (rung - 1)

addRung :: [Word] -> ForestDictionary -> Maybe Word -> Ladder -> Either Ladder Ladder
addRung word4list revDict lastWord ladder
  -- For now, don't do anything clever with rows 2 and 3
  | length ladder < 3 = addFirstRung word4list revDict lastWord ladder
  | otherwise = addMidRung word4list revDict lastWord ladder

addFirstRung :: [Word] -> ForestDictionary -> Maybe Word -> Ladder -> Either Ladder Ladder
addFirstRung word4list revDict Nothing ladder = Right ((head word4list):ladder)
addFirstRung word4list revDict (Just lastWord) ladder =
  case find (lastWord <) word4list of
    Nothing   -> Left []
    Just word -> Right (word:ladder)

addMidRung :: [Word] -> ForestDictionary -> Maybe Word -> Ladder -> Either Ladder Ladder
addMidRung word4list revDict lastWord lad =
  -- Can we find a word that fits this rung?
  case findWord word4list revDict (take 3 lad) lastWord of
    Just word -> Right (word:lad) -- Yes we can!
    Nothing -> -- Nope. revise the ladder we've been given.
      case addRung word4list revDict (Just (head lad)) (tail lad) of
        Left lad'   -> Left lad -- Admit defeat
        Right lad'  -> addRung word4list revDict Nothing lad'

findWord :: [Word] -> ForestDictionary -> Ladder -> Maybe Word -> Maybe Word
-- TODO: somehow save viableWords instead of passing around lastWord?
findWord wordList revDict lad (Just lastWord) =
  -- dropWhile rather than filter - assume  that wordList is sorted alphabetically
  findWord (dropWhile (lastWord <) wordList) revDict lad Nothing
findWord word4List revDict (a:b:c:[]) Nothing =
  let { l1c = getSuffixCond revDict l1 [(l4 c), (l3 b), (l2 a)]
      ; l2c = getSuffixCond revDict l2 [(l4 b), (l3 a)]
      ; l3c = getSuffixCond revDict l3 [(l4 a)]
      ; suffixConds = sequence [l1c, l2c, l3c] :: Maybe [(Word -> Bool)]
      ; viableWords = foldr filter word4List <$> suffixConds :: Maybe [Word]
      }
  in listToMaybe =<< viableWords

getSuffixCond :: ForestDictionary -> (Word -> Char) -> String -> Maybe (Word -> Bool)
getSuffixCond revDict lN revSuffix =
  (\chars word -> elem (lN word) chars) <$> getLettersRev revDict revSuffix
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

getWordList :: String -> [Word]
getWordList fileContents =
  sort [Word a b c d | (a:b:c:d:[]) <- lines fileContents]

addTree :: ForestDictionary -> String -> ForestDictionary
addTree dict (h:t) =
  case partition ((h ==) . rootLabel) dict of
    ([],      othDict) -> ((Node h (addTree []            t)):othDict)
    ((n:[]),  othDict) -> ((Node h (addTree (subForest n) t)):othDict)
    otherwise          -> error "duplicate trees"
addTree dict [] = dict

getReverseDict :: String -> ForestDictionary
getReverseDict fileContents =
  let ruofLetterWords = reverse $ filter ((4==) . length) (lines fileContents) :: [String]
  in foldl addTree [] ruofLetterWords



main = do
  contents <- getContents
  let word4List = getWordList contents
      revDict = getReverseDict contents
  print $ findLadder word4List revDict 20
