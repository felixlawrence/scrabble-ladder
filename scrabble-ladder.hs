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
revDict = [Node 'a' []] :: ForestDictionary

word4list :: [Word]
word4list = []

type Ladder = [Word]

findLadder :: Int -> Either Ladder Ladder
-- Calculate a ladder of height 'rung'
findLadder 0 = Right []
findLadder rung = addRung Nothing =<< findLadder (rung - 1)

addRung :: Maybe Word -> Ladder -> Either Ladder Ladder
addRung lastWord ladder
  -- For now, don't do anything clever with rows 2 and 3
  | length ladder < 3 = addFirstRung lastWord ladder
  | otherwise = addMidRung lastWord ladder

addFirstRung :: Maybe Word -> Ladder -> Either Ladder Ladder
addFirstRung Nothing ladder = Right ((head word4list):ladder)
addFirstRung (Just lastWord) ladder =
  case find (lastWord <) word4list of
    Nothing   -> Left []
    Just word -> Right (word:ladder)

addMidRung :: Maybe Word -> Ladder -> Either Ladder Ladder
addMidRung lastWord lad =
  -- Can we find a word that fits this rung?
  case findWord word4list (take 3 lad) lastWord of
    Just word -> Right (word:lad) -- Yes we can!
    Nothing -> -- Nope. revise the ladder we've been given.
      (addRung Nothing) =<< addRung (Just (head lad)) (tail lad)
      --case addRung (Just (head lad)) (tail lad) of
      --  Left lad'   -> Left lad' -- Admit defeat
      --  Right lad'  -> addRung Nothing lad'

findWord :: [Word] -> Ladder -> Maybe Word -> Maybe Word
-- TODO: somehow save viableWords instead of passing around lastWord?
findWord wordList lad (Just lastWord) =
  -- dropWhile rather than filter - assume  that wordList is sorted alphabetically
  findWord (dropWhile (lastWord <) wordList) lad Nothing
findWord wordList (a:b:c:[]) Nothing =
  let { l1c = getSuffixCond l1 [(l4 c), (l3 b), (l2 a)]
      ; l2c = getSuffixCond l2 [(l4 b), (l3 a)]
      ; l3c = getSuffixCond l3 [(l4 a)]
      ; suffixConds = sequence [l1c, l2c, l3c] :: Maybe [(Word -> Bool)]
      ; viableWords = foldr filter wordList <$> suffixConds :: Maybe [Word]
      }
  in listToMaybe =<< viableWords

getSuffixCond :: (Word -> Char) -> String -> Maybe (Word -> Bool)
getSuffixCond lN revSuffix =
  (\chars word -> elem (lN word) chars) <$> getLettersRev revSuffix
  --case getLettersRev revSuffix of
  --  Nothing -> Nothing
  --  Just lets -> Just ((\l w -> elem (lN w) l) lets)

getLettersRev :: [Char] -> Maybe [Char]
-- Given the first n letters of a word, find the possible n+1th letters
getLettersRev letters =
  map rootLabel <$> foldM lookupSuffixes revDict letters

lookupSuffixes :: ForestDictionary -> Char -> Maybe ForestDictionary
-- Search the forest for char and return the relevant subforest
lookupSuffixes dict char =
  subForest <$> find ((char ==) . rootLabel) dict

