import Data.Functor
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
  case findWord' (take 3 lad) lastWord of
    Just word -> Right (word:lad) -- Yes we can!
    Nothing -> -- Nope. revise the ladder we've been given.
      (addRung Nothing) =<< addRung (Just (head lad)) (tail lad)
      --case addRung (Just (head lad)) (tail lad) of
      --  Left lad'   -> Left lad' -- Admit defeat
      --  Right lad'  -> addRung Nothing lad'

findWord :: Ladder -> Maybe Word -> Maybe Word
findWord (a:b:c:[]) lastWord =
  let { l1s = getLettersRev [(l4 c), (l3 b), (l2 a)]     -- possible first letters for new word
      ; l2s = getLettersRev [(l4 b), (l3 a)]
      ; l3s = getLettersRev [(l4 a)]
      } in
    actuallyFindWord l1s l2s l3s lastWord

getLettersRev :: [Char] -> [Char] -- TODO: Go to Maybe [Char]?
getLettersRev letters =
  map rootLabel $ foldl lookupSuffixes revDict letters

lookupSuffixes :: ForestDictionary -> Char -> ForestDictionary
lookupSuffixes dict letter =
  -- Search the forest for letter and return the relevant subforest
  --subForest <$> find ((letter ==) . rootLabel) dict
  case find ((letter ==) . rootLabel) dict of
    Just subDict  -> subForest subDict
    Nothing       -> []

findWord' :: Ladder -> Maybe Word -> Maybe Word
findWord' (a:b:c:[]) lastWord =
  let { l1c = [(l4 c), (l3 b), (l2 a)]
      ; l2c = [(l4 b), (l3 a)]
      ; l3c = [(l4 a)]
      -- possible first letters for new word
      ; letters = mapM getLettersRev' [l1c, l2c, l3c] :: Maybe [[Char]]
      ; accessors = Just [l1, l2, l3] :: Maybe [Word -> Char]
      ; conds = liftM2 zip accessors letters :: Maybe [((Word -> Char), [Char])]
      }
  in actuallyFindWord' lastWord =<< conds

getLettersRev' :: [Char] -> Maybe [Char]
-- Given the first n letters of a word, find the possible n+1th letters
getLettersRev' letters =
  map rootLabel <$> foldM lookupSuffixes' revDict letters

lookupSuffixes' :: ForestDictionary -> Char -> Maybe ForestDictionary
-- Search the forest for char and return the relevant subforest
lookupSuffixes' dict char =
  subForest <$> find ((char ==) . rootLabel) dict

actuallyFindWord'' :: [Word] -> Maybe Word -> [Word -> Bool] -> Maybe Word
actuallyFindWord'' word4list (Just lastWord) conds =
  actuallyFindWord'' (filter (lastWord <) word4list) Nothing conds
actuallyFindWord'' word4list Nothing conds =
  listToMaybe $ foldr filter word4list conds


actuallyFindWord' :: Maybe Word -> [((Word -> Char), [Char])] -> Maybe Word
actuallyFindWord' lastWordM conds =
  let { isAfterLW = case lastWordM of
          Nothing       -> const True
          Just lastWord -> (lastWord <)
      ; searchCrit = (\x -> foldl (&&) (isAfterLW x) [elem (a x) l | (a, l) <- conds]) :: Word -> Bool
      }
  in find searchCrit word4list


actuallyFindWord :: [Char] -> [Char] -> [Char] -> Maybe Word -> Maybe Word
-- TODO: speed this up by indexing or other?
-- TODO: check if (e.g.) l3s==[] then does it bother checking everything?
actuallyFindWord l1s l2s l3s lastWordM =
  if l1s == [] || l2s == [] || l3s == [] -- Is haskell smart enough to take the shortcut?
  then Nothing
  else
    let searchLetCrit = (\x -> elem (l1 x) l1s && elem (l2 x) l2s && elem (l3 x) l3s)
        searchCrit =
          case lastWordM of 
            Nothing       -> searchLetCrit
            Just lastWord -> liftM2 (&&) searchLetCrit (lastWord <)
    in
      find searchCrit word4list
