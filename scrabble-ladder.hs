import Data.List

data Word = Word
  { l1 :: Char
  , l2 :: Char
  , l3 :: Char
  , l4 :: Char
  } deriving (Eq, Show, Ord)
-- FIXME: get Ord working properly

type Dictionary = [String]
dictionary :: Dictionary
dictionary = [[]]

type Dictionary' = [Word]
dictionary' :: Dictionary'
dictionary' = []

reverseDictionary :: Dictionary
reverseDictionary = [[]]

type Ladder = [Word]

findLadder :: Int -> Either Ladder Ladder
findLadder 0 = Right []
findLadder rung =
  case findLadder (rung - 1) of
    Left doneLadder   -> Left doneLadder -- admit defeat
    Right subLadder   -> addRung Nothing subLadder
-- Calculate a ladder of height 'rung', the bottom word should be > lastWord if specified

addRung :: Maybe Word -> Ladder -> Either Ladder Ladder
addRung lastWord ladder =
  if length ladder < 3 then
    addEarlyRung lastWord ladder
  else
    addMidRung lastWord ladder

addEarlyRung :: Maybe Word -> Ladder -> Either Ladder Ladder
addEarlyRung lastWord ladder = Left [] -- TODO: stub

addMidRung :: Maybe Word -> Ladder -> Either Ladder Ladder
addMidRung lastWord lad =
  -- Can we find a word that fits this rung?
  case findWord (take 3 lad) lastWord of
    Just word -> Right (word:lad) -- Yes we can!
    Nothing -> -- Nope. revise the ladder we've been given.
      case addRung (Just (head lad)) (tail lad) of
        Left lad'   -> Left lad' -- Admit defeat
        Right lad'  -> addRung Nothing lad'


findWord :: Ladder -> Maybe Word -> Maybe Word
findWord (a:b:c:[]) lastWord =
  let { l1s = getLettersRev [(l4 c), (l3 b), (l2 a)]     -- possible first letters for new word
      ; l2s = getLettersRev [(l4 b), (l3 a)]
      ; l3s = getLettersRev [(l4 a)]
      } in
    actuallyFindWord l1s l2s l3s lastWord


getLettersRev :: [Char] -> [Char]
getLettersRev letters =
  map head $ foldr lookupSuffixes reverseDictionary letters

lookupSuffixes :: Char -> Dictionary -> Dictionary
lookupSuffixes letter revDict =
  map tail $ filter (\rw -> head rw == letter) revDict
  -- TODO: stripPrefix will do this?

actuallyFindWord :: [Char] -> [Char] -> [Char] -> Maybe Word -> Maybe Word
actuallyFindWord l1s l2s l3s Nothing =
  find (\x -> (elem (l1 x) l1s) && (elem (l2 x) l2s) && (elem (l3 x) l3s)) dictionary'
actuallyFindWord l1s l2s l3s (Just lastWord) =
  find (\x -> (x > lastWord) && (elem (l1 x) l1s) 
    && (elem (l2 x) l2s) && (elem (l3 x) l3s)) dictionary'

