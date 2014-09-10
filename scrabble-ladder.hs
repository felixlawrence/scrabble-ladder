import Data.Char
import Data.Functor
import Data.List
import Data.Tree
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

desiredLadderHeight = 24 :: Int

data Word = Word Char Char Char Char deriving (Show, Ord, Eq)
(!) :: Word -> Int -> Char
(!) (Word a _ _ _) 0 = a
(!) (Word _ b _ _) 1 = b
(!) (Word _ _ c _) 2 = c
(!) (Word _ _ _ d) 3 = d
wordLen = 4 :: Int

toString :: Word -> String
toString (Word a b c d) = [a,b,c,d]

fromString :: String -> Word
fromString [a,b,c,d] = Word a b c d

type ForestDictionary = Forest Char
type Word4List = [Word]
type ReverseDictionaries = [(Int, ForestDictionary)]
type Ladder = [Word4List]
type Tiles = [Char]


findLadder :: (Word4List, ReverseDictionaries, Tiles) -> Int -> Either Ladder Ladder
-- Calculate a ladder of height 'rung'
findLadder (word4List, _, _) 1 = Right [word4List]
findLadder context rung = addRung context =<< findLadder context (rung - 1)

addRung :: (Word4List, ReverseDictionaries, Tiles) -> Ladder -> Either Ladder Ladder
addRung context lad =
  -- Can we find some words that fit this rung?
  case findWords context lad of
    [] -> addRung context =<< swapRung context lad -- Nope. revise the ladder we've been given.
    words -> Right (words:lad)

swapRung :: (Word4List, ReverseDictionaries, Tiles) -> Ladder -> Either Ladder Ladder
swapRung _ [] = Left [] -- Give up
swapRung context ((h:[]):t) = either (const (Left ([h]:t))) (addRung context) $ swapRung context t
swapRung _ ((_:h'):t) = Right (h':t)

findWords :: (Word4List, ReverseDictionaries, Tiles) -> Ladder -> Word4List
findWords (word4List, revDicts, allTiles) l =
  let { suffixConds = sequence $ (Just (tilesAllow (tt allTiles l))):(map (getSuffixCond revDicts l) [0..(wordLen-2)]) :: Maybe[(Word -> Bool)]
      ; viableWords = foldr filter word4List <$> suffixConds :: Maybe [Word]
      } in
    case viableWords of
      Nothing -> []
      Just words -> words

tilesAllow :: Tiles -> Word -> Bool
tilesAllow tiles word =
  [] == ((toString word) \\ tiles)

tt :: Tiles -> Ladder -> Tiles
tt tiles = (tiles \\ ) . concat . (map (toString . head)) 
  --foldl (\\) tiles $ (map head ladder) -- slower because \\ is slow

getSuffixCond :: ReverseDictionaries -> Ladder -> Int -> Maybe (Word -> Bool)
getSuffixCond revDicts ladder letterNumber =
  let { revSuffix = getRevSuffix letterNumber ladder
      ; revDict = getDict ladder letterNumber revSuffix revDicts
      } in
    (\chars word -> Set.member ({-# SCC bangbang #-} word ! letterNumber) chars) <$> getLettersRev revDict revSuffix

getDict :: Ladder -> Int -> String -> ReverseDictionaries -> ForestDictionary
getDict ladder lettNum suffix revDicts =
  let { ladTopLen = (desiredLadderHeight - length ladder) + length suffix
      ; ladBotLen = 1 + lettNum + length suffix
      ; thisWordLen = foldl min wordLen [ladTopLen, ladBotLen]
      } in
  fromJust $ lookup thisWordLen revDicts

getRevSuffix :: Int -> Ladder -> String
getRevSuffix lN ladder =
  foldl (\s (i, w) -> (w ! (i+1+lN)):s) "" $ take (wordLen - 1 - lN) $ zip [0..] (map head ladder)

getLettersRev :: ForestDictionary -> [Char] -> Maybe (Set Char)
-- Given the last n letters of a word in reverse order, find the possible n+1th letters
getLettersRev revDict revSuffix =
  Set.fromList <$> map rootLabel <$> foldM lookupSuffixes revDict revSuffix

lookupSuffixes :: ForestDictionary -> Char -> Maybe ForestDictionary
-- Search the forest for char and return the relevant subforest
lookupSuffixes dict char = subForest <$> find ((char ==) . rootLabel) dict

getWord4List :: [String] -> [Word]
getWord4List wl = map fromString $ filter ((wordLen==) . length) wl

addTree :: ForestDictionary -> String -> ForestDictionary
addTree dict (h:t) =
  case partition ((h ==) . rootLabel) dict of
    ([],      othDict) -> ((Node h (addTree []            t)):othDict)
    ((n:[]),  othDict) -> ((Node h (addTree (subForest n) t)):othDict)
    otherwise          -> error "duplicate trees"
addTree dict [] = dict

getReverseDicts :: [String] -> [(Int, ForestDictionary)]
getReverseDicts sortedWords =
  [(i, foldl addTree [] $ map reverse $ filter ((i==) . length)  sortedWords) | i <- [2..wordLen]]

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
  unlines $ reverse [take 80 $ drop (mod (-i) 25) $ cycle (w ++ replicate (25 - wordLen) ' ') 
                      | (i, w) <- zip [0..] $ reverse (map (toString . head) ladder)]


main = do
  contents <- getContents
  let sortedWords = sort $ lines $ map toUpper contents
      word4List = reverse $ getWord4List sortedWords
      revDicts = getReverseDicts sortedWords
      allTiles = "AAAAAAAAABBCCDDDDEEEEEEEEEEEEFFGGGHHIIIIIIIIIJKLLLLMMNNNNNNOOOOOOOOPPQRRRRRRSSSSTTTTTTUUUUVVWWXYYZ"
  printLadderM allTiles $ findLadder (word4List, revDicts, allTiles) desiredLadderHeight
