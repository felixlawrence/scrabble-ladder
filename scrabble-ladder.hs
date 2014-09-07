import Data.Char
import Data.Functor
import Data.Foldable (foldrM)
import Data.List
import Data.Tree
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

data Word = Word Char Char Char Char deriving (Show, Ord, Eq)
(!) :: Word -> Int -> Char
(!) (Word a _ _ _) 0 = a
(!) (Word _ b _ _) 1 = b
(!) (Word _ _ c _) 2 = c
(!) (Word _ _ _ d) 3 = d

type ForestDictionary = Forest Char
type Word4List = [Word]
type ReverseDictionaries = [(Int, ForestDictionary)]
type Ladder = [Word4List]
type Tiles = [Char]


toString :: Word -> String
toString (Word a b c d) = [a,b,c,d]

fromString :: String -> Word
fromString [a,b,c,d] = Word a b c d

findLadder :: (Word4List, ReverseDictionaries, Tiles) -> Int -> Either Ladder Ladder
-- Calculate a ladder of height 'rung'
findLadder (word4List, _, _) 1 = Right [word4List]
findLadder context rung = addRung context =<< findLadder context (rung - 1)

addRung :: (Word4List, ReverseDictionaries, Tiles) -> Ladder -> Either Ladder Ladder
addRung context@(word4List, revDicts, allTiles) lad =
  -- Can we find some words that fit this rung?
  case findWords word4List revDicts (tt allTiles lad) lad of
    [] -> addRung context =<< swapRung context lad -- Nope. revise the ladder we've been given.
    words -> Right (words:lad)

swapRung :: (Word4List, ReverseDictionaries, Tiles) -> Ladder -> Either Ladder Ladder
swapRung _ [] = Left [] -- Give up
swapRung context ((h:[]):t) = either (const (Left ([h]:t))) (addRung context) $ swapRung context t
swapRung _ ((_:h'):t) = Right (h':t)

findWords :: Word4List -> ReverseDictionaries -> Tiles -> Ladder -> Word4List
findWords word4List revDicts tiles l =
  let { getSuffixCondI = (\i -> getSuffixCond (getDict l i revDicts) i (getSuffix i l)) :: Int -> Maybe (Word -> Bool)
      ; suffixConds = sequence $ (Just (tilesAllow tiles)):(map getSuffixCondI [0..2]) :: Maybe[(Word -> Bool)]
      ; viableWords = foldr filter word4List <$> suffixConds :: Maybe [Word]
      } in
    case viableWords of
      Nothing -> []
      Just words -> words

getDict :: Ladder -> Int -> ReverseDictionaries -> ForestDictionary
getDict ladder lettNum revDicts =
  fromJust $ lookup (min 4 (1 + lettNum + (length ladder))) revDicts

getSuffix :: Int -> Ladder -> String
getSuffix letterNumber ladder =
  foldr (\(i, w) s -> (w ! (i+1)):s) [] $ take (3 - letterNumber) $ zip [letterNumber..] (map head ladder)

getSuffixCond :: ForestDictionary -> Int -> String -> Maybe (Word -> Bool)
getSuffixCond revDict letterNumber revSuffix =
  -- TODO: construct the condition in one hit?
  -- TODO: move heavy lifting from findWord let statement into here?
  (\chars word -> Set.member ({-# SCC bangbang #-} word ! letterNumber) chars) <$> getLettersRev revDict revSuffix

getLettersRev :: ForestDictionary -> [Char] -> Maybe (Set Char)
-- Given the last n letters of a word, find the possible n+1th letters
getLettersRev revDict letters =
  Set.fromList <$> map rootLabel <$> foldM lookupSuffixes revDict (reverse letters)

lookupSuffixes :: ForestDictionary -> Char -> Maybe ForestDictionary
-- Search the forest for char and return the relevant subforest
lookupSuffixes dict char = subForest <$> find ((char ==) . rootLabel) dict

tilesAllow :: Tiles -> Word -> Bool
tilesAllow tiles word =
  --all (flip elem $ group tiles) (group $ sort word) -- FIXME: wrong
  [] == ((toString word) \\ tiles)

tt :: Tiles -> Ladder -> Tiles
tt tiles ladder = (\\) tiles $ concat (map (toString . head) ladder)
  --foldl (\\) tiles $ (map head ladder) -- slower because \\ is slow

getWord4List :: [String] -> [Word]
getWord4List wl = map fromString $ filter ((4==) . length) wl

addTree :: ForestDictionary -> String -> ForestDictionary
addTree dict (h:t) =
  case partition ((h ==) . rootLabel) dict of
    ([],      othDict) -> ((Node h (addTree []            t)):othDict)
    ((n:[]),  othDict) -> ((Node h (addTree (subForest n) t)):othDict)
    otherwise          -> error "duplicate trees"
addTree dict [] = dict

getReverseDicts :: [String] -> [(Int, ForestDictionary)]
getReverseDicts sortedWords =
  [(i, foldl addTree [] $ map reverse $ filter ((i==) . length)  sortedWords) | i <- [2..4]]

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
                      | (i, w) <- zip [0..] $ reverse (map (toString . head) ladder)]


main = do
  contents <- getContents
  let sortedWords = sort $ lines $ map toUpper contents
      word4List = getWord4List sortedWords
      revDicts = getReverseDicts sortedWords
      allTiles = "AAAAAAAAABBCCDDDDEEEEEEEEEEEEFFGGGHHIIIIIIIIIJKLLLLMMNNNNNNOOOOOOOOPPQRRRRRRSSSSTTTTTTUUUUVVWWXYYZ"
  printLadderM allTiles $ findLadder (word4List, revDicts, allTiles) 23



-- TODO List
-- Get ladder top working
-- blanks??