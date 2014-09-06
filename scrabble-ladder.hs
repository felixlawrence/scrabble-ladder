import Data.Char
import Data.Functor
import Data.Foldable (foldrM)
import Data.List
import Data.Tree
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Vector (Vector, (!), toList)
import Control.Monad


type Word = Vector Char
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
addRung context@(word4List, revDicts, allTiles) lad =
  -- Can we find some words that fit this rung?
  case findWords word4List revDicts (tt allTiles lad) (take 3 lad) of
    [] -> addRung context =<< swapRung context lad -- Nope. revise the ladder we've been given.
    words -> Right (words:lad)

swapRung :: (Word4List, ReverseDictionaries, Tiles) -> Ladder -> Either Ladder Ladder
swapRung _ [] = Left [] -- Give up
swapRung context lad@(h:t) = 
  case ((drop 1 h):t) of
    ([]:t') -> addRung context =<< swapRung context t' -- Go deeper
    lad'    -> Right lad'

findWords :: Word4List -> ReverseDictionaries -> Tiles -> Ladder -> Word4List
-- TODO: use the fact that lad is only length 3
findWords word4List revDicts tiles l =
  let { getDictN = (\wordLen -> fromJust $ lookup wordLen revDicts) :: Int -> ForestDictionary
      ; getDictL = (\l i -> getDictN (min 4 (1 + i + (length l)))):: Ladder -> Int -> ForestDictionary
      ; getSuffixCondI = (\i -> getSuffixCond (getDictL l i) i (getSuffix i l)) :: Int -> Maybe (Word -> Bool)
      ; suffixConds = sequence $ (Just (tilesAllow tiles)):(map getSuffixCondI [0..2]) :: Maybe[(Word -> Bool)]
      ; viableWords = foldr filter word4List <$> suffixConds :: Maybe [Word]
      } in
    case viableWords of
      Nothing -> []
      Just words -> words

getSuffix :: Int -> Ladder -> String
getSuffix letterNumber ladder =
  foldr (\(i, w) s -> (w ! (i+1)):s) [] $ take (3 - letterNumber) $ zip [letterNumber..] (map head ladder)

getSuffixCond :: ForestDictionary -> Int -> String -> Maybe (Word -> Bool)
getSuffixCond revDict letterNumber revSuffix =
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
  [] == ((toList word) \\ tiles)

tt :: Tiles -> Ladder -> Tiles
tt tiles ladder = (\\) tiles $ concat (map (toList . head) ladder)
  --foldl (\\) tiles $ (map head ladder) -- slower because \\ is slow

getWord4List :: [String] -> [Word]
getWord4List wl = map Vector.fromList $ filter ((4==) . length) wl

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
                      | (i, w) <- zip [0..] $ reverse (map (Vector.toList . head) ladder)]


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