module PermutationMap 
 (
    PermutationMap,
    createPermutationMap,
    findWithDefault
 )
 where

import qualified Data.AssocMap as M 
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

type PermutationMap = M.AssocMap String [String]

empty :: PermutationMap
empty = M.empty

member :: String -> PermutationMap -> Bool
member key = M.member (L.sort key)

alter :: (Maybe [String] -> Maybe [String]) -> String -> PermutationMap -> PermutationMap
alter f key = M.alter f (L.sort key)

delete :: String -> PermutationMap -> PermutationMap
delete key = M.delete (L.sort key)

insert :: String -> [String] -> PermutationMap -> PermutationMap
insert key = M.insert (L.sort key)

lookup :: String -> PermutationMap -> Maybe [String]
lookup key = M.lookup (L.sort key)

findWithDefault :: [String] -> String -> PermutationMap -> [String]
findWithDefault defaultValue key map = 
    fromMaybe defaultValue (PermutationMap.lookup key map)

createPermutationMap :: [String] -> PermutationMap
createPermutationMap xs = go empty xs
    where
        go :: PermutationMap -> [String] -> PermutationMap
        go permMap [] = permMap
        go permMap (x:xs) = go (insertPermutation x permMap) xs
        insertPermutation :: String -> PermutationMap -> PermutationMap
        insertPermutation word = alter (insertList word) word
        insertList :: String -> Maybe [String] -> Maybe [String]
        insertList word Nothing = Just [word]
        insertList word (Just words) = Just (word: words)
