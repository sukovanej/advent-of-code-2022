{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode2022.Day7 where

import AdventOfCode2022.Parsing
import Data.Attoparsec.Text
import Data.List (find, sort)
import Data.Maybe (fromMaybe)

data Command = Ls ![LSResult] | Cd !String deriving (Show)

data LSResult = LsDir !String | LsFile !Int !String deriving (Show)

data FilesTree a = File !Int !String | Dir !a !String ![FilesTree a] deriving (Eq, Show)

solve1 :: String -> Int
solve1 = sumDirSizes . calculateSizes . resultToTree . parseInput

solve2 :: String -> Int
solve2 = findSmallestDirToDelete . calculateSizes . resultToTree . parseInput

findSmallestDirToDelete :: FilesTree Int -> Int
findSmallestDirToDelete tree = fromMaybe (error "Nothing found") (find (>= minimalSize) allDirSizes)
  where
    getAllDirSizes :: FilesTree Int -> [Int]
    getAllDirSizes (File _ _) = []
    getAllDirSizes (Dir s _ nodes) = s : (nodes >>= getAllDirSizes)
    allDirSizes = sort $ getAllDirSizes tree
    minimalSize = 30000000 - (70000000 - last allDirSizes)

sumDirSizes :: FilesTree Int -> Int
sumDirSizes (File _ _) = 0
sumDirSizes (Dir s _ nodes) = sum (map sumDirSizes nodes) + (if s <= 100000 then s else 0)

calculateSizes :: FilesTree () -> FilesTree Int
calculateSizes (File s n) = File s n
calculateSizes (Dir () n ns) = Dir dirSize n ns'
  where
    ns' = map calculateSizes ns
    dirSize = sum $ map nodeSize ns'
    nodeSize (Dir x _ _) = x
    nodeSize (File x _) = x

resultToTree :: [Command] -> FilesTree ()
resultToTree cs = resolve $ resultToTree' cs []
  where
    resolve ([xs], []) = xs
    resolve _ = error "Tree not parsed correctly"

resultToTree' :: [Command] -> [FilesTree ()] -> ([FilesTree ()], [Command])
resultToTree' ((Cd name) : cs) dir =
  if maybe True isCdUp (head' newCs)
    then (newDir, drop 1 newCs)
    else resultToTree' (Cd name : newCs) newDir
  where
    nameDir = fromMaybe (error $ "Cannot find " <> name) $ getDirByName name dir
    (newNameDir, newCs) = resultToTree' cs nameDir
    newDir = replaceDirByName name dir newNameDir
resultToTree' ((Ls rs) : cs) _ = (map lsToFile rs, cs)
  where
    lsToFile (LsDir name) = Dir () name []
    lsToFile (LsFile s name) = File s name
resultToTree' [] cs = (cs, [])

isCdUp :: Command -> Bool
isCdUp (Cd "..") = True
isCdUp _ = False

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

replaceDirByName :: String -> [FilesTree ()] -> [FilesTree ()] -> [FilesTree ()]
replaceDirByName name dir newDir = filter (not . isDirByName name) dir ++ [Dir () name newDir]

getDirByName :: String -> [FilesTree a] -> Maybe [FilesTree a]
getDirByName name dir = getContent <$> find (isDirByName name) dir
  where
    getContent (File _ _) = error "expected dir"
    getContent (Dir _ _ xs) = xs

isDirByName :: String -> FilesTree a -> Bool
isDirByName _ (File _ _) = False
isDirByName name (Dir _ name' _) = name' == name

parseInput :: String -> [Command]
parseInput = unsafeParse expr
  where
    expr = many1 (choice [cdE, lsE] <* char '\n')
    cdE :: Parser Command
    cdE = Cd <$> (string "$ cd " *> many1 (satisfy (/= '\n')))
    lsE = Ls <$> (string "$ ls\n" *> sepBy lsResultE (char '\n'))
    lsResultE =
      choice
        [ LsFile <$> (decimal <* space) <*> many1 (satisfy (/= '\n')),
          LsDir <$> (string "dir" *> space *> many1 (satisfy (/= '\n')))
        ]
