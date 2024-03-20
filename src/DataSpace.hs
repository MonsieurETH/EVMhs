module DataSpace
  ( DataSpace,
    newDataSpace,
    writeBytes,
    readBytes,
    memorySize,
  )
where

import qualified Data.HashMap.Strict as HashMap
import Debug.Trace (trace)

type DataSpace = (HashMap.HashMap Int Char, HashMap.HashMap (Int, Int) Bool)

newDataSpace :: DataSpace
newDataSpace = (HashMap.empty, HashMap.empty)

writeBytes :: DataSpace -> Int -> String -> DataSpace
writeBytes (mem, size) pos bytes =
  let updatedMem = foldl updateSpace mem (zip [realPos ..] bytes)
      updatedSize = foldl updateSize size (memoryRange32 0 (pos + (length bytes `div` 2) - 1))
   in (updatedMem, updatedSize)
  where
    updateSpace acc (p, b) = HashMap.insert p b acc
    updateSize acc k = HashMap.insert k True acc
    realPos = pos * 2

memoryRange32 :: Int -> Int -> [(Int, Int)]
memoryRange32 p l
  | l == 0 = []
  | l `mod` 32 /= 0 = (l - l `mod` 32, l + 32 - l `mod` 32 - 1) : memoryRange32 p (l - l `mod` 32)
  | otherwise = (l - 32, l - 1) : memoryRange32 p (l - 32)

readBytes :: DataSpace -> Int -> Int -> (DataSpace, String)
readBytes (mem, memSize) pos len =
  let bytes = map (\p -> HashMap.lookupDefault '0' p mem) [realPos .. realPos + realLen - 1]
      updatedSize = foldl updateSize memSize (memoryRange32 0 (pos + (length bytes `div` 2) - 1))
   in ((mem, updatedSize), bytes)
  where
    updateSize acc k = HashMap.insert k True acc
    realPos = pos * 2
    realLen = len * 2

memorySize :: DataSpace -> Int
memorySize (_, memSize) = HashMap.size memSize * 32
