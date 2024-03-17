module DataSpace
  ( DataSpace,
    newDataSpace,
    writeBytes,
    readBytes,
  )
where

import qualified Data.HashMap.Strict as HashMap

type DataSpace = HashMap.HashMap Int Int

newDataSpace :: DataSpace
newDataSpace = HashMap.empty

writeBytes :: DataSpace -> Int -> [Int] -> DataSpace
writeBytes mem pos bytes = foldl updateSpace mem (zip [pos ..] bytes)
  where
    updateSpace acc (p, b) = HashMap.insert p b acc

readBytes :: DataSpace -> Int -> Int -> [Int]
readBytes mem pos len = map (\p -> HashMap.lookupDefault 0 p mem) [pos .. pos + len - 1]
