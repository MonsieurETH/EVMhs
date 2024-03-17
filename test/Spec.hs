{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON, decode, parseJSON, withObject, (.:))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromJust, isJust)
import Data.Stack (Stack, stackIsEmpty, stackNew, stackPop, stackPush, stackSize)
import Lib
import Numeric (showHex)
import Test.Hspec

data Test = Test
  { testName :: String,
    testHint :: String,
    testCode :: Code,
    testExpect :: Main.Expectation
  }
  deriving (Show)

data Code = Code
  { codeAsm :: Maybe String,
    codeBin :: String
  }
  deriving (Show)

data Expectation = Expectation
  { expectSuccess :: Bool,
    expectStack :: [String]
  }
  deriving (Show)

instance FromJSON Test where
  parseJSON = withObject "Test" $ \v ->
    Test
      <$> v .: "name"
      <*> v .: "hint"
      <*> v .: "code"
      <*> v .: "expect"

instance FromJSON Code where
  parseJSON = withObject "Code" $ \v ->
    Code
      <$> v .: "asm"
      <*> v .: "bin"

instance FromJSON Main.Expectation where
  parseJSON = withObject "Expectation" $ \v ->
    Expectation
      <$> v .: "success"
      <*> v .: "stack"

main :: IO ()
main = do
  jsonContent <- B.readFile "./test/test.json"
  let maybeTests = decode jsonContent :: Maybe [Test]
  case maybeTests of
    Just tests -> hspec $ describe "EVM Tests" $ mapM_ createTest tests
    Nothing -> putStrLn "Failed to parse JSON tests"

createTest :: Test -> Spec
createTest test = it (testName test) $ do
  let initialVm = newEVM
  let input = codeBin $ testCode test
  let result = Lib.run initialVm input

  isJust result `shouldBe` (expectSuccess (testExpect test))
  let updatedVm = fromJust result
  (map integerToHexStr (stackToList (stack updatedVm))) `shouldBe` (expectStack (testExpect test))

stackToList :: Stack a -> [a]
stackToList s
  | stackIsEmpty s = []
  | otherwise = case stackPop s of
      Just (s', item) -> item : stackToList s'
      Nothing -> []

integerToHexStr :: Integer -> String
integerToHexStr n
  | n == 0 = "0x0"
  | otherwise = "0x" ++ dropWhile (== '0') hexStr
  where
    hexStr = showHex (n `mod` (2 ^ 256)) ""
