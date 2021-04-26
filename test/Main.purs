module Test.Main where

import Prelude

import Concurrent.Channel (recvList, sendList)
import Concurrent.Channel.Stream (streamInput, streamOutput)
import Control.Monad.List.Trans (foldl, repeat, take)
import Data.Array (replicate)
import Data.Foldable (fold)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, joinFiber, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Buffer (fromString, toString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, unlink, writeTextFile)
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.Stream (destroy)
import Test.Assert (assert')

test :: String -> Aff Boolean -> Aff Unit
test s k = k >>= \r -> liftEffect $ assert' s r *> log ("[OK] " <> s)

testStreamInput :: Aff Unit
testStreamInput = test "stream/input" do
  let str = fold $ replicate (1024 * 1024) "0123456789"
      path = "./test1.tmp"
  writeTextFile UTF8 path str
  stream <- liftEffect $ createReadStream path
  inp <- liftEffect $ streamInput stream
  result <- foldl (<>) "" $ recvList inp >>= liftEffect <<< toString UTF8
  unlink path
  pure $ str == result

testStreamInputClose :: Aff Unit
testStreamInputClose = test "stream/input/close" do
  let str = fold $ replicate (1024 * 1024) "0123456789"
      path = "./test2.tmp"
  writeTextFile UTF8 path str
  stream <- liftEffect $ createReadStream path
  inp <- liftEffect $ streamInput stream
  liftEffect $ destroy stream
  result <- foldl (<>) "" $ recvList inp >>= liftEffect <<< toString UTF8
  unlink path
  pure $ str > result

testStreamOutput :: Aff Unit
testStreamOutput = test "stream/output" do
  let list = take (1024 * 1024) $ repeat "0123456789"
      path = "./test3.tmp"
  str <- foldl (<>) "" list
  stream <- liftEffect $ createWriteStream path
  output <- liftEffect $ streamOutput stream
  result1 <- sendList output $ list >>= liftEffect <<< flip fromString UTF8
  delay $ Milliseconds 1000.0
  result2 <- readTextFile UTF8 path
  unlink path
  pure $ result1 && str == result2

testStreamOutputClose :: Aff Unit
testStreamOutputClose = test "stream/output/close" do
  let list = take (1024 * 1024) $ repeat "0123456789"
      path = "./test4.tmp"
  str <- foldl (<>) "" list
  stream <- liftEffect $ createWriteStream path
  output <- liftEffect $ streamOutput stream
  fiber <- forkAff $ sendList output $ list >>= liftEffect <<< flip fromString UTF8
  liftEffect $ destroy stream
  delay $ Milliseconds 1000.0
  result1 <- readTextFile UTF8 path
  result2 <- joinFiber fiber
  unlink path
  pure $ str > result1 && not result2

main :: Effect Unit
main = launchAff_ do
  testStreamInput
  testStreamInputClose
  testStreamOutput
  testStreamOutputClose
