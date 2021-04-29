module Test.Main where

import Prelude

import Concurrent.Channel (recvList, sendList)
import Concurrent.Channel.Stream (streamInput, streamOutput)
import Control.Monad.List.Trans (foldl, repeat, take)
import Effect (Effect)
import Effect.Aff (Aff, forkAff, joinFiber, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Buffer (fromString, toString)
import Node.Encoding (Encoding(..))
import Node.Stream (Duplex, pipe)
import Node.Stream as Stream
import Test.Assert (assert')

test :: String -> Aff Boolean -> Aff Unit
test s k = k >>= \r -> liftEffect $ assert' s r *> log ("[OK] " <> s)

foreign import createGzip :: Effect Duplex

foreign import createGunzip :: Effect Duplex

testStreamChannelBase :: Boolean -> Aff Boolean
testStreamChannelBase close = do
  gzip <- liftEffect createGzip
  gunzip <- liftEffect createGunzip
  _ <- liftEffect $ pipe gzip gunzip
  output <- liftEffect $ streamOutput gzip
  input <- liftEffect $ streamInput gunzip
  part <- foldl (<>) "" $ take 1024 $ repeat "0123456789"
  let list = take 1024 $ repeat part
      end = liftEffect $ Stream.end gzip $ pure unit
  str <- foldl (<>) "" list
  f1 <- forkAff do
    r <- sendList output $ list >>= flip fromString UTF8 >>> liftEffect
    when (not close) end 
    pure r
  when close end
  r1 <- foldl (<>) "" $ recvList input >>= toString UTF8 >>> liftEffect
  r2 <- joinFiber f1
  pure $ if close
     then r1 < str && not r2
     else r1 == str && r2

testStreamChannel :: Aff Unit
testStreamChannel = test "stream/channel" $ testStreamChannelBase false

testStreamChannelClose :: Aff Unit
testStreamChannelClose = test "stream/channel/close" $ testStreamChannelBase true

main :: Effect Unit
main = launchAff_ do
  testStreamChannel
  testStreamChannelClose
  liftEffect $ log "[DONE]"
