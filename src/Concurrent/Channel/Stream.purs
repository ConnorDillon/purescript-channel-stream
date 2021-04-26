module Concurrent.Channel.Stream
  ( streamChannel
  , streamInput
  , streamOutput
  , module Channel
  ) where

import Prelude

import Concurrent.Channel (Channel, Input(..), Output(..))
import Concurrent.Channel (Channel, Input(..), Output(..), send, recv) as Channel
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff.AVar (put, take)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer)
import Node.Stream (Duplex, Readable, Writable)
import Node.Stream as Stream

foreign import onceDrain
  :: forall a
   . Writable a
  -> Effect Unit
  -> Effect Unit

foreign import isWritable
  :: forall a
   . Writable a
  -> Effect Boolean

foreign import onceReadable
  :: forall a
   . Readable a
  -> Effect Unit
  -> Effect Unit

foreign import isReadable
  :: forall a
   . Readable a
  -> Effect Boolean

foreign import readableLength
  :: forall a
   . Readable a
  -> Effect Number

-- | Creates a new `Output` with a provided writable `Stream` as backend.
streamOutput :: forall a. Writable a -> Effect (Output Buffer)
streamOutput stream = do
  var <- AVar.new unit
  pure $ Output \buf -> do
    _ <- take var
    writable <- liftEffect $ isWritable stream
    if writable
       then do
         result <- liftEffect $ Stream.write stream buf $ pure unit
         if result
            then put unit var
            else liftEffect $ onceDrain stream $ void $ AVar.put unit var \_ -> pure unit
         pure true
       else pure false

-- | Creates a new `Input` with a provided readable `Stream` as backend.
streamInput :: forall a. Readable a -> Effect (Input Buffer)
streamInput stream = do
  var <- AVar.empty
  pure $ Input do
    len <- liftEffect $ readableLength stream
    if len > 0.0
       then liftEffect $ Stream.read stream Nothing
       else do
         readable <- liftEffect $ isReadable stream
         if readable
            then do
              liftEffect $ onceReadable stream do
                buf <- Stream.read stream Nothing
                _ <- AVar.put buf var \_ -> pure unit
                pure unit
              take var
            else pure Nothing

-- | Creates a new `Channel` with a provided duplex `Stream` as backend.
-- | Closing the `Channel` will destroy the `Stream`.
streamChannel :: Duplex -> Effect (Channel Buffer Buffer)
streamChannel s = do
  output <- streamOutput s
  input <- streamInput s
  pure {output, input, close: liftEffect $ Stream.destroy s}
