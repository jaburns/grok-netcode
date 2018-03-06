module Network(
    box
) where

import Control.Monad.Trans.State

data Network = Network'
  { packets :: [String]
  }

type NetworkM = State Network

push :: String -> NetworkM ()
push packet = do
    x <- get
    put . Network' $ packet:(packets x)

-- stepNetwork :: ServerSocket -> ClientSocket -> Network -> Network

pushTwice :: NetworkM ()
pushTwice = do
    push "Hello"
    push "world"

testRun :: [String]
testRun = packets $ execState pushTwice (Network' ["see"])