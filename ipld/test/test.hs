import Control.Monad
import System.Exit (exitFailure)
import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Test.Data.IPLD.Graph as G
import qualified Test.Data.IPLD.Multihash as MH

props :: IO [Bool]
props = sequence [G.props, MH.props]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- props

  unless (and results) exitFailure
