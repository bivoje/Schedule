
module Main where


import Explorer
import Extract

import Codec.Xlsx
import Graphics.Vty
import Control.Monad.RWS

import qualified Data.ByteString.Lazy as L
import System.Environment
import System.Exit


-- simple terminal xlsx file explorer
main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
    die "specify xlsx file path!"

  putStrLn ("loading file " ++ args !! 0)

  pxlsx <- load (args !! 0)
  xlsx <- case pxlsx of
    Left err -> die ("error while loading: " ++ show err)
    Right xlsx -> return xlsx

  cfg <- standardIOConfig
  vty <- mkVty cfg
  execRWST explorer vty xlsx
  shutdown vty

load :: String -> IO (Parser Xlsx)
load filepath = toXlsxEither <$> L.readFile filepath
