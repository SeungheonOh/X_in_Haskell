module Main where

import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Word
import System.IO
import System.Process
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Control.Exception ( SomeException(..),
                           AsyncException(..)
                         , catch, handle, throw)
import System.Environment

getAllFrames :: String -> Int -> Int -> Int -> IO [Word8]
getAllFrames file x y fps = do
  (_, Just handle, _, _) <- createProcess (proc (head $ words cmd) (tail $ words cmd)) {std_out = CreatePipe, std_err = NoStream}
  hSetBuffering handle NoBuffering
  bs <- B.hGetContents handle
  pure (B.unpack bs)
  where
    cmd = "ffmpeg -hide_banner -i "++file++" -s "++show x++"x"++show y++" -vf fps="++show fps++"/1 -f rawvideo -c:v rawvideo -pix_fmt rgb24 -"

data Pixel = Pixel
  { r :: Word8,
    g :: Word8,
    b :: Word8
  }
  deriving (Show, Ord, Eq)

rawToPixels :: [Word8] -> [Pixel]
rawToPixels [] = []
rawToPixels (r : g : b : xs) = Pixel r g b : rawToPixels xs

data Frame = Frame
  { pixels :: [Pixel],
    width :: Int,
    height :: Int
  }
  deriving (Show, Ord, Eq)

rawToFrame :: [Word8] -> Int -> Int -> Frame
rawToFrame raw = Frame (rawToPixels raw)

printFrame :: Frame -> IO ()
printFrame frame = printFrame' frame (length $ pixels frame)

printFrame' :: Frame -> Int -> IO ()
printFrame' _ 0 = pure ()
printFrame' frame x = do
  putStr $ "\ESC[38;2;" ++ show (r pixel) ++ ";" ++ show (g pixel) ++ ";" ++ show (b pixel) ++ "m█\ESC[39m"
  if (x - 1) `rem` width frame == 0 then putStr "\n" else pure ()
  printFrame' (Frame rest (width frame) (height frame)) (x - 1)
  where
    pixel : rest = pixels frame

eachFrame :: Int -> Int -> Chan() -> [Word8] -> IO ()
eachFrame _ _ _ [] = pure ()
eachFrame x y chan stream = do
  readChan chan
  putStr "\ESC[H" -- to top
  printFrame frame
  eachFrame x y chan rest
  where
    (rawFrame, rest) = splitAt (x * y * 3) stream
    frame = rawToFrame rawFrame x y

newTicker :: Int -> IO (Chan(), IO())
newTicker ms = do
  chan <- newChan
  thread <- async $ forever $ do
    threadDelay $ ms * 1000
    writeChan chan()
  return (chan,  cancel thread)

recover :: IO()
recover =
    putStrLn "\ESC[2J\ESC[H\ESC[?25hdone"

onAbort :: SomeException -> IO()
onAbort _ =
  recover


main :: IO ()
main = handle onAbort $ do
  args <- getArgs
  putStr "\ESC[2J\ESC[H\ESC[?25l" -- clear screen & to top
  (ticker, cancelTicker) <- newTicker $ 1000 `div` fps
  stream <- getAllFrames (head args) x y (fps `div` 2)
  eachFrame x y ticker stream
  cancelTicker
  recover
  where
    x = 140
    y = x `div` 2
    fps = 30