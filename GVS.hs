module GVS (DENKEN(..), GVS, getGVS, sendCommand, makeJanken, closeGVS) where

import System.Hardware.Serialport
import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (pack)
import Data.Word (Word8)
import Control.Monad (forM_, forever)
import Control.Concurrent (threadDelay)

type Port = String
type GVS = SerialPort
type Current = Int
type Channel = Int
data DENKEN = Rock | Scissors | Paper | Unknown deriving (Eq,Show)

getGVS :: Port -> IO GVS
getGVS port = openSerial port defaultSerialSettings{ commSpeed = CS115200 }

sendCommand :: GVS -> Channel -> Current -> IO Int
sendCommand gvs ch i = send gvs . pack $ makeCommand ch i

makeCommand :: Channel -> Current -> [Word8]
makeCommand ch i = map fromIntegral [comm1, comm2] where
	pole = if i >= 0 then 1 else 0
	comm1 = (shiftL ch 5) + (shiftL pole 4) + (shiftR i 8)
	comm2 = (2^8-1) .&. i

makeRectangleWave :: GVS -> Channel -> Current -> Int -> IO ()
makeRectangleWave gvs ch i interval = forM_ [1..20] f where
	f _ = do
		_ <- sendCommand gvs ch i
		threadDelay (interval * 1000)
		_ <- sendCommand gvs ch 0
		threadDelay (interval * 1000)


makeJanken :: GVS -> DENKEN -> IO ()
makeJanken gvs Rock = makeRectangleWave gvs 0 3000 50
makeJanken gvs Paper = makeRectangleWave gvs 1 3000 50
makeJanken gvs Scissors = forM_ [1..20] f where
	interval = 50
	f _ = do
		_ <- sendCommand gvs 0 2000
		_ <- sendCommand gvs 1 4000
		threadDelay (interval * 1000)
		_ <- sendCommand gvs 0 0
		_ <- sendCommand gvs 1 0
		threadDelay (interval * 1000)

testJanken gvs ch = makeRectangleWave gvs ch 3000 50

closeGVS :: GVS -> IO ()
closeGVS = closeSerial