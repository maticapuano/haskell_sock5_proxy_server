module Main where

import Proxy
import Data.Word
import Data.Vector as DV
import Data.Vector.Unboxed as DVU
import Data.ByteString as BS
import Data.Maybe
import GHC.IO.Handle
import Data.Bits
import System.IO
import System.Environment
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Network.Socket
import Network.Socket.ByteString
import Network.Socket.Internal

import Foreign
import Foreign.C

import Control.Exception
import Control.Monad (when)
import Control.Monad.Fix (fix)

foreign import ccall unsafe "htonl" htonl :: Word32 -> Word32

data StreamState = StreamState
  { sseq :: Word16
  , tcpbuf :: ByteString
  , databuf :: ByteString
  , sock :: Socket } deriving (Show)

data Direction = Upstream | Downstream deriving (Show)

relayer :: Direction -> Socket -> TChan (Direction, ByteString) -> IO ()
relayer dir sock chan = forever $ do
  tcp_data <- recv sock 65536
  atomically $ writeTChan chan (dir, tcp_data)

midleLoop :: Chan String -> TChan(Direction, ByteString) -> StreamState -> StreamState -> IO()
midleLoop printChan dataChan cSock sSock = do
  (dir, tcp_data) <- atomically $ readTChan dataChan
  let (sender, receiver) = case dir of
        Upstream -> (cSock, sSock)
        Downstream -> (sSock, cSock)
  -- let sender1 = sender { tcpbuf = tcpbuf sender <> tcp_data }
  -- (gameState1, sender2, receiver1) <- proctcp printChan gameState xorKey sender1 receiver dir 0
  writeChan printChan (show tcp_data)
  send (sock receiver) tcp_data
  let (client1, server1) = case dir of
        Upstream -> (sender, receiver)
        Downstream -> (receiver, sender)
  midleLoop printChan dataChan client1 server1

msmMITM :: Chan String -> MITM
msmMITM printChan clientSock serverSock = do
  middleChan <- atomically $ newTChan
  let closeSocks (Left a) = print a >> Network.Socket.close clientSock >> Network.Socket.close serverSock
      closeSocks (Right a) = Network.Socket.close clientSock >> Network.Socket.close serverSock
      cState = StreamState{sock = clientSock, sseq = 1, tcpbuf = BS.empty, databuf = BS.empty}
      sState = StreamState{sock = serverSock, sseq = 0, tcpbuf = BS.empty, databuf = BS.empty}

  forkFinally (midleLoop printChan middleChan cState sState) closeSocks
  forkFinally (relayer Upstream clientSock middleChan) closeSocks
  forkFinally (relayer Downstream serverSock middleChan) closeSocks
  return ()

printMessages :: Chan String -> IO ()
printMessages printChan = forever $ do
  str <- readChan printChan
  Prelude.putStrLn str
  hFlush stdout

spawnProxy port = do
    printChan <- newChan
    forkIO (printMessages printChan)
    runProxy port (msmMITM printChan)

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    System.IO.hPutStrLn hdl "Hi, what's your name?"
    name <- fmap Prelude.init (System.IO.hGetLine hdl)
    broadcast ("--> " Prelude.++ name Prelude.++ " entered chat.")
    System.IO.hPutStrLn hdl ("Welcome, " Prelude.++ name Prelude.++ "!")

    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ System.IO.hPutStrLn hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap Prelude.init (System.IO.hGetLine hdl)
        case line of
             -- If an exception is caught, send a message and break the loop
             "quit" -> System.IO.hPutStrLn hdl "Bye!"
             -- else, continue looping.
             _      -> broadcast (name Prelude.++ ": " Prelude.++ line) >> loop

    killThread reader                      -- kill after the loop ends
    broadcast ("<-- " Prelude.++ name Prelude.++ " left.") -- make a final broadcast
    hClose hdl                             -- close the handle

main :: IO ()
main = do
--   port:_ <- getArgs
  -- spawnProxy "1080"

  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 $ htonl (0))
  listen sock 2
  chan <- newChan
  _ <- forkIO $ fix $ \loop -> do
    (_, _) <- readChan chan
    loop
  mainLoop sock chan 0
