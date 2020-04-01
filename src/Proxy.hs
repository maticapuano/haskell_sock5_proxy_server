module Proxy where

import Network.Socket
import Network.Socket.ByteString
import Foreign
import Foreign.C
import Network.Socket.Internal
import Control.Monad
import Data.ByteString
import Control.Concurrent
-- import System.Posix.IO
-- import System.Posix.Types

-- Parse the sockaddr structure that is used by the C sockets API into its Haskell
-- SockAddr equivalent. These functions are probably already implemented inside
-- Network.Socket but, alas, are hidden.

parseSockAddrUnix :: Ptr a -> IO SockAddr

parseSockAddrUnix ptr = do
  path <- peekCString (plusPtr ptr 2)
  return $ SockAddrUnix path

parseSockAddrInet :: Ptr a -> IO SockAddr

parseSockAddrInet ptr = do
  port <- peekByteOff ptr 2
  addr <- peekByteOff ptr 4
  return $ SockAddrInet port addr

parseSockAddrInet6 :: Ptr a -> IO SockAddr

parseSockAddrInet6 ptr = do
  port <- peekByteOff ptr 2
  flowInfo <- peekByteOff ptr 4
  addr0 <- peekByteOff ptr 8
  addr1 <- peekByteOff ptr 12
  addr2 <- peekByteOff ptr 16
  addr3<- peekByteOff ptr 20
  scopeId <- peekByteOff ptr 24
  return $ SockAddrInet6 port flowInfo (addr0, addr1, addr2, addr3) scopeId

-- Must change ccall to stdcall for i386. getsockopt is defined in the object files linked
-- in by the Network package.

foreign import ccall unsafe "getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt

foreign import ccall unsafe "getsockname"
  c_getsockname :: CInt -> Ptr a-> Ptr CInt -> IO CInt

-- Gets the original socket address. This is needed because iptables overwrites packet
-- headers (including destination address and port) when doing REDIRECTions. A TPROXY
-- target is not viable because it only works on the PREROUTING chain, whereas output from
-- the emulator is on the OUTPUT chain.

getSockOrigDest :: Socket -> IO SockAddr
getSockOrigDest s = do
  let szInt = 128 :: Int -- Size of struct sockaddr_storage
      szCInt = 128 :: CInt -- Size of struct sockaddr_storage
      solIP = 0 -- Protocol level required for SO_ORIGINAL_DST
      soOrigDest = 80 -- The option name SO_ORIGINAL_DST has value 80
      familyOffset = 0 -- Offset of sin_family member of sockaddr_in
  allocaBytes szInt $ \ptr -> do
    withFdSocket s $ \fd -> with szCInt $ \ptr_sz -> do
      throwSocketErrorIfMinus1Retry_ "getSockOrigDest" $ 
        -- c_getsockopt fd solIP soOrigDest ptr ptr_sz
        c_getsockname fd ptr ptr_sz
      family <- peekByteOff ptr familyOffset
      print $ unpackFamily (fromIntegral (family :: CShort))
      case unpackFamily (fromIntegral (family :: CShort)) of
        AF_UNIX -> parseSockAddrUnix ptr
        AF_INET -> parseSockAddrInet ptr
        AF_INET6 -> parseSockAddrInet6 ptr
        _ -> throwSocketError ("Unsupported socket address type: " ++ show family)

getSockAddrFamily :: SockAddr -> Family

getSockAddrFamily (SockAddrUnix _) = AF_UNIX

getSockAddrFamily (SockAddrInet _ _) = AF_INET

getSockAddrFamily (SockAddrInet6 _ _ _ _) = AF_INET6

-- An MITM is a function that takes the addresses of two endpoints, a handle for reading
-- data sent from the first endpoint to the second, and a handle for reading data sent
-- from the second endpoint to the first.

type MITM = Socket -> Socket -> IO () 
-- Connect to the supplied socket's original destination. Then send data received from
-- one socket to the other, and vice-versa. When either socket is closed, close the other.
-- All the while, make sure the MITM is receiving copies of the communication.

proxySocket :: Socket -> MITM -> IO ()
proxySocket clientSock mitm = do
  serverAddr <- getSockOrigDest clientSock
  print serverAddr
  let serverAI = defaultHints { addrSocketType = Stream, addrAddress = serverAddr, addrFamily = getSockAddrFamily serverAddr  }
  print ("addrFamily", addrFamily serverAI)
  serverSock <- socket (addrFamily serverAI) (addrSocketType serverAI) (addrProtocol serverAI)
  connect serverSock serverAddr
  forkIO (mitm clientSock serverSock)
  return ()

-- Start listening for client connections on the given port. When a connection is made,
-- transparently forward data to the original destination. Give a copy of the data sent
-- by the client to mitm1 and give a copy of the data sent by the server to mitm2.

runProxy :: String -> MITM -> IO ()
runProxy port mitm = do
  let proxyHints = defaultHints { addrSocketType = Stream, addrFlags = [AI_PASSIVE] }
  proxyAI:_ <- getAddrInfo (Just proxyHints) Nothing (Just port)
  proxySock <- socket (addrFamily proxyAI) (addrSocketType proxyAI) (addrProtocol proxyAI)
  setSocketOption proxySock ReuseAddr 1
  bind proxySock (addrAddress proxyAI)
  listen proxySock 1
  forever $ do
    (clientSock, _) <- accept proxySock
    proxySocket clientSock mitm
