module Lib (
  serve,
) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Network.Socket
import Network.Socket.ByteString (recvFrom, sendTo)

broadcast :: [SockAddr] -> SockAddr -> Socket -> ByteString -> IO ()
broadcast clients author target msg = forM_ (filter (/= author) clients) $ \client -> do
  sendTo target msg client

listenPackets :: Socket -> [SockAddr] -> IO b
listenPackets sock clients = do
  (msg, client) <- Network.Socket.ByteString.recvFrom sock 100
  case (words . unpack) msg of
    ("login" : _) -> do
      _ <- sendTo sock (pack "> ") client
      listenPackets sock (clients ++ [client])
    text -> do
      broadcast clients client sock (pack ("\ESC[1A\n\r\ESC[2Kanon> " ++ unwords text ++ "\n> "))
      _ <- sendTo sock (pack "> ") client
      listenPackets sock clients

serve :: IO ()
serve = do
  addr <- head <$> getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  sock <- socket (addrFamily addr) Datagram defaultProtocol
  bind sock (addrAddress addr)
  _ <- listenPackets sock []
  close sock
