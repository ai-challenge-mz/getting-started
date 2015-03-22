
import Control.Monad
import qualified Network.Simple.TCP as TCP
import Network.Socket (socketToHandle)
import System.Environment
import System.IO

import Channel
import Util

main :: IO ()
main = do
    [host, port] <- getArgs
    print (host, port)
    TCP.connect host port $ \(sock, _addr) -> do
        hPutStrLn stderr "Connected"
        h <- socketToHandle sock ReadWriteMode
        let chIn = inChannelFromHandle h
            chOut = outChannelFromHandle h
        forever $ do
            hPutStrLn stderr "reading to dot"
            _ <- chReadUntilDot chIn
            hPutStrLn stderr "replying with dot"
            sendLine chOut "."

