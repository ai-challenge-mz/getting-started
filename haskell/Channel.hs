
module Channel where

import System.IO

data OutChannel = OutChannel
    { sendLine :: String -> IO ()
    , closeOutChannel :: IO ()
    }

data InChannel = InChannel
    { receiveLine :: IO String
    , channelEOF :: IO Bool
    , closeInChannel :: IO ()
    }

inChannelFromHandle :: Handle -> InChannel
inChannelFromHandle h = InChannel
    (hGetLine h)
    (hIsEOF h)
    (hClose h)

stdinChannel :: InChannel
stdinChannel = inChannelFromHandle stdin

yesChannel :: String -> InChannel
yesChannel line = InChannel (return line) (return False) (return ())

outChannelFromHandle :: Handle -> OutChannel
outChannelFromHandle h = OutChannel
    (\ln -> hPutStrLn h ln >> hFlush h)
    (hClose h)

stdoutChannel :: OutChannel
stdoutChannel = outChannelFromHandle stdout

whateverChannel :: OutChannel
whateverChannel = OutChannel (const (return ())) (return ())