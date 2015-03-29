
module Report (dumpReport) where

import qualified Data.ByteString.Lazy.Char8 as B
-- import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson (encode)

import Types
import JSON

dumpReport :: Replay -> FilePath -> IO ()
dumpReport replay filename = do
    B.writeFile filename prologue
    B.appendFile filename (encode replay)
    -- B.appendFile filename (encodePretty replay)
    B.appendFile filename epilogue

prologue :: B.ByteString
prologue = B.pack
    "<!DOCTYPE html>\
    \<html>\
    \  <head>\
    \    <script language=\"javascript\" src=\"all.js\"></script>\
    \  </head>\
    \  <body>\
    \    <pre id=\"json\" style=\"display:none\">"

epilogue :: B.ByteString
epilogue = B.pack
    "    </pre>\
    \  </body>\
    \</html>"