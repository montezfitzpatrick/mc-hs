{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.Memcache.Protocol
import Database.Memcache.Wire

import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Monoid

main :: IO ()
main =
    defaultMain [
        bgroup "header" [
            bench "serialize"   $ whnf serializeHeader getHeader,
            bench "deserialize" $ whnf deserializeHeader' getRespHeaderBytes
        ],
        bgroup "get" [
            bench "serialize"   $ whnf (serializeMsg' noExtras) getReqMsg,
            bench "deserialize" $ whnf (deserializeMsg' deserializeFlags) getRespBytes
        ]
    ]

getHeader :: Header
getHeader = Header {
        magic    = MsgSend,
        op       = OpGet,
        keyLen   = 0,
        extraLen = 0,
        dataType = 0,
        status   = 0,
        bodyLen  = 0,
        opaque   = 0,
        cas      = 0
    }

getReqMsg :: Msg ()
getReqMsg = Msg {
        header = Header {
            magic    = MsgSend,
            op       = OpGet,
            keyLen   = fromIntegral $ B.length key',
            extraLen = 0,
            dataType = 0,
            status   = 0,
            bodyLen  = fromIntegral $ B.length val',
            opaque   = 0,
            cas      = 0
        },
        extras = (),
        key    = key',
        value  = val'
    }
  where
    key' = "hello world"
    val' = "aren't you some fun?"

getRespHeaderBytes :: L.ByteString
getRespHeaderBytes =
    --        magic, op,               extral, 0
    L.pack $ [0x81, 0x00] ++ keyl' ++ [0x04, 0x00] ++ status' ++ bodyl' ++ opaque' ++ cas'
  where
    keyl'   = [0x00, 0x00]
    status' = [0x00, 0x00, 0x00, 0x00]
    bodyl'  = [0x00, 0x00, 0x00, 0x08]
    opaque' = [0x00, 0x00, 0x00, 0x07]
    cas'    = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x09]

getRespBytes :: L.ByteString
getRespBytes = getRespHeaderBytes <> L.pack extras' <> L.pack key' <> value'
  where
    extras' = [0x00, 0x00, 0x00, 0x01] -- BE: so 1?
    key'    = []
    value'  = "12345678"

