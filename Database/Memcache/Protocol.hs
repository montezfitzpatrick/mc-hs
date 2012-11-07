module Database.Memcache.Protocol where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word

{- Error types... -}
-- ErrNotFound       = errors.New("mc: not found")
-- ErrKeyExists      = errors.New("mc: key exists")
-- ErrValueTooLarge  = errors.New("mc: value to large")
-- ErrInvalidArgs    = errors.New("mc: invalid arguments")
-- ErrValueNotStored = errors.New("mc: value not stored")
-- ErrNonNumeric     = errors.New("mc: incr/decr called on non-numeric value")
-- ErrAuthRequired   = errors.New("mc: authentication required")
-- ErrAuthContinue   = errors.New("mc: authentication continue (unsupported)")
-- ErrUnknownCommand = errors.New("mc: unknown command")
-- ErrOutOfMemory    = errors.New("mc: out of memory")

type Key        = ByteString
type Value      = ByteString
type Expiration = Word32
type Flags      = Word32
type Initial    = Word64
type Delta      = Word64
type Version    = Word64

-- 8 bits...
data Operation
    = OpGet         -- Extras: Word32 (rcv only)
    | OpGetQ
    | OpGetK
    | OpGetKQ
    | OpSet         -- Extras: (Word32, Word32) (snd only)
    | OpSetQ
    | OpAdd         -- Extras: (Word32, Word32) (snd only)
    | OpAddQ
    | OpReplace     -- Extras: (Word32, Word32) (snd only)
    | OpReplaceQ
    | OpDelete
    | OpDeleteQ
    | OpIncrement   -- Extras: (Word64, Word64, Word32)
    | OpIncrementQ
    | OpDecrement   -- Extras: (Word64, Word64, Word32)
    | OpDecrementQ
    | OpAppend
    | OpAppendQ
    | OpPrepend
    | OpPrependQ
    | OpTouch       -- Extras: Word32 (snd only)
    | OpGAT         -- Extras: Word32 (both snd/rcv)
    | OpGATQ
    | OpGATK
    | OpGATKQ
    | OpStat
    | OpQuit
    | OpQuitQ
    | OpFlush       -- Extras: Maybe Word32 (snd only)
    | OpFlushQ
    | OpNoop
    | OpVersion

-- 8 bits
data Direction
    = MsgSend
    | MsgRecv

data Header = Header {
        magic    :: Direction, -- 8 bits...
        op       :: Operation, -- 8 bits...
        keyLen   :: Word16,
        extraLen :: Word8,
        dataType :: Word8,  -- Not used...
        status   :: Word16, -- Not used for snd, only rcv
        bodyLen  :: Word32,
        opaque   :: Word32,
        cas      :: Version
    }

data Msg a = Msg {
        header :: Header,
        extras :: a,
        key    :: Key,
        value  :: Value
    }

sendHeader :: Header
sendHeader = Header {
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

emptyMsg :: Msg a
emptyMsg = Msg {
        header = sendHeader,
        extras = undefined,
        key    = B.empty,
        value  = B.empty
    }

