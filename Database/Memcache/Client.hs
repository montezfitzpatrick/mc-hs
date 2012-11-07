module Database.Memcache.Client where

import Database.Memcache.Protocol
import Database.Memcache.Server
import Database.Memcache.Wire

import Data.Word

-- XXX: Errors as exceptions or return values?

get :: Connection -> Key -> Version -> IO (Value, Flags, Version)
get c k v = do
    -- XXX: check key length is valid
    let msg = serializeMsg noExtras $ emptyMsg {
            header = sendHeader { op = OpGet, cas = v },
            key = k
        }
    r_z <- sendRecv c msg
    let r = deserializeMsg' deserializeFlags r_z
    return (value r, extras r, cas $ header r)

gat :: Connection -> Key -> Expiration -> IO (Value, Flags, Version)
gat c k e = do
    let msg = serializeMsg serializeExpiration $ emptyMsg {
            header = sendHeader { op = OpGAT },
            key    = k,
            extras = e
        }
    r_z <- sendRecv c msg
    let r = deserializeMsg' deserializeFlags r_z
    return (value r, extras r, cas $ header r)

touch :: Connection -> Key -> Expiration -> IO Version
touch c k e = do
    let msg = serializeMsg serializeExpiration $ emptyMsg {
            header = sendHeader { op = OpTouch },
            key    = k,
            extras = e
        }
    r_z <- sendRecv c msg
    let r = deserializeMsg' noExtras' r_z
    return (cas $ header r)

--

set :: Connection -> Key -> Value -> Flags -> Expiration -> Version -> IO Version
set c k v f e ver = do
    let msg = serializeMsg serializeFE $ emptyMsg {
            header = sendHeader { op = OpSet, cas = ver },
            extras = (f, e),
            key    = k,
            value  = v
        }
    r_z <- sendRecv c msg
    let r = deserializeMsg' noExtras' r_z
    return (cas $ header r)

add :: Connection -> Key -> Value -> Flags -> Expiration -> IO Version
add c k v f e = do
    let msg = serializeMsg serializeFE $ emptyMsg {
            header = sendHeader { op = OpAdd },
            extras = (f, e),
            key    = k,
            value  = v
        }
    r_z <- sendRecv c msg
    let r = deserializeMsg' noExtras' r_z
    return (cas $ header r)

replace :: Connection -> Key -> Value -> Flags -> Expiration -> IO Version
replace c k v f e = do
    let msg = serializeMsg serializeFE $ emptyMsg {
            header = sendHeader { op = OpReplace },
            extras = (f, e),
            key    = k,
            value  = v
        }
    r_z <- sendRecv c msg
    let r = deserializeMsg' noExtras' r_z
    return (cas $ header r)

--

delete :: Connection -> Key -> Version -> IO ()
delete = undefined

--

increment :: Connection -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Word64, Version)
increment = undefined

decrement :: Connection -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Word64, Version)
decrement = undefined

--

append :: Connection -> Key -> Value -> Version -> IO Version
append = undefined

prepend :: Connection -> Key -> Value -> Version -> IO Version
prepend = undefined

--

flush :: Connection -> Expiration -> IO ()
flush = undefined

noop :: Connection -> IO ()
noop = undefined

version :: Connection -> IO String
version = undefined

quit :: Connection -> IO ()
quit = undefined

