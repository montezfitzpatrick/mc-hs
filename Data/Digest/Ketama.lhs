Ketama is imagined as a consistent way to map keys to nodes.

See <http://www.last.fm/user/RJ/journal/2007/04/10/rz_libketama_-_a_consistent_hashing_algo_for_memcache_clients>
for more information on the library.

/begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}

module Data.Digest.Ketama where

import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, FunPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.C.Types (CInt(..), CUInt(..), CSize(..), CUChar(..))
import Foreign.C.String (CString)

import Data.Word (Word8)


/end{code}

Ketama Library C Functions.

/begin{code}
foreign import ccall unsafe "static ketama.h ketama_roll" c_ketama_roll
	:: Ptr Word8 -> Ptr Word8 -> IO CInt

ketamaRoll :: Ptr Word8 -> Ptr Word8 -> IO CInt
ketamaRoll p q = c_ketama_roll p q

foreign import ccall unsafe "static ketama.h ketama_smoke" c_ketama_smoke
	:: Ptr Word8 -> IO ()

ketamaSmoke :: Ptr Word8 -> IO ()
ketamaSmoke p = c_ketama_smoke p >> return ()

foreign import ccall unsafe "static ketama.h ketama_get_server" c_ketama_get_server
	:: Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)

ketamaGetServer :: Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)
ketamaGetServer p q = c_ketama_get_server p q

foreign import ccall unsafe "static ketama.h ketama_compare" c_ketama_compare
	:: Ptr Word8 -> Ptr Word8 -> IO CInt

ketamaCmp :: Ptr Word8 -> Ptr Word8 -> IO CInt
ketamaCmp p q = c_ketama_compare p q

foreign import ccall unsafe "static ketama.h ketama_hashi" c_ketama_hashi
	:: Ptr Word8 -> IO CUInt

ketamaHashInt :: Ptr Word8 -> IO CUInt
ketamaHashInt p = c_ketama_hashi p

foreign import ccall unsafe "static ketama.h ketama_md5_digest" c_ketama_md5_digest
	:: Ptr Word8 -> Ptr CUChar -> IO ()

ketamaHashMD5 :: Ptr Word8 -> Ptr CUChar -> IO ()
ketamaHashMD5 p q = c_ketama_md5_digest p q >> return ()
/end{code}
