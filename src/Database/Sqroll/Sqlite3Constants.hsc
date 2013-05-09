{-# LANGUAGE CPP #-}

module Database.Sqroll.Sqlite3Constants (
      sqlite3TransientPtr
) where

import Foreign.Ptr


#include <sqlite3.h>



-- | Value to pass into sqlite3_bind_blob / sqlite3_bind_text / etc. to make
-- clear that sqlite should make its own copy of the memory.
--
-- This is necessary in garbage-collected language bindings where the
-- life-time of the created statement and the string is is created from
-- are generally not the same.
--
-- See http://www.sqlite.org/capi3ref.html#sqlite3_bind_blob
--
-- Beware that SQLITE_TRANSIENT is a void* == -1 in sqlite,
-- so don't freak out if you see this number to be something like
-- 18446744073709551615 - it is 0xFF..FFF.
-- I really have no idea why they couldn't just make it 1 ...
--
-- This is also the reason why this constant is *NOT* exported as an Int.
sqlite3TransientPtr :: Ptr ()
sqlite3TransientPtr = intPtrToPtr #{const SQLITE_TRANSIENT}
