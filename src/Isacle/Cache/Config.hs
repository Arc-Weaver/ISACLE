-- | L1 cache configuration types.
module Isacle.Cache.Config
    ( CacheConfig(..)
    , WritePolicy(..)
    , defaultCacheConfig
    ) where

import Prelude

-- | L1 cache parameters.
data CacheConfig = CacheConfig
    { ccSize        :: Int          -- ^ total data bytes; must be a power of 2
    , ccWays        :: Int          -- ^ associativity (1 = direct-mapped)
    , ccLineWords   :: Int          -- ^ cache line size in words
    , ccWritePolicy :: WritePolicy
    } deriving (Show, Eq)

data WritePolicy
    = WriteThrough  -- ^ stores update cache and system bus immediately
    | WriteBack     -- ^ stores update cache only; writeback on eviction
    deriving (Show, Eq)

-- | Sensible defaults: 4 KiB, direct-mapped, 4-word lines, write-through.
defaultCacheConfig :: CacheConfig
defaultCacheConfig = CacheConfig
    { ccSize        = 4096
    , ccWays        = 1
    , ccLineWords   = 4
    , ccWritePolicy = WriteThrough
    }
