{-# LANGUAGE FlexibleContexts #-}
-- | 'HdlIO' — the entity typeclass: how a backend creates ('bind') and
-- instantiates ('entity') entities.  The synthesis backend ('Entity') is the
-- instance below; sim / vhdl / verilog become further instances.
module Hdl.IO
    ( HdlIO(..)
    ) where

import Prelude

import Hdl.Net    (NetM)
import Hdl.Entity (Entity)
import Hdl.Types  (Named)
import qualified Hdl.Entity as E
import Hdl.Class  (instEntity)

-- | A backend's entity representation @h i o@ (input bundle @i@, output @o@):
-- 'bind' builds one from a body, 'entity' instantiates one into the current
-- design.  Both interfaces are 'Named' records (port names from fields).
class HdlIO h where
    -- | Build an entity from a name and its body.
    bind   :: String -> (i -> NetM o) -> h i o
    -- | Instantiate an entity as a named sub-instance, wiring inputs → outputs.
    entity :: (Named i, Named o) => String -> h i o -> i -> NetM o

instance HdlIO Entity where
    bind                  = E.entity
    entity label e inputs = instEntity e label inputs
