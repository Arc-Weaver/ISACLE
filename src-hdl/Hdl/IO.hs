{-# LANGUAGE FlexibleContexts #-}
-- | 'Entity' — the encapsulation typeclass: how a backend creates ('entity')
-- and instantiates ('instanceOf') entities.  The synthesis backend ('EntityDef')
-- is the instance below; sim / vhdl / verilog become further instances.
module Hdl.IO
    ( Entity(..)
    ) where

import Prelude

import Hdl.Net    (NetM)
import Hdl.Entity (EntityDef)
import Hdl.Types  (Named)
import qualified Hdl.Entity as E
import Hdl.Class  (instEntity)

-- | A backend's encapsulated entity @e i o@ (input bundle @i@, output @o@):
-- 'entity' builds one from a body, 'instanceOf' instantiates one into the
-- current design.  Both interfaces are 'Named' records (port names from fields).
class Entity e where
    -- | Build (encapsulate) an entity from a name and its body.
    entity     :: String -> (i -> NetM o) -> e i o
    -- | Instantiate an entity as a named sub-instance, wiring inputs → outputs
    -- (models VHDL @u : entity work.foo@).
    instanceOf :: (Named i, Named o) => String -> e i o -> i -> NetM o

instance Entity EntityDef where
    entity                    = E.entityDef
    instanceOf label e inputs = instEntity e label inputs
