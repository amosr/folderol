{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Folderol.Typed (
    module Typed
  ) where

import Folderol.Typed.Name as Typed
import Folderol.Typed.Network as Typed

-- Processes shouldn't be necessary most of the time..
-- import Folderol.Typed.Process as Typed

