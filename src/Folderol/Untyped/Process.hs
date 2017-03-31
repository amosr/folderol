{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Folderol.Untyped.Process where

import Folderol.Untyped.Name

import P

import Data.Map (Map)
import Data.Set (Set)

import qualified Folderol.Internal.Pretty as Pretty
import Folderol.Internal.Pretty (pretty, (<+>), (<#>))

import qualified Folderol.Internal.Haskell as Haskell

data Process
 = Process
 { pInputs        :: Set Channel
 , pOutputs       :: Set Channel
 , pInitial       :: Next
 , pInstructions  :: Map Label Info
 }

data Info
 = Info
 { infoBindings     :: Set Var
 , infoInstruction  :: Instruction
 }

data Instruction
 = I'Pull Channel Var Next Next
 | I'Push Channel Haskell.Exp Next
 | I'Jump Next
 | I'Bool Haskell.Exp Next Next
 | I'Drop Channel Next
 | I'Done

data Next
 = Next
 { nextLabel    :: Label
 , nextUpdates  :: Map Var Haskell.Exp
 }


instance Pretty.Pretty Process where
 pretty (Process ins outs label instrs)
  = "process: " <#> 
  ( Pretty.indent 2
  $ Pretty.vsep
  [ "inputs:  " <> Pretty.set ins
  , "outputs: " <> Pretty.set outs
  , "initial: " <> pretty label
  , "instructions:" <#>
    ( Pretty.indent 2
    $ Pretty.vsep 
    $ Pretty.mapEq instrs)
  ])

instance Pretty.Pretty Info where
 pretty (Info bs i)
  = "[" <> Pretty.set bs <> "]." <#> Pretty.indent 2 (pretty i)

instance Pretty.Pretty Instruction where
 pretty = \case
  I'Pull c v n n'
   -> "pull" <+> pretty c <+> pretty v <+> pretty n <+> pretty n'
  I'Push c x n
   -> "push" <+> pretty c <+> "(" <> pretty x <> ")" <+> pretty n
  I'Jump n
   -> "jump" <+> pretty n
  I'Bool x n n'
   -> "bool" <+> "(" <> pretty x <> ")" <+> pretty n <+> pretty n'
  I'Drop c n
   -> "drop" <+> pretty c <+> pretty n
  I'Done
   -> "done"
  

instance Pretty.Pretty Next where
 pretty (Next l u)
  = pretty l <> Pretty.list (Pretty.mapEq u)

