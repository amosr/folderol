{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
module Folderol.Untyped.Process where

import Folderol.Untyped.Name

import P

import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import           Data.String (String)

import qualified Folderol.Internal.Pretty as Pretty
import Folderol.Internal.Pretty (pretty, (<+>), (<#>))

import qualified Folderol.Internal.Haskell as Haskell

data Process
 = Process
 { pName          :: [Char]
 , pInputs        :: Set Channel
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

outlabels :: Instruction -> [Label]
outlabels = \case
 I'Pull _ _ (Next l _) (Next l' _)
  -> [l, l']
 I'Push _ _ (Next l _)
  -> [l]
 I'Jump (Next l _) 
  -> [l]
 I'Bool _ (Next l _) (Next l' _) 
  -> [l, l']
 I'Drop _ (Next l _) 
  -> [l]
 I'Done 
  -> []

instance Pretty.Pretty Process where
 pretty p@(Process name ins outs label instrs)
  = "process " <> Pretty.text name <#> 
  ( Pretty.indent 2
  $ Pretty.vsep
  [ "inputs:  " <> Pretty.set ins
  , "outputs: " <> Pretty.set outs
  , "initial: " <> prettyNext lns label
  , "instructions:" <#> pInstrs
  ])
  where 
   lns = processLabelShortNames p

   pInstrs
    = Pretty.indent 2
    $ Pretty.vsep
    $ fmap (\(k,v) -> prettyLabel lns k <+>
        "(" <> Pretty.text (show $ Haskell.ppr $ unLabel k) <> ")" <+>
        "=" <+> prettyInfo lns v)
    $ Map.toList instrs

prettyInfo :: Map Label String -> Info -> Pretty.Doc b
prettyInfo lns (Info bs i)
 | null bs
 = prettyInstruction lns i
 | otherwise
 = Pretty.indent 0 ("[" <> Pretty.set bs <> "]." <#> prettyInstruction lns i)

prettyInstruction :: Map Label String -> Instruction -> Pretty.Doc b
prettyInstruction lns = \case
  I'Pull c v n n'
   -> "pull" <+> pretty c <+> pretty v <+> prettyNext lns n <+> prettyNext lns n'
  I'Push c x n
   -> "push" <+> pretty c <+> "(" <> pretty x <> ")" <#> Pretty.indent 2 (prettyNext lns n)
  I'Jump n
   -> "jump" <+> prettyNext lns n
  I'Bool x n n'
   -> "bool" <+> "(" <> pretty x <> ")" <#> Pretty.indent 2 (prettyNext lns n <+> prettyNext lns n')
  I'Drop c n
   -> "drop" <+> pretty c <+> prettyNext lns n
  I'Done
   -> "done"

prettyNext :: Map Label String -> Next -> Pretty.Doc b
prettyNext lns (Next l u)
 | null u
 = prettyLabel lns l
 | otherwise
 = prettyLabel lns l <> Pretty.list (Pretty.mapEq u)

prettyLabel :: Map Label String -> Label -> Pretty.Doc b
prettyLabel lns l
 | Just s <- Map.lookup l lns = Pretty.text s
 | otherwise = "??"

processLabelShortNames :: Process -> Map Label String
processLabelShortNames (Process _ _ _ _ instrs) = names
 where
  padlen = length $ show $ length instrs
  names = Map.fromList $ List.zipWith namedLabel [0 :: Int ..] $ Map.keys instrs
  namedLabel i l = (l, Pretty.padl padlen ' ' ("L" <> show i))

