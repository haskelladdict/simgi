{-----------------------------------------------------------------
 
  (c) 2009 Markus Dittrich 
 
  This program is free software; you can redistribute it 
  and/or modify it under the terms of the GNU General Public 
  License Version 3 as published by the Free Software Foundation. 
 
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License Version 3 for more details.
 
  You should have received a copy of the GNU General Public 
  License along with this program; if not, write to the Free 
  Software Foundation, Inc., 59 Temple Place - Suite 330, 
  Boston, MA 02111-1307, USA.

--------------------------------------------------------------------}

-- | module responsible for doing some basic input checking
module InputCheck ( check_input ) where

-- imports
import Control.Monad.Error()
import qualified Data.List as L
import qualified Data.Map as M
import Prelude

-- local imports
import GenericModel
import RpnData


-- | small function checking for common errors in the input deck
-- since checking is currently fairly simple we'll do it by hand
-- should this ever become more extensive we should probably consider
-- using Control.Monad.Error
check_input :: ModelState -> Either String Bool 
check_input (ModelState { molCount    = theMols
                        , reactions   = theReactions
                        , maxIter     = iterCount
                        , outputFreq  = outFreq
                        , outfileName = fileName
                        }) 
  = check_molecules (defined_mols theMols) 
      (react_mols theReactions)
    >> check_positive_outfreq outFreq
    >> check_positive_itercount iterCount
    >> check_filename fileName
    >> check_reaction_rate_functions (defined_mols theMols) 
         (rate_mols theReactions)

 where
  -- | extract all reaction participants
  react_mols = L.nub . L.concat . map (map (fst) . react) 


  -- | extract all definied molecules
  defined_mols = M.keys 


  -- | extract all molecules appearing in reaction rate
  -- functions
  rate_mols theRates = 
    let
      stacks    = foldr extract_rate_func [] . map rate $ theRates
    in
      L.nub . concat . map (foldr extract_rate_vars []) $ stacks

      where
        extract_rate_func (Function a) acc = a:acc
        extract_rate_func _            acc = acc

        extract_rate_vars (Variable a) acc = a:acc
        extract_rate_vars _            acc = acc


-- | make sure the user specified an output file name
check_filename :: String -> Either String Bool
check_filename name 
  | name == ""  = Left "Error: Please specify an output file name!"
  | otherwise   = Right True


-- | make sure all molecules in reactions are defined
check_molecules :: [String] -> [String] -> Either String Bool
check_molecules defMols reactMols = 
  let 
    not_present = reactMols L.\\ defMols
  in
    case null not_present of
      True  -> Right True
      False -> Left $ 
        "Error: The following molecules are not definied: " 
        ++ (L.concat $ L.intersperse "," not_present)


-- | make sure the user entered a positive integer for outputFreq
check_positive_outfreq :: Integer -> Either String Bool
check_positive_outfreq freq = 
  if freq < 0 
    then Left "Error: outputFreq must be a positive integer!"
    else Right True


-- | make sure the user entered a positive integer for outputFreq
check_positive_itercount :: Integer -> Either String Bool
check_positive_itercount iter = 
  if iter < 0 
    then Left "Error: outputIter must be a positive integer!"
    else Right True


-- | make sure the user defined reaction rate function reference
-- only existing molecule names
check_reaction_rate_functions :: [String] -> [String] 
                              -> Either String Bool
check_reaction_rate_functions defMols rateMols =
  let 
    no_mol = rateMols L.\\ defMols
  in
    case null no_mol of
      True  -> Right True
      False -> Left $
        "Error: The following molecules defined in reaction "
        ++ "rates do not exist: " 
        ++ (L.concat $ L.intersperse "," no_mol)
