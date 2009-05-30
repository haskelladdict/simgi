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

-- | RpnCalc defines the data structures and a calculator engine
-- for computing mathematical expressions that have been parsed
-- into reverse polish notations
module RpnCalc ( rpn_compute ) where


-- imports 
import qualified Data.Map as M
import Prelude

-- local imports
import GenericModel
import RpnData


-- | computes an expressions based on an rpn stack
-- NOTE: This function expects the RPNstack to be sanitized
-- with respect to the variables, i.e., all variables in
-- the stack are assumed to exist in the MoleculeMap
rpn_compute :: MoleculeMap -> Double -> RpnStack -> Double
rpn_compute _      _    [(Number x)] = x
rpn_compute molMap theTime xs           = num 

  where
    (Number num) = head . foldl evaluate [] $ xs

    -- evaluate unary function (sin, cos, ..)
    evaluate ((Number x):ys) (UnaFunc f) = 
      (Number $ f x):ys

    -- evaluate binary function (*,+,..)
    evaluate ((Number x):(Number y):ys) (BinFunc f) =
      (Number $ f y x):ys

    -- extrace current time
    evaluate ys (Time) = (Number theTime):ys

    -- extract molecule variable
    evaluate ys (Variable x) = (Number $ replace_var x):ys
      where
        replace_var :: String -> Double
        replace_var = fromIntegral . (M.!) molMap

    evaluate ys item = item:ys
