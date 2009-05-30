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
module RpnData ( RpnItem(..)
               , RpnStack
               ) where


-- imports 
import Prelude


-- | RpnItem describes all items that can be present in our
-- rpn stack
data RpnItem = Time
               | Number Double 
               | Variable String
               | UnaFunc (Double -> Double) 
               | BinFunc (Double -> Double -> Double)

type RpnStack = [RpnItem]


instance Show RpnItem where
  show (Time)       = "TIME"
  show (Number x)   = show x
  show (BinFunc x)  = show (x 1 2) 
  show (UnaFunc x)  = show (x 1)
  show (Variable x) = x



