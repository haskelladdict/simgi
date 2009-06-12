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

-- | short helper script for averaging a file containing a single
-- column of doubles
module Main where

-- imports
import System
import System.Environment


main :: IO Int
main = getArgs 
       >>= \(name:value:tol:_) -> readFile name
       >>= \content -> 
         let
            items     = map (read) . lines $ content :: [Double]
            theSum    = foldr (+) 0 items
            average   = theSum/(fromIntegral . length $ items)
            expected  = read value :: Double
            tolerance = read tol :: Double
            deviation = abs((average - expected)/expected)
         in
           if ( deviation < tolerance )
             then exitWith ExitSuccess
             else exitWith $ ExitFailure 1
           
