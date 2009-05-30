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

-- | main gsim driver
module Messages ( show_version 
                , startup_message
                , usage
                ) where

-- imports
import Prelude


-- | show version info
show_version :: IO ()
show_version = putStrLn "This is simgi v0.1 (C) 2009 Markus Dittrich"


-- | show a brief startup message
startup_message :: IO ()
startup_message = show_version 
  >> putStrLn "\nstarting simulation ..... here we go\n"



-- | provide brief usage info
usage :: IO ()
usage = putStrLn "Usage: simgi <input file>"
