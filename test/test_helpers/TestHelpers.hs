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

-- | this module provides a few common routines used in our
-- unit tests
module TestHelpers ( examine_output
                   , TestResult(TestResult)
                   ) where


-- imports 
import Control.Monad
import Prelude

-- local imports
import PrettyPrint



-- | examine the output of a test routine
-- | helper function for examining the output of a good test run
-- (i.e. one that should succeed), prints out the result for each 
-- test, collects the number of successes/failures and returns 
-- True in case all tests succeeded and False otherwise
examine_output :: [TestResult] -> IO Bool
examine_output = foldM examine_output_h True
                 
  where
    examine_output_h :: Bool -> TestResult -> IO Bool
    examine_output_h acc (TestResult status token target actual) = do
      if status == True then do
          putStr   $ color_string Blue "["
          putStr   $ color_string White "OK"
          putStr   $ color_string Blue  "] "
          putStr   $ color_string Green " Successfully evaluated "
          putStrLn $ color_string Yellow token
          return $ acc && True
        else do
          putStr   $ color_string Blue "["
          putStr   $ color_string Red "TROUBLE"
          putStr   $ color_string Blue "] "
          putStr   $ color_string Green " Failed to evaluate "
          putStrLn $ color_string Yellow token
          putStrLn $ color_string Green "\t\texpected : " 
                       ++ (show target)
          putStrLn $ color_string Green "\t\tgot      : " 
                       ++ (show actual)
          return False



-- | data structure for keeping track of our test results
-- which consist of a bool indicating success
-- or failure, the test token as well as the expected and
-- received result
data TestResult = TestResult { status :: Bool
                             , token  :: String
                             , target :: String
                             , actual :: String
                             }


defaultResult :: TestResult
defaultResult = TestResult False "" "" ""

