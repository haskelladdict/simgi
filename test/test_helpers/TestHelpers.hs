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
                   , testModelState
                   , TestResult(TestResult)
                   , defaultResult
                   ) where


-- imports 
import Control.Monad
import qualified Data.Map as M
import Prelude

-- local imports
import GenericModel
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
    examine_output_h acc (TestResult state tok targ act) = do
      if state == True then do
          putStr   $ color_string Blue "["
          putStr   $ color_string White "OK"
          putStr   $ color_string Blue  "] "
          putStr   $ color_string Green " Successfully evaluated "
          putStrLn $ color_string Yellow tok
          return $ acc && True
        else do
          putStr   $ color_string Blue "["
          putStr   $ color_string Red "TROUBLE"
          putStr   $ color_string Blue "] "
          putStr   $ color_string Green " Failed to evaluate "
          putStrLn $ color_string Yellow tok
          putStrLn $ color_string Green "\t\texpected : " 
                       ++ (show targ)
          putStrLn $ color_string Green "\t\tgot      : " 
                       ++ (show act)
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
defaultResult = TestResult { status = False 
                           , token  = ""
                           , target = ""
                           , actual = ""
                           }


-- | initial model state we use for our unit tests
-- we use a negative system volume so we don't have
-- to deal with unit conversion when testing the
-- reaction parser
testModelState :: ModelState
testModelState = ModelState { molCount    = M.empty
                            , rates       = []
                            , reactions   = []
                            , randNums    = []
                            , events      = []
                            , systemVol   = -1.0 -- negative!
                            , currentTime = 0.0
                            , currentIter = 0
                            , maxTime     = 0.0
                            , maxIter     = 10000
                            , outputFreq  = 1000
                            , outputCache = []
                            , outfileName = ""
                            }
