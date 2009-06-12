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

-- | this routine tests some aspects of the input parsing
-- routines
module Main where


-- imports 
import Control.Monad.Writer
import qualified Data.Map as M
import Prelude
import System.Exit

-- local imports
import ExtraFunctions
import GenericModel
import PrettyPrint
import InputParser
--import RpnCac
import TokenParser


-----------------------------------------------------------------
-- 
-- test data specifications
--
-----------------------------------------------------------------

-- | set up the test data
-- Format : (parse expression, expected result)
type TestCase = (String, Double)



-- | simple tests without access to local variables
--- NOTE: for now we simply test if parsing succeeds
simpleEventParseTests :: [TestCase]
simpleEventParseTests = 
  [ ("{ c == 100 } => { a = 10 }", 0.0)
  , ("{ c== 100} => { a= 10}", 0.0)
  , ("{c==121 } => { a=10 }", 0.0) 
  , ("{c==100} => { a = 10 }", 0.0)
  , ("{ c == 100}=>{ a= 10}", 0.0)
  , ("{ c == 121 }=>{a=10}", 0.0) 
  , ("{ c <= 100} => { a= 10}", 0.0)
  , ("{ c >= 100} => { a= 10}", 0.0)
  , ("{ c < 100} => { a= 10}", 0.0)
  , ("{ c > 100} => { a= 10}", 0.0)
  , ("{ c == 121 } => { a=10 }", 0.0) 
  , ("{ c == 100 } => { a = 10; b = 20 }", 0.0)
  , ("{ c == 100 } => { a = 10; b = 20; }", 0.0)
  , ("{ c == 100 } => { a = 10; b = 20 }", 0.0)
  , ("{ c == 100 } => { a = 10; b = 20; }", 0.0)
  , ("{ c == 100 } => { a = 10*a*b; b = 20+a^2 }", 0.0)
  , ("{ c == 100 } => { a = 10*exp(-TIME); b = a; }", 0.0)
  , ("{ c == 100 } => { a = b^a; b = exp(-a*TIME) }", 0.0)
  , ("{ c == 100 } => { a = 10+c; b = sqrt(log((a^2))); }", 0.0)
  ]

{-
-- | variable tests
variableTests :: [TestCase]
variableTests =
  [ ("3*x", 3000)
  , ("x", 1000)
  , ("sqrt(x)^2", 1000)
  , ("x+y*z", 1000)
  , ("x + y *   z", 1000)
  , ("(x+y)*z", 0)
  , ("(x  + y)* z", 0)
  , ("exp(z)*TIME", 12.345)
  , ("exp(z ) *  TIME  ", 12.345)
  , ("-x * -y", 2.0e6)
  , ("x*exp(-TIME)", 4.351456244655325e-3)
  , ("x-x +x -x -y + y", 0.0)
  , ("-TIME/TIME + TIME - TIME + 1.0", 0.0)
  ]
-}

----------------------------------------------------------------
-- tests with access to local variables and time
----------------------------------------------------------------



----------------------------------------------------------------
-- Molecule maps and definitions for specific simulation times
----------------------------------------------------------------

-- | testmap containing a set of molecules and their 
-- concentrations
testMap_1 :: MoleculeMap
testMap_1 = M.fromList [("x",1000),("y",2000),("z",0)]

-- | a simulation time
time_1 :: Double
time_1 = 12.345




----------------------------------------------------------------
--
--  main driver routines
--
----------------------------------------------------------------


-- | main test driver
main :: IO ()
main = putStrLn "\n\n\nTesting Input Parser"

  -- run simple tests
  >> (putStr $ color_string Cyan "\nSimple parse tests:\n")
  >> let simpleOut = execWriter $ test_driver 
           testMap_1 time_1 simpleEventParseTests
     in
  examine_output simpleOut >>= \simpleStatus ->


  -- run variable tests
{-  (putStr $ color_string Cyan "\n\nVariable tests:\n")
  >> let varOut = execWriter $ test_driver testMap_1 time_1
                               variableTests
     in
  examine_output varOut >>= \varStatus ->
-}

  -- evaluate status and return
  let status = simpleStatus in
    if status == True then
      exitWith ExitSuccess
    else
      exitWith $ ExitFailure 1 


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


-- | driver for running a test routine that results in a
-- successful evaluation of a test expression
test_driver :: MoleculeMap -> Double -> [TestCase] 
            -> Writer [TestResult] ()
test_driver _ _ []          = return ()
test_driver mol time (x:xs) =

  let expr     = fst x
      expected = snd x
  in

    -- parse expression
    case runParser parse_events initialModelState "" expr of
      Left er -> tell [TestResult False expr (show expected) (show er)]
      Right stack -> 
        tell [TestResult True expr (show expected) ("good parse")]
        >> test_driver mol time xs
          

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

