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
import Engine
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

-- | expected values for parser events. The first
-- entry in the tuple is of type Bool giving the expected
-- value of the evaluated trigger expression. The second
-- is a Map of expected molecule counts after the actions
-- have been evaluated
type ExpectedOutput = (Bool, M.Map String Int)



-- | set up the test data
-- Format : (parse expression, expected result)
type TestCase = (String, ExpectedOutput)



-- | simple tests without access to local variables
--- NOTE: for now we simply test if parsing succeeds
simpleEventParseTests :: [TestCase]
simpleEventParseTests = 
  [ ("{ z == 100 } => { x = x }", 
      (True, M.fromList [("x",1000),("y",2000),("z",100)]))

  , ("{ z== 100} => { x = x+y; y = y+1 }", 
      (True, M.fromList [("x",3000),("y",2001),("z",100)]))

  , ("{z==121 } => { y=10 }", 
      (False, M.fromList [("x",1000),("y",10),("z",100)]))

  , ("{z==101} => { x = x/2.0 }", 
      (False, M.fromList [("x",500),("y",2000),("z",100)]))

  , ("{ z == 100}=>{ z= 121; x = 500; y=2000;}", 
      (True, M.fromList [("x",500),("y",2000),("z",121)]))

  , ("{ y >= 121 }=>{z=1050}", 
      (True, M.fromList [("x",1000),("y",2000),("z",1050)]))

  , ("{ z <= 100} => { x= sqrt(4)*100; y = x^2/10}", 
      (True, M.fromList [("x",200),("y",4000),("z",100)]))

  , ("{ x >= 100} => { z= z; z = 2*z; z = 3*z;}", 
      (True, M.fromList [("x",1000),("y",2000),("z",600)]))

  , ("{ z < 100} => { x= 10; y = z}", 
      (False, M.fromList [("x",10),("y",100),("z",100)]))

  , ("{ x > 100} => { y= x+z}", 
      (True, M.fromList [("x",1000),("y",1100),("z",100)]))

  , ("{ y == 2000 } => { x=1001 }", 
      (True, M.fromList [("x",1001),("y",2000),("z",100)]))

  , ("{ z <= 302 } => { x = 10; y = 10; z= 200; z=10 }", 
      (True, M.fromList [("x",10),("y",10),("z",10)]))

  , ("{ y == 100 } => { x = 10; y = 20; }", 
      (False, M.fromList [("x",10),("y",20),("z",100)]))

  , ("{ z == 100 } => { x = 10; y = 20 }", 
      (True, M.fromList [("x",10),("y",20),("z",100)]))

  , ("{ z == 100 } => { x = 10; y = 20 +      z }", 
      (True, M.fromList [("x",10),("y",120),("z",100)]))

  , ("{ x > 100 } => { x = 10*x+y; y = 20+z^2 }", 
      (True, M.fromList [("x",12000),("y",10020),("z",100)]))

  , ("{ z == 100 } => { x = 1e6*exp(-TIME); y = x; }",
      (True, M.fromList [("x",4),("y",4),("z",100)]))

  , ("{ y == 100 } => { x = 2; x = y^x; y = x*exp(-TIME) }", 
      (False, M.fromList [("x",4000000),("y",17),("z",100)]))

  , ("{ z == 100 } => { x = 10+z; y = sqrt(log((x^2))); }",
      (True, M.fromList [("x",110),("y",3),("z",100)])) 
  ]


----------------------------------------------------------------
-- tests with access to local variables and time
----------------------------------------------------------------



----------------------------------------------------------------
-- Molecule maps and definitions for specific simulation times
----------------------------------------------------------------

-- | testmap containing a set of molecules and their 
-- concentrations
testMap_1 :: MoleculeMap
testMap_1 = M.fromList [("x",1000),("y",2000),("z",100)]

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

  -- check event parser
  >> (putStr $ color_string Cyan "\nEvent parse tests:\n")
  >> let eventParseOut = execWriter $ event_parser_test_driver 
           testMap_1 time_1 simpleEventParseTests
     in
  examine_output eventParseOut >>= \eventParseStatus ->


  -- check event triggers and actions
  (putStr $ color_string Cyan "\n\nEvent trigger/action tests:\n")
  >> let eventActionOut = execWriter $ event_action_test_driver 
           testMap_1 time_1 simpleEventParseTests
     in
  examine_output eventActionOut >>= \eventActionStatus ->


  -- evaluate status and return
  let status = eventParseStatus && eventActionStatus 
  in
    if status == True then
      exitWith ExitSuccess
    else
      exitWith $ ExitFailure 1 



-- | this driver parses the event expression and checks that
-- there are no errors (there shouldn't be any)
event_parser_test_driver :: MoleculeMap -> Double -> [TestCase] 
            -> Writer [TestResult] ()
event_parser_test_driver _   _    []     = return ()
event_parser_test_driver mol time (x:xs) =

  let expr     = fst x
      expected = snd x
  in

    -- parse expression
    case runParser parse_events initialModelState "" expr of
      Left er -> tell [TestResult False expr (show expected) (show er)]
      Right stack -> 
        tell [TestResult True expr (show expected) ("good parse")]
        >> event_parser_test_driver mol time xs
          


-- | this driver parses the event expression and checks if
-- the trigger and its actions action check out, i.e., we apply 
-- them to our current state and see if we get the proper number 
-- of molecules.
-- NOTE: We do not thread the state through all tests but always
-- start with our default, otherwise it becomes very tedious
-- to add/remove tests.
-- This parser should be run after the proper parsing has been
-- verified via event_parser_test_driver
event_action_test_driver :: MoleculeMap -> Double -> [TestCase] 
            -> Writer [TestResult] ()
event_action_test_driver _   _    []     = return ()
event_action_test_driver mol time (x:xs) =

  let expr            = fst x
      expectedTrigger = fst . snd $ x
      expectedMols    = snd . snd $ x 
  in

    -- parse expression
    case runParser parse_events initialModelState "" expr of
      Left er     -> tell [TestResult False expr "" (show er)]
      Right event -> 
        
        -- make sure the trigger expression evaluated properly
        let
          actions    = evtActions event
          outMols    = execute_actions actions mol time 
          outTrigger = check_trigger mol time expectedTrigger event 
        in
          case outTrigger && (outMols == expectedMols) of

            False -> tell [TestResult False expr 
              (show expectedTrigger ++ " => " ++ show expectedMols)
              (show outTrigger ++ " => " ++ show outMols)]

            True  -> tell [TestResult True expr "" ("good parse")]
                       >> event_action_test_driver mol time xs



-- | given an Event and a MoleculeMap check that the trigger
-- evaluates to the expected value
check_trigger :: MoleculeMap -> Double -> Bool -> Event
                         -> Bool
check_trigger map t expected (Event { evtTrigger = trigger }) =
  computed == expected
  
  where
    computed = compute_trigger map t trigger


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

