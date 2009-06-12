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
  [ ("{ z == 100 } => { a = 10 }", (True, M.fromList [("a",10)]))
{-  , ("{ z== 100} => { a= 10}", 0.0)
  , ("{z==121 } => { a=10 }", 0.0) 
  , ("{a==100} => { a = 10 }", 0.0)
  , ("{ a == 100}=>{ a= 10}", 0.0)
  , ("{ b == 121 }=>{a=10}", 0.0) 
  , ("{ b <= 100} => { a= 10}", 0.0)
  , ("{ a >= 100} => { a= 10}", 0.0)
  , ("{ c < 100} => { a= 10}", 0.0)
  , ("{ a > 100} => { a= 10}", 0.0)
  , ("{ b == 121 } => { a=10 }", 0.0) 
  , ("{ c == 100 } => { a = 10; b = 20 }", 0.0)
  , ("{ b == 100 } => { a = 10; b = 20; }", 0.0)
  , ("{ a == 100 } => { a = 10; b = 20 }", 0.0)
  , ("{ a == 100 } => { a = 10; b = 20; }", 0.0)
  , ("{ b == 100 } => { a = 10*a*b; b = 20+a^2 }", 0.0)
  , ("{ c == 100 } => { a = 10*exp(-TIME); b = a; }", 0.0)
  , ("{ a == 100 } => { a = b^a; b = exp(-a*TIME) }", 0.0)
  , ("{ c == 100 } => { a = 10+c; b = sqrt(log((a^2))); }", 0.0) -}
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


  -- check event expressions
  (putStr $ color_string Cyan "\n\nEvent Expression tests:\n")
  >> let eventContentOut = execWriter $ event_content_test_driver 
           testMap_1 time_1 simpleEventParseTests
     in
  examine_output eventContentOut >>= \eventContentStatus ->


  -- evaluate status and return
  let status = eventParseStatus && eventContentStatus in
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
          


-- | this driver parses the event expression and checks their
-- content. This test really only makes sense if the parser can 
-- parse the expressions in the first place, hence run 
-- event_parser_test_driver first to catch all parse failures first.
event_content_test_driver :: MoleculeMap -> Double -> [TestCase] 
            -> Writer [TestResult] ()
event_content_test_driver _   _    []     = return ()
event_content_test_driver mol time (x:xs) =

  let expr            = fst x
      expectedTrigger = fst . snd $ x
      expectedCounts  = snd . snd $ x 
  in

    -- parse expression
    case runParser parse_events initialModelState "" expr of
      Left er     -> tell [TestResult False expr "" (show er)]
      Right event -> 
        
        -- make sure the trigger expression evaluated properly
        case check_trigger_expression mol expectedTrigger event of
          False -> tell [TestResult False expr "" ("bad parse")]
          True  -> tell [TestResult True expr "" ("good parse")]
                   >> event_parser_test_driver mol time xs
 


-- | given an Event and a MoleculeMap check that the trigger
-- evaluates to the expected value
check_trigger_expression :: MoleculeMap -> Bool -> Event
                         -> Bool
check_trigger_expression map want (Event { evtTrigger = trigger }) =
  True



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

