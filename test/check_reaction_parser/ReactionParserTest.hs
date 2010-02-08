{-----------------------------------------------------------------
 
  (c) 2009-2010 Markus Dittrich 
 
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

-- | this routine tests some aspects of the reaction block
-- parsing routines
module Main where


-- imports 
import Control.Monad.Writer
import qualified Data.Map as M
import Prelude
import System.Exit

-- local imports
import GenericModel
import PrettyPrint
import InputParser
import RpnData
import TestHelpers
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
type ExpectedOutput = Reaction



-- | set up the test data
-- Format : (parse expression, expected result)
type TestCase = (String, ExpectedOutput)



-- | simple tests without access to local variables
--- NOTE: for now we simply test if parsing succeeds
simpleReactParseTests :: [TestCase]
simpleReactParseTests = 
  [ ("x + y -> z | 1e6 |",
    Reaction (Constant 1e6) 
             [("y",id),("x",id)]
             [("x",-1),("y",-1),("z",1)])

  , ("x + y + z -> w | 10.3 |",
    Reaction (Constant 10.3)
             [("z",id),("y",id),("x",id)]
             [("x",-1),("y",-1),("z",-1),("w",1)])

  , ("x + y + z -> w | {x*y} |",
    Reaction (Function . RpnStack $ [Variable "x", Variable "y", 
              BinFunc (*)])
             [("z",id),("y",id),("x",id)]
             [("x",-1),("y",-1),("z",-1),("w",1)])

  , ("2x + 2y + z -> nil | { exp(-TIME) } |",
    Reaction (Function . RpnStack $ [Variable "TIME", Number (-1.0)
              , BinFunc (*), UnaFunc exp])
             [("z",id),("y",\a -> 0.5 * a * (a-1))
              ,("x",\a -> 0.5 * a * (a-1))]
             [("x",-2),("y",-2),("z",-1)])

  , ("2x + y + z -> 2x + 20y + 5z | {x+y+z } |",
    Reaction (Function . RpnStack $ [Variable "x", Variable "y", 
              BinFunc (+), Variable "z", BinFunc (+)])
             [("z",id),("y",id),("x",\a -> 0.5 * a * (a-1))]
             [("y",19),("z",4)])

  , ("2x + y + z -> 2x + 20y + 5z | { x*   y*  z } |",
    Reaction (Function . RpnStack $ [Variable "x", Variable "y", 
              BinFunc (*), Variable "z", BinFunc (*)])
             [("z",id),("y",id),("x",\a -> 0.5 * a * (a-1))]
             [("y",19),("z",4)])

  , ("2   x+ y+ z-> 2  x + 20 y + 5z|{x*y   *  z } |",
    Reaction (Function . RpnStack $ [Variable "x", Variable "y", 
              BinFunc (*), Variable "z", BinFunc (*)])
             [("z",id),("y",id),("x",\a -> 0.5 * a * (a-1))]
             [("y",19),("z",4)])

  , ("x + y + z -> x + y + z |{ x-x+y-y+z-z }|",
    Reaction (Function . RpnStack $ [Number 0.0])
             [("z",id),("y",id),("x",id)]
             [])

  , ("nil -> x + y + z |{ x-x+y-y+z-z }|",
    Reaction (Function . RpnStack $ [Number 0.0])
             []
             [("x",1),("y",1),("z",1)])

  , ("nil -> nil |{ sqrt(2.0) }  |",
 --   Reaction (Function . RpnStack $ [Number 2.0, UnaFunc sqrt])
      Reaction (Constant 1.4142135623730951)
             []
             [])
  ]



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
main = putStrLn "\n****** Testing Reaction Parser ******"

  -- check event parser
  >> (putStr $ color_string Cyan "\nReaction parse tests:\n")
  >> let reactParseOut = execWriter $ react_parser_test_driver 
           testMap_1 time_1 simpleReactParseTests
     in
  examine_output reactParseOut >>= \reactParseStatus ->


  -- evaluate status and return
  let status = reactParseStatus 
  in
    if status == True then
      exitWith ExitSuccess
    else
      exitWith $ ExitFailure 1 



-- the trigger and its actions action check out, i.e., we apply 
-- them to our current state and see if we get the proper number 
-- of molecules.
-- NOTE: We do not thread the state through all tests but always
-- start with our default, otherwise it becomes very tedious
-- to add/remove tests.
-- This parser should be run after the proper parsing has been
-- verified via event_parser_test_driver
react_parser_test_driver :: MoleculeMap -> Double -> [TestCase] 
            -> Writer [TestResult] ()
react_parser_test_driver _   _    []     = return ()
react_parser_test_driver mol t (x:xs) =

  let
    parseString = fst x
    expected    = snd x
  in

    -- parse expression
    case runParser parse_reaction testModelState "" parseString of
      Left er     -> tell [TestResult False parseString "" (show er)]
      Right react -> 
         
         case (react == expected) of

           False -> tell [TestResult False parseString "N/A" "N/A"]

           True  -> tell [TestResult True parseString "" "good parse"]
                      >> react_parser_test_driver mol t xs


