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

-- | this routine tests the functionality in our RPNStack, i.e.,
-- the infix to RPN parser as well as the compute engine
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
import RpnParser
import RpnCalc
import TestHelpers
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
simpleTests :: [TestCase]
simpleTests = 
  [ ("3+4", 7.0) 
  , ("3 +4", 7.0)
  , ("3+  4", 7.0)
  , ("3 +  4   ", 7.0)
  , ("-3 + 3   ", 0.0)
  , ("3*4+4", 16.0)
  , ("3 * 4+4 ", 16.0)
  , ("3+4*5", 23.0)
  , ("(3+4)*5", 35.0)
  , ("( 3 + 4    )*  5", 35.0)
  , ("sqrt(2)^2", 2.0)
  , ("(-2)^2", 4.0)
  , ("exp(-1)", 0.36787944117144233)
  , ("log(exp(3)) * 3^2", 27.0)
  , ("log (  exp ( 3)   ) *3   ^2", 27.0)
  , ("((((((((3*(3+(3*3) + 4) + 2) +3)-34) -4)+1))))", 16.0)
  , ("(1*(1 * (1 *(((((3*(3+(3*3) + 4) + 2) +3)-34) -4)+1))))", 16.0)
  , ("log(exp(2) - sqrt(2))", 1.7875577437560926)
  , ("2 * 3.14 * sqrt(2)", 8.88126117170303786)
  ]



----------------------------------------------------------------
-- tests with access to local variables and time
----------------------------------------------------------------

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
main = putStrLn "\n\n\nTesting RPN stack (parser/compute engine)"

  -- run simple tests
  >> (putStr $ color_string Cyan "\nSimple tests:\n")
  >> let simpleOut = execWriter $ test_driver 
           testMap_1 time_1 simpleTests
     in
  examine_output simpleOut >>= \simpleStatus ->


  -- run variable tests
  (putStr $ color_string Cyan "\n\nVariable tests:\n")
  >> let varOut = execWriter $ test_driver testMap_1 time_1
                               variableTests
     in
  examine_output varOut >>= \varStatus ->


  -- evaluate status and return
  let status = simpleStatus && varStatus in
    if status == True then
      exitWith ExitSuccess
    else
      exitWith $ ExitFailure 1 


-- | driver for running a test routine that results in a
-- successful evaluation of a test expression
test_driver :: MoleculeMap -> Double -> [TestCase] 
            -> Writer [TestResult] ()
test_driver _ _ []          = return ()
test_driver molMap t (x:xs) =

  let expr     = fst x
      expected = snd x
  in

    -- parse expression
    case runParser parse_infix_to_rpn testModelState "" expr of
      Left er -> tell [TestResult False expr (show expected) (show er)]
      Right stack ->

        -- evalute RPN stack
        let result = rpn_compute (SymbolTable molMap M.empty) t stack in
          examine_result expected result expr 
          >> test_driver molMap t xs

          where
            examine_result target out anExpr = 
              if is_equal target out
                then 
                  tell [TestResult True anExpr (show target) (show out)]
                else 
                  tell [TestResult False anExpr (show target) (show out)]
