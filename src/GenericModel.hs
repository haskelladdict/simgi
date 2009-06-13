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

-- | data structures needed for defining a stochastic model
module GenericModel ( defaultRateList
                    , Event(..)
                    , EventAction(..)
                    , EventTrigger(..)
                    , GillespieState
                    , initialModelState
                    , MathExpr(..)
                    , ModelState(..)
                    , MoleculeMap
                    , Output(..)
                    , Rate
                    , RateList
                    , Reaction(..)
                    ) where


-- imports
import Control.Monad.State
import qualified Data.Map as M
import Prelude


-- local imports
import RpnData


-- | A MoleculeMap keeps track of the current number of molecules
type MoleculeMap = M.Map String Int



-- | generic data type for a mathematical expression. This could
-- either be a constant or an expression inside an RpnStack
data MathExpr = Constant Double | Function RpnStack



-- | data type for reaction rates which are of type MathExpr
type Rate = MathExpr



-- | List of reactions and corresponding rates
type RateList    = [Double]

defaultRateList :: RateList
defaultRateList = [] 



-- | for each elementary reaction i we need to keep track of
--   
--   rate: the reaction rate c_i or rate function 
--   
--   aList: description of which molecular species are participating
--     in a reaction (needed for computing h_mu in Gillespie's 
--     notation) and a function mapping a molecule count to the
--     proper h_mu value (needed e.g. for cases where we have
--     2X terms where h_mu would be 0.5*X*(X-1).
--   
--   react: a list of tuples (i,j) describing that the reaction
--     changes the count of molecule i by j 
--
data Reaction = Reaction { rate       :: Rate
                         , actors     :: [(String,Double -> Double)]
                         , reaction   :: [(String,Int)]
                         }


-- | make Reaction an instance of Eq so we can compare them
-- (used in our unit tests)
instance Eq Reaction where

  x == y  =  True {-compare_reactions x y

  where
    compare_reactions :: Reaction -> Reaction -> Bool
    compare_reactions 
      (Reaction { rate = rate1, aList = aList1, react = react1 })
      (Reaction { rate = rate2, aList = aList2, react = react2 }) =

      let
        rateComp = compare_rate rate1 rate2
        aListComp = compare_alist 

-}



-- | data type describing an action triggered during an event
-- It consists of a String tracking the molecule affected
-- as well as a mathematic expression describing the new molecule
-- count for this molecule
data EventAction = EventAction { evtName   :: String
                               , evtAct    :: MathExpr
                               }



-- | data type describing an expression that triggers a 
-- user event
data EventTrigger = EventTrigger 
  { trigLeftExpr  :: RpnStack
  , trigRelation  :: Double -> Double -> Bool
  , trigRightExpr :: RpnStack
  }



-- | data type keeping track of possible events occuring during
-- the simulation. Each event consist of a
--
--   <trigger>: numerical expression that triggers the event
--     when equal to 0 
-- 
--   <action>: an expression of the form 
--
--              mol = <numerical expression>
--
-- changing the number of mol by <numerical expression>
--
data Event = Event { evtTrigger :: EventTrigger
                   , evtActions :: [EventAction]
                   }



-- | Our model state
data ModelState = ModelState { molCount    :: MoleculeMap
                             , rates       :: RateList
                             , reactions   :: [Reaction]
                             , randNums    :: [Double]
                             , events      :: [Event]
                             , systemVol   :: Double
                             , currentTime :: Double
                             , currentIter :: Integer
                             , maxTime     :: Double
                             , maxIter     :: Integer
                             , outputFreq  :: Integer
                             , outputList  :: [Output]
                             , outfileName :: String
                             }

type GillespieState a = State ModelState a



-- | data structure for keeping track of our output
data Output = Output { iteration :: Integer
                     , time      :: Double
                     , mols      :: MoleculeMap 
                     }
  deriving(Show)



-- | initial model state to be partially filled by the 
-- parser from the input deck
initialModelState :: ModelState
initialModelState = ModelState { molCount    = M.empty
                               , rates       = []
                               , reactions   = []
                               , randNums    = []
                               , events      = []
                               , systemVol   = 1.0
                               , currentTime = 0.0
                               , currentIter = 0
                               , maxTime     = 0.0
                               , maxIter     = 10000
                               , outputFreq  = 1000
                               , outputList  = []
                               , outfileName = ""
                               }


