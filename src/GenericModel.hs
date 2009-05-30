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
                    , GillespieState
                    , initialModelState
                    , ModelState(..)
                    , MoleculeMap
                    , Output(..)
                    , Rate(..)
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


-- | data type for reaction rates which can either be a Double
-- or an RpnStack describing a function to compute the rate
-- at run time
data Rate = Constant Double | Function RpnStack

-- | List of reactions and corresponding rates
type RateList    = [Double]

defaultRateList :: RateList
defaultRateList = [] 


-- | for each elementary reaction i we need to provide 
--   1) the reaction rate c_i or rate function 
--   2) the reaction order (first, second, ...)
--   2) aList describing which molecular species are participating
--      in a reaction (needed for computing h_mu in Gillespie's 
--      notation) and a function mapping a molecule count to the
--      proper h_mu value (needed e.g. for cases where we have
--      2X terms where h_my would be 0.5*X*(X-1).
--   3) a list of tuple (i,j) describing that the count of molecule
--      i changes by j should this reaction take place
data Reaction = Reaction { rate       :: Rate
                         , aList      :: [(String,Double -> Double)]
                         , react      :: [(String,Int)]
                         }


-- | Our model state
data ModelState = ModelState { molCount    :: MoleculeMap
                             , rates       :: RateList
                             , reactions   :: [Reaction]
                             , randNums    :: [Double]
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
                               , systemVol   = 1.0
                               , currentTime = 0.0
                               , currentIter = 0
                               , maxTime     = 0.0
                               , maxIter     = 10000
                               , outputFreq  = 1000
                               , outputList  = []
                               , outfileName = ""
                               }


