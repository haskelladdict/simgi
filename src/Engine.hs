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

-- | the main compute Engine
module Engine ( create_initial_output
              , create_initial_state
              , gillespie_driver
              , module GenericModel
              ) where


-- imports
import Control.Monad.State
import qualified Data.Map as M
import Prelude
import Text.Printf
import System.IO


-- local imports
import ExtraFunctions
import GenericModel
import RpnCalc

--import Debug.Trace

-- | main simulation driver
-- the simulator either stops when
-- 1) the number of iterations is exhausted
-- 2) the current time is > t_max, if t_max is set to
--    zero t_max is treated as being infinity 
gillespie_driver :: Handle -> Double -> Integer -> ModelState -> IO ()
gillespie_driver handle simTime dmpIter state =  
  let (output, outState)  = runState run_gillespie $ state 
      (curTime, newState) = update_state dmpIter outState
  in
    (write_output handle . reverse $ output)
    >> if curTime >= simTime
         then return ()
         else gillespie_driver handle simTime dmpIter newState



-- | updates the state for the next iteration
update_state :: Integer -> ModelState -> (Double,ModelState)    
update_state dataDumpIter 
             state@(ModelState { currentTime = t 
                               , maxIter     = it
                               }) 
  = (t, state { maxIter = it + dataDumpIter, outputList = [] })



-- | actual compute loop
run_gillespie :: GillespieState [Output]
run_gillespie = get

  >>= \inState@(ModelState { molCount    = in_mols
                           , reactions   = in_reacts
                           , randNums    = (r1:r2:randRest)
                           , events      = molEvents
                           , currentTime = t
                           , currentIter = it
                           , maxTime     = t_max
                           , maxIter     = it_max
                           , outputFreq  = freq
                           , outputList  = output
                           }) ->


    -- compute and update the next state
    let out_rates  = compute_rates in_reacts in_mols t []
        a_0        = sum out_rates
        tau        = (-1.0/a_0) * log(r1)
        t_new      = t+tau
        mu         = get_mu (a_0*r2) out_rates
        out_mols   = adjust_molcount in_mols in_reacts mu
        evt_mols   = handle_events molEvents out_mols t_new
        new_output = generate_output freq it t_new out_mols output
        newState   = inState { molCount    = evt_mols
                             , rates       = out_rates
                             , randNums    = randRest
                             , currentTime = t_new
                             , currentIter = it+1
                             , outputList  = new_output
                             }
    in

    -- this prevents simulation from getting stuck
    -- FIXME: We need to come up with mechanism to propagate
    -- error message corresponding to cases such as this one 
    -- to the user!
    if (is_equal tau 0.0)
      then let finalState = newState { currentTime = t_max } in
           put finalState >> return output
      else 
        if ( it_max == it || t >= t_max )
          then return output
          else put newState >> run_gillespie



-- | handle all user defined events and return the adjusted
-- number of molecules
-- WARNING: We should probably the Event Stack before we use
-- it to compute stuff; at least make sure molecule exist
handle_events :: [Event] -> MoleculeMap -> Double -> MoleculeMap
handle_events [] molMap     _ = molMap
handle_events (x:xs) molMap t = 
  let
    newMolMap = handle_single_event x molMap t
  in
    handle_events xs newMolMap t


-- | handle a single user event
handle_single_event :: Event -> MoleculeMap -> Double -> MoleculeMap
handle_single_event evt molMap t =
  let
    trigger    = evtTrigger evt
    actions    = evtActions evt
    triggerVal = rpn_compute molMap t trigger
  in 
    if is_equal triggerVal 0.0 
      then execute_actions actions molMap t 
      else molMap



-- | handle all actions associated with a user event
execute_actions :: [EventAction] -> MoleculeMap -> Double 
                -> MoleculeMap
execute_actions [] molMap _     = molMap
execute_actions (x:xs) molMap t =
  let
    newMolMap = execute_single_action x molMap t
  in
    execute_actions xs newMolMap t



-- | handle a single event triggered action
execute_single_action :: EventAction -> MoleculeMap -> Double
                      -> MoleculeMap
execute_single_action eventAction molMap t =
  let
    name   = evtName eventAction
    action = evtAct eventAction
  in
    case action of
      Constant c   -> adjust_mol_count name (to_int c) molMap

      Function rpn -> let
                        newCount = to_int $ rpn_compute molMap t rpn
                      in
                        adjust_mol_count name newCount molMap
  
      where
        -- NOTE: presently, converting double -> int is done
        -- via floor. Is this a good policy (once documented
        -- properly)?
        to_int :: Double -> Int
        to_int = floor 



-- | adjust the count of a certain molecule to a new value
adjust_mol_count :: String -> Int -> MoleculeMap -> MoleculeMap
adjust_mol_count key val molMap = M.insert key val molMap



-- | generate a new Output data structure based on the current
-- molecule counts
generate_output :: Integer -> Integer -> Double -> MoleculeMap 
                -> [Output] -> [Output]
generate_output afreq it t amol outlist  
  | mod it afreq /= 0  = outlist
  | otherwise        = new_out:outlist

    where
      new_out = Output { iteration = it
                       , time      = t
                       , mols      = amol
                       }



-- | depending on which reaction happened adjust the number of 
-- molecules in the system
adjust_molcount :: MoleculeMap -> [Reaction] -> Int -> MoleculeMap
adjust_molcount theMap rs mID =

  let (Reaction { react = react_in }) = rs !! mID
  in
    adjustMap react_in theMap 

  where
    adjustMap :: [(String,Int)] -> MoleculeMap -> MoleculeMap
    adjustMap [] m = m
    adjustMap ((k,a):rands) m = let val   = (M.!) m k
                                    m_new = M.insert k (a+val) m
                                in
                                  adjustMap rands m_new



-- | pick the \mu value for the randomly selected next reaction 
-- reaction to happen
get_mu :: Double -> [Double] -> Int
get_mu val = length . takeWhile ( <val ) . scanl1 (+) 



-- | compute the current value for the reaction probabilities based 
-- on the number of molecules and reaction rates
compute_rates :: [Reaction] -> MoleculeMap -> Double 
              -> RateList -> RateList
compute_rates [] _ _ rts = reverse rts
compute_rates ((Reaction {rate = c_in, aList = a_in }):rs) 
  theMap theTime rts = 
  
  case c_in of
    (Constant aRate)    -> compute_rates rs theMap theTime
       ((a_new aRate): rts)
    (Function rateFunc) -> compute_rates rs theMap theTime
       ((a_new . (rpn_compute theMap theTime) $ rateFunc):rts)
 
  where
    mult  = product $ map (\(a,f) -> f . fromIntegral $ 
            (M.!) theMap a) a_in 
    a_new = (*) mult 



-- | initialize the output data structure
create_initial_output :: ModelState -> Output
create_initial_output (ModelState { molCount = initialMols }) = 
  
  Output { iteration = 1
         , time      = 0.0
         , mols      = initialMols
         }



-- | set up the initial state
create_initial_state:: ModelState -> [Double] -> Output -> ModelState
create_initial_state state rand output = 

  state { rates       = defaultRateList
        , randNums    = rand
        , currentTime = 0.0
        , currentIter = 1
        , outputList  = [output]
        }



-- | basic routine writing the simulation output to stdout
write_output :: Handle -> [Output] -> IO ()
write_output _ [] = return ()
write_output handle ((Output {iteration = it, time = t, mols = m}):xs) = 
  let header = (printf "%-10d %18.15g" it t) :: String
      counts = create_count_string m
  in
    -- write molecule data to output file
    hPutStrLn handle (header ++ counts)

    -- write current iteration to stdout
    >> (putStrLn $ "iteration | time   --->   " ++ header)
    >> write_output handle xs

  where
    create_count_string :: MoleculeMap -> String
    create_count_string = foldr (\x a -> (printf "%10d " x) ++ a) 
                          "" . M.elems 
