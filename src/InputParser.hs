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

-- | input file parser 
module InputParser ( input_parser ) where

-- imports
import Control.Monad
import qualified Data.Map as M
import Prelude
import TokenParser


-- local imports
import ExtraFunctions
import GenericModel
import RpnData
import RpnParser

--import Debug.Trace


-- | main parser entry point
input_parser :: CharParser ModelState ModelState
input_parser = whiteSpace 
               *> many ( choice [ try parse_parameter_def
                                , try parse_molecule_def
                                , try parse_reaction_def
                                , try parse_event_def
                                ])
               *> eof
               >> getState
            <?> "main parser"


-- | parser for event definitions
parse_event_def :: CharParser ModelState ()
parse_event_def = join ( updateState <$> insert_events <$>
  parse_def_block "events" (parse_events `sepBy` whiteSpace) )
               <?> "event definitions" 

  where
    insert_events :: [Event] -> ModelState -> ModelState
    insert_events newEvents state = state { events = newEvents }



-- | parser for individual events
parse_events :: CharParser ModelState Event
parse_events = Event <$> (parse_trigger <* whiteSpace) 
                <*> (reservedOp "=>" *> parse_actions)
            <?> "reaction event"



-- | parser for an event trigger
parse_trigger :: CharParser ModelState EventTrigger
parse_trigger = braces parse_trigger_expression 
             <?> "event trigger block"



-- | parse a trigger expression
parse_trigger_expression :: CharParser ModelState EventTrigger
parse_trigger_expression = 
  EventTrigger <$> parse_infix_to_rpn <*> parse_relational
               <*> parse_infix_to_rpn
                        <?> "event trigger expression"



-- | parse a relational expression and return its associated
-- binary function
parse_relational :: CharParser ModelState (Double -> Double -> Bool)
parse_relational =  try ( reserved ">=" >> pure (>=) )
                <|> try ( reserved "<=" >> pure (<=) )
                <|> try ( reserved "==" >> pure (==) )
                <|> ( reserved ">" >> pure (>) )
                <|> ( reserved "<" >> pure (<) )
                <?> "relational expression"



-- | parser for an event action
parse_actions :: CharParser ModelState [EventAction]
parse_actions = braces parse_action_expressions
            <?> "event action block"



-- | parser for a list of action expressions
parse_action_expressions :: CharParser ModelState [EventAction]
parse_action_expressions = 
  parse_single_action_expression `sepEndBy` semi 
                       <?> "event action"


-- | parser for a single event action expression
parse_single_action_expression :: CharParser ModelState EventAction
parse_single_action_expression = EventAction <$> 
  (molname <* whiteSpace) <*> (reservedOp "=" *> parse_expression)
                              <?> "event action expression"



-- | parser for simulation parameters
parse_parameter_def :: CharParser ModelState ()
parse_parameter_def = parse_def_block "parameters" 
                        (parse_parameters `sepBy` whiteSpace)
                      *> pure ()
                   <?> "parameter definitions"



-- | parse the individual parameters
parse_parameters :: CharParser ModelState ()
parse_parameters = parse_time
                <|> parse_outputFile
                <|> parse_outputIter
                <|> parse_outputFreq
                <|> parse_systemVol
                <?> "time, outputIter, systemVol, outputFreq,\
                    \outputFile"



-- | parse the simulation time specs
parse_time :: CharParser ModelState ()
parse_time = join (updateState <$> insert_time 
                   <$> (reserved "time" *> reservedOp "=" 
                        *> parse_number))
  
  where
    insert_time t state = state { maxTime = t }



-- | parse the value of the simulated system volume
parse_systemVol :: CharParser ModelState ()
parse_systemVol = join (updateState <$> insert_volume
                       <$> (reserved "systemVol" *> reservedOp "="
                           *> (parse_positive_number
                             <|> parse_systemVol_nil )))
               <?> "system volume"

  where
    -- needed to avoid monomorphism warning
    parse_systemVol_nil :: CharParser ModelState Double
    parse_systemVol_nil = reserved "nil" *> pure (-1.0)

    insert_volume vol state = state { systemVol = vol }



-- | parse the name of the output file 
-- accepts paths but will NOT create any of the parents
parse_outputFile :: CharParser ModelState ()
parse_outputFile = join (updateState <$> insert_filename
                     <$> (reserved "outputFile" *> reservedOp "="
                          *> parse_filename ))

  where
    insert_filename name state = state { outfileName = name }



-- | parse a filename
parse_filename :: CharParser ModelState String
parse_filename = stringLiteral




-- | parse the output iteration specification if present
parse_outputIter :: CharParser ModelState ()
parse_outputIter = join (updateState <$> insert_outputIter
                        <$> (reserved "outputIter" *> reservedOp "="
                             *> integer ))

  where
    insert_outputIter i state = state { maxIter = i }



-- | parse the output iteration specification if present
parse_outputFreq :: CharParser ModelState ()
parse_outputFreq = join (updateState <$> insert_outputFreq
                        <$> (reserved "outputFreq" *> reservedOp "="
                             *> integer ))

  where
    insert_outputFreq i state = state { outputFreq = i }



-- | parser for molecule definitions
parse_molecule_def :: CharParser ModelState ()
parse_molecule_def = join ( updateState <$> insert_molecules <$> 
  parse_def_block "molecules" (parse_molecules `sepBy` whiteSpace) )
                  <?> "molecule definitions"

  where
    insert_molecules :: [(String, Int)] -> ModelState -> ModelState
    insert_molecules theMols state = 
      state { molCount = M.fromList theMols }



-- | parse a molecule name and the number of molecules of this type
parse_molecules :: CharParser ModelState (String,Int)
parse_molecules = make_molecule <$> (try molname) <*> integer
  where
    make_molecule mol aCount = (mol,fromInteger aCount)



-- | parser for a molecule name 
-- A molecule name can consist of letters and numbers but has to 
-- start with a letter. The following keywords are reserved
molname :: CharParser ModelState String
molname = not_end ((:) <$> letter <*> many (alphaNum <?> ""))
        <?> "molecule name" 



-- | short checker making sure we don't scan beyond the "end" statement
-- of a block 
not_end :: CharParser ModelState String -> CharParser ModelState String
not_end p = p >>= \name -> case name /= "end" of
                             True  -> pure name
                             False -> pzero



-- | parser for reaction definitions
parse_reaction_def :: CharParser ModelState ()
parse_reaction_def = join ( updateState <$> insert_reactions <$>
  parse_def_block "reactions" (parse_reaction `sepBy` whiteSpace) )
                  <?> "reaction definitions"
  
  where
    insert_reactions :: [Reaction] -> ModelState -> ModelState
    insert_reactions reacts state = state { reactions = reacts }



-- | parser for a single reaction specification of the type
-- aA + bB + cC + .... -> n1P1 + n2P2 + ......   : rate :
-- NOTE: In order to convert the reaction rates (if requested
--       by the user) we also need to extract the system
--       volume)
parse_reaction :: CharParser ModelState Reaction
parse_reaction = setup_reaction 
                 <$> (parse_react_prod <* reservedOp "->") 
                 <*> parse_react_prod 
                 <*> parse_rate
                 <*> (getState 
                      >>= \(ModelState {systemVol = vol}) -> pure vol)  
  where
    -- | set up a Reaction data structure from the parsed reaction
    setup_reaction r p cin vol = 
      let 
        action  = create_react r p
        hFactor = create_hFact r 
        theRate = if (vol < 0.0) -- no rate conversion for 
                    then cin     -- systemVol = nil
                    else convert_rate cin (M.size r) vol
      in 
        Reaction { rate       = theRate
                 , aList      = hFactor
                 , react      = action
                 }


    -- | convert reaction propensities into rates if requested
    -- by the user. For constants we simply multiply, for
    -- rate functions we push the neccessary conversion onto
    -- the stack
    convert_rate theConst@(Constant c) order volume =
      case order of
        1 -> theConst
        _ -> Constant $ c/(avogadroNum * volume^(order-1))

    convert_rate theFunc@(Function stack) order volume =
      case order of
        1 -> theFunc
        _ -> let mult = 1.0/(avogadroNum * volume^(order-1)) in
               Function $ stack ++ [Number mult,BinFunc (*)]



    -- | create the list holding the molecule number changes for 
    -- this reaction
    create_react r p = let 
                         reacts = M.map (*(-1)) r 
                       in
                         M.assocs $ M.unionWith (+) reacts p


    -- | create the list containing the h factors
    -- WARNING/FIXME: Currently, things are ill defined if the number 
    -- of molecules for species A is below the stoichiometric reactant
    -- coefficient; i.e. if #A = 2 then 3A -> ... does not make sense
    create_hFact :: (M.Map String Int) -> [(String, Double -> Double)]
    create_hFact     = create_hFact_h [] . M.assocs  
    
      where
        create_hFact_h acc [] = acc
        create_hFact_h acc ((k,v):xs) = 
          let 
            v_int = fromIntegral v :: Double
          in
            create_hFact_h ((k,\x -> (1.0/v_int) 
              * generate_lambda v_int x):acc) xs

            where
              generate_lambda :: Double -> Double -> Double
              generate_lambda 1 x   = x
              generate_lambda n x   = (x-n+1) * generate_lambda (n-1) x   


-- | parse rate parses a reaction rate. This can either be a simple
-- constant of a full blown infix math expression.
-- Reaction rates must be enclosed by colons ":"
parse_rate :: CharParser ModelState Rate
parse_rate = braces parse_expression



-- | parse list of reactants/products of reaction
-- we expect to parse a stream that looks like
-- n_1 R1 + n_2 R2 + n_3 R3 + .....
-- If n_i is missing we assume it is 1.0
parse_react_prod :: CharParser ModelState (M.Map String Int)
parse_react_prod = (reserved "nil" *> pure (M.empty))
                <|> (M.fromList <$> ((make_tuple <$> option 1 integer 
                    <*> (try molname <* whiteSpace)) 
                         `sepBy` reservedOp "+") )
                <?> "reactant or product list"
  
  where
    make_tuple x y = (y, fromInteger x)



-- | parse a number, can be used with 'many' and other parser
-- combinators; integers are automatically promoted to double
parse_number :: CharParser ModelState Double
parse_number = converter <$> naturalOrFloat
            <?> "signed integer or double"
  where
    converter val = case val of
                      Left i  -> (fromInteger i)
                      Right x -> x



-- | parse a positive number, can be used with 'many' and other 
-- parser combinators; integers are automatically promoted to double
parse_positive_number :: CharParser ModelState Double
parse_positive_number = naturalOrFloat 
  >>= \num -> case num of
                Left ival  -> if (ival > 0)
                                then return (fromInteger ival)
                                else pzero
                Right dval -> if (dval > 0)
                                then return dval
                                else pzero

  <?> "unsigned integer or double"         



-- | parser for a def block structure
parse_def_block :: String -> CharParser ModelState a 
                -> CharParser ModelState a
parse_def_block blockName parser = 

  between (reserved "def" *> reserved blockName )
          (reserved "end")
          (parser)
  <?> "parameter definitions"



-- | parse either a constant float or a complex function 
-- expression evaluation to a float at runtime
-- NOTE: It is important that we parse for a function
-- expression with try and not the constant expression
-- otherwise we get incomplete matches for things like 
-- 10*x+y.
parse_expression :: CharParser ModelState MathExpr
parse_expression = (try parse_function_expression) 
                <|> parse_constant_expression
                <?> "constant or function expression"



-- | parser for a simple rate constant expression
parse_constant_expression :: CharParser ModelState MathExpr
parse_constant_expression = Constant <$> parse_number
                         <?> "rate constant" 



-- | parser for a rate function
parse_function_expression :: CharParser ModelState MathExpr
parse_function_expression = Function <$> parse_infix_to_rpn
                         <?> "rate function" 


