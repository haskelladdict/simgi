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

-- | main gsim driver
module CommandLine ( process_commandline 
                   , Options(..)
                   ) where

-- imports
import Prelude
import System
import System.Console.GetOpt

-- local imports
import Messages


-- | main driver for command line processing
process_commandline :: [String] -> IO ((CmdlRequest,String), [String])
process_commandline args = 

  let 
    (actions, nonOpts, _) = getOpt RequireOrder options args
  in
    foldl (>>=) ( return defaultOptions ) actions >>= \opts ->

    let 
      Options { cmdlRequest = request 
              , cmdlString   = pattern
              } = opts
    in
      return ((request,pattern),nonOpts) 



-- | possible options for commandline
data CmdlRequest = None | Help


-- | data structure for keeping track of 
-- selected command line options
data Options = Options {
  cmdlRequest :: CmdlRequest,
  cmdlString  :: String
}


-- | default selections
defaultOptions :: Options
defaultOptions = Options {
  cmdlRequest = None,
  cmdlString  = ""
}


-- | available command line flags
options :: [OptDescr (Options -> IO Options)]
options = [
  Option ['v'] ["version-info"] (NoArg version_info) 
         "show version information"]


-- | extractor function for version info
version_info :: Options -> IO Options
version_info _ =
  do
    show_version
    exitWith ExitSuccess
