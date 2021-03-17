{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module NixEval ( nixInstantiate
               , NixOptions(..)
               , OptionValue(..)
               , unsetNixOptions
               , EvalMode(..)
               , EvalResult(..)
               , NixEvalOptions(..)
               , defNixEvalOptions
               ) where

import           Data.ByteString.Lazy       (ByteString, append)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Text                  (Text, pack)
import qualified Data.Text                  as Text
import Data.Text.Encoding
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           System.Exit
import qualified System.Process.Typed       as TP
import           GHC.Generics

data OptionValue = OptInt  Int
                 | OptBool Bool
                 | OptStr  String
                 deriving (Generic)

value :: OptionValue -> String
value (OptBool True)  = "true"
value (OptBool False) = "false"
value (OptInt i)      = show i
value (OptStr s)      = show s

data NixOptions = NixOptions
  { nixConf                   :: Map String OptionValue
  , readWriteMode             :: Bool
  } deriving (Generic)

unsetNixOptions :: NixOptions
unsetNixOptions = NixOptions
  { nixConf = M.empty
  , readWriteMode = False
  }
{-
publicOptions :: NixOptions
publicOptions = def
  { cores = 0
  , fsyncMetadata = False
  , restrictEval = True
  , sandbox = True
  , timeout = 3
  , maxJobs = 0
  , allowImportFromDerivation = False
  }
-}

optionsToArgs :: NixOptions -> [String]
optionsToArgs opts = concat $ 
    (map (\(k, v) -> [ "--option", k, value v ]) (M.toList $ nixConf opts)) 
    ++ if readWriteMode opts then [ [ "--read-write-mode" ] ] else []

data EvalMode = Parse | Lazy | Strict | Json

modeToArgs :: EvalMode -> [String]
modeToArgs Parse  = ["--parse"]
modeToArgs Lazy   = ["--eval"]
modeToArgs Strict = ["--eval", "--strict"]
modeToArgs Json   = ["--eval", "--strict", "--json"]

data NixEvalOptions = NixEvalOptions
  { contents   :: Either ByteString FilePath
  , attributes :: [String]
  , arguments  :: Map String String
  , nixPath    :: [String]
  , mode       :: EvalMode
  , options    :: NixOptions
  }

defNixEvalOptions :: Either ByteString FilePath -> NixEvalOptions
defNixEvalOptions file = NixEvalOptions
  { contents = file
  , attributes = []
  , arguments = M.empty
  , nixPath = []
  , mode = Lazy
  , options = unsetNixOptions
  }

toProc :: FilePath -> NixEvalOptions -> TP.ProcessConfig () () ()
toProc nixInstantiatePath NixEvalOptions { contents, attributes, arguments, nixPath, mode, options } = let
  opts = modeToArgs mode
    ++ [case contents of
          Left _     -> "-"
          Right path -> path
       ]
    ++ concatMap (\a -> [ "-A", a ]) attributes
    ++ concatMap (\(var, val) -> [ "--arg", var, val ]) (M.assocs arguments)
    ++ concatMap (\p -> [ "-I", p ]) nixPath
    ++ optionsToArgs options
  process = TP.proc "@timeout@" $ ["-k", "5s", "5s", nixInstantiatePath] ++ opts
  in case contents of
    Left bytes -> TP.setStdin (TP.byteStringInput bytes) process
    Right _    -> process


data EvalResult = Ok String | Error String | Timeout

nixInstantiate :: FilePath -> NixEvalOptions -> IO EvalResult
nixInstantiate nixInstPath opts = toEither <$> TP.readProcess (toProc nixInstPath opts)
  where toEither (ExitSuccess, stdout, _)   = Ok $ toString stdout
        toEither (ExitFailure code, _, stderr) = if code == (-9) then Timeout 
                                                                 else Error $ toString stderr
        toString = Text.unpack . decodeUtf8 . BS.toStrict
