{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Command.Nix.Repl where

import           NixEval
import           Bot
import qualified Bot as B

import           Control.Applicative        ((<|>))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson          hiding (Error)
import           Data.Bifunctor             (bimap)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List
import           Data.Text                  (Text, unpack, pack)
import qualified Data.Text                  as Text
import Data.Text.Encoding
import           GHC.Generics
import           System.Directory
import           System.FilePath
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as C

import           Data.Map                   (Map)
import qualified Data.Map                   as M

import           Web.Telegram.API (ChatId(..))

import           HTMLEntities.Text


data Instruction = Definition String String
                 | Evaluation Bool String
                 | ReplCommand String [String]
                 deriving Show

data NixState = NixState
  { variables :: Map String String
  , scopes    :: [ String ]
  } deriving (Show, Read, Generic)

instance FromJSON NixState
instance ToJSON NixState

type Parser = P.Parsec () String

type ReplApp = StateT NixState TelegraM

parser :: Parser Instruction
parser =
  P.try cmdParser <|> P.try defParser <|> Evaluation False <$> (C.space *> P.takeRest)
    where
      literal :: Parser String
      literal = (:) <$> (C.letterChar <|> C.char '_') <*> P.many (C.alphaNumChar <|> C.char '_' <|> C.char '-' <|> C.char '\'')

      cmdParser :: Parser Instruction
      cmdParser = do
        C.space
        _ <- C.char ':'
        cmd <- literal
        case cmd of
          "p" -> Evaluation True <$> (C.space *> P.takeRest)
          _ -> do
            args <- P.many (C.space *> P.some (P.anySingleBut ' '))
            C.space
            return $ ReplCommand cmd args

      defParser :: Parser Instruction
      defParser = do
        C.space
        lit <- literal
        C.space
        _ <- C.char '='
        C.space
        Definition lit <$> P.takeRest

nixFile :: NixState -> String -> TelegraM String
nixFile NixState { variables, scopes } lit = do
    writeExprFile scopes lit
    file <- gets exprFilePath
    predefinedVariables <- gets predefinedVariables
    return $ "let overrides = {\n"
             ++ concatMap (\(l, val) -> "\t" ++ l ++ " = " ++ val ++ ";\n") (M.assocs (M.unions [predefinedVariables, defaultVariables, variables]))
             ++ "}; in scopedImport overrides " ++ file

writeExprFile :: [ String ] -> String -> TelegraM ()
writeExprFile scopes lit = do
    exprPath <- gets exprFilePath
    let content = concatMap (\scope -> "\twith (" ++ scope ++ ");\n") (reverse scopes) ++ "\t" ++ lit
    lift $ writeFile exprPath content

nixEval :: String -> EvalMode -> TelegraM EvalResult
nixEval contents mode = do
  nixInstPath   <- gets nixInstantiatePath
  nixPath       <- gets Bot.nixPath
  exprPath      <- gets exprFilePath
  rwMode        <- gets Bot.readWriteMode
  res <- lift . liftIO $ nixInstantiate nixInstPath (defNixEvalOptions (Left (BS.fromStrict (encodeUtf8 (Text.pack contents)))))
    { mode = mode
    , NixEval.nixPath = exprPath:nixPath
    , options = unsetNixOptions
      { allowImportFromDerivation = Just True
      , restrictEval = Just True
      , sandbox = Just True
      , showTrace = Just True
      , NixEval.readWriteMode = rwMode
      }
    }
  return res

formatResult :: EvalResult -> String
formatResult result = case result of
    Ok output -> cut . escape $ "> " ++ output
    Error err -> if length err > 4000 then cut $ escape err else escape err ++ "\n<b><i>You shall not parse</i></b>"
    Timeout   -> "<b><i>Too Long, Don't Evaluate</i></b>"
  where cut str = if length str > 4000 then (take 4000 str) 
                                            ++ "...\n\n<b><i>I have received a truly marvelous result of this, which this chatbox is too narrow to contain</i></b>"
                                      else str

escape :: String -> String
escape str = unpack . text . pack $ str

tryMod :: (NixState -> NixState) -> ReplApp (Maybe String)
tryMod modi = do
  newState <- gets modi
  contents <- lift $ nixFile newState "null"
  result <- lift $ nixEval contents Parse
  case result of
    Ok _ -> do
      put newState
      return Nothing
    Error err -> return $ Just err
    Timeout   -> return $ Just "Timeout"

handle :: Instruction -> ReplApp String
handle (Definition lit val) = do
  result <- tryMod (\s -> s { variables = M.insert lit val (variables s) })
  case result of
    Nothing  -> return $ lit ++ " defined"
    Just err -> return . formatResult $ Error err
handle (Evaluation strict lit) = do
  st <- get
  contents <- lift $ nixFile st ("_show (\n" ++ lit ++ "\n)")
  result <- lift $ nixEval contents (if strict then Strict else Lazy)
  return $ formatResult result
handle (ReplCommand "h" _) = return . escape $ "<expr>        Evaluate and print expression\n"
    ++ "<x> = <expr>  Bind expression to variable\n"
    ++ ":a <expr>     Add attributes from resulting set to scope\n"
    ++ ":p <expr>     Evaluate and print expression recursively\n"
    ++ ":v            Show all variable bindings\n"
    ++ ":v <x>        Show variable bindings of <x>\n"
    ++ ":s            Show all scopes\n"
    ++ ":r            Reset bindings and scopes"
handle (ReplCommand "a" []) = return ":a needs an argument"
handle (ReplCommand "a" args) = do
  result <- tryMod (\s -> s { scopes = unwords args : scopes s } )
  case result of
    Nothing  -> return "imported scope"
    Just err -> return . formatResult $ Error err
handle (ReplCommand "v" [var]) = do
  val <- gets $ M.findWithDefault (var ++ " is not defined") var . variables
  return . escape $ var ++ " = " ++ val
handle (ReplCommand "v" _) = do
  vars <- gets $ M.keys . variables
  return . escape $ "All bindings:\n  " ++ intercalate ", " vars
handle (ReplCommand "s" _) = do
  scopes <- gets scopes
  return . escape $ "All scopes:\n  " ++ intercalate "\n, " (map (\s -> "(" ++ s ++ ")") scopes)
--handle (ReplCommand "d" [lit]) = do
--  litDefined <- gets $ M.member lit . variables
--  if litDefined
--    then do
--      modify (\s -> s { variables = M.delete lit (variables s) })
--      return . Just $ "undefined " ++ lit
--    else return . Just $ lit ++ " is not defined"
--handle (ReplCommand "d" _) = return $ Just ":d takes a single argument"
handle (ReplCommand "r" []) = do
  modify (\s -> s { scopes = defaultScopes, variables = M.empty })
  return "Scopes and bindings got reset"
--handle (ReplCommand "r" ["v"]) = do
--  modify (\s -> s { variables = M.empty })
--  return $ Just "Variables got reset"
--handle (ReplCommand "r" _) = do
--  put $ NixState M.empty []
--  return $ Just "State got reset"
handle (ReplCommand cmd _) = return $ "Unknown command: " ++ cmd

defaultVariables :: Map String String
defaultVariables = M.fromList
  [ ("_show", "x: if builtins.isAttrs x && x ? type && x.type == \"derivation\" then \"«derivation ${x.drvPath}»\" else x")
  , ("import", "fn: scopedImport overrides fn")
  , ("scopedImport", "attrs: fn: scopedImport (overrides // attrs) fn")
  , ("builtins", "builtins // { inherit (overrides) import scopedImport; } // (overrides.builtinsOverrides or {})")
  ]

defaultScopes :: [String]
defaultScopes = [ "import <nixpkgs> {}" ]

evalCommand :: Text -> TelegraM ExecuteResult
evalCommand input = do
    chat <- gets chatId
    case chat of
      Nothing -> return $ Fail "ChatId not defined."
      Just (ChatId cid) -> case P.runParser parser "(input)" (Text.unpack input) of
                            Right instruction -> doEval cid instruction
                            Left _            -> return $ Fail "Failed to parse input."
      _ -> return $ Fail "Unknown error"
    where doEval cid instruction = do
            let stateDir = "state" </> show cid </> "eval.state"
                stateFile = stateDir </> "eval.state"
            lift $ createDirectoryIfMissing True stateDir
            exists       <- liftIO $ doesFileExist stateFile
            initialState <- if exists then
              liftIO (decodeFileStrict stateFile) >>= \case
                  Just result -> return result
                  Nothing     -> do
                      lift $ putStrLn $ "Failed to decode nix state at " <> stateFile
                      return $ NixState M.empty defaultScopes
              else
                return $ NixState M.empty defaultScopes       
            (result, newState) <- runStateT (handle instruction) initialState
            liftIO $ encodeFile stateFile newState
            return $ B.Success $ pack result
