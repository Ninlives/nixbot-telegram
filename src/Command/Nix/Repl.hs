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
import           Data.Aeson
import           Data.Bifunctor             (bimap)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List
import           Data.Text                  (Text, pack)
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
    let content = concatMap (\scope -> "\twith " ++ scope ++ ";\n") (reverse scopes) ++ "\t" ++ lit
    lift $ writeFile exprPath content

nixEval :: String -> EvalMode -> TelegraM (Either String String)
nixEval contents mode = do
  nixInstPath <- gets nixInstantiatePath
  nixPath <- gets Bot.nixPath
  exprPath <- gets exprFilePath
  res <- lift . liftIO $ nixInstantiate nixInstPath (defNixEvalOptions (Left (BS.fromStrict (encodeUtf8 (Text.pack contents)))))
    { mode = mode
    , NixEval.nixPath = exprPath:nixPath
    , options = unsetNixOptions
      { allowImportFromDerivation = Just True
      , restrictEval = Just True
      , sandbox = Just True
      , showTrace = Just True
      }
    }
  return $ bimap (Text.unpack . decodeUtf8 . BS.toStrict) (Text.unpack . decodeUtf8 . BS.toStrict) res

tryMod :: (NixState -> NixState) -> ReplApp (Maybe String)
tryMod modi = do
  newState <- gets modi
  contents <- lift $ nixFile newState "null"
  result <- lift $ nixEval contents Parse
  case result of
    Right _ -> do
      put newState
      return Nothing
    Left err -> return $ Just err

handle :: Instruction -> ReplApp String
handle (Definition lit val) = do
  result <- tryMod (\s -> s { variables = M.insert lit val (variables s) })
  case result of
    Nothing  -> return $ lit ++ " defined"
    Just err -> return err
handle (Evaluation strict lit) = do
  st <- get
  contents <- lift $ nixFile st ("_show (\n" ++ lit ++ "\n)")
  result <- lift $ nixEval contents (if strict then Strict else Lazy)
  case result of
    Right value -> return value
    Left err    -> return err
handle (ReplCommand "l" []) = return ":l needs an argument"
handle (ReplCommand "l" args) = do
  result <- tryMod (\s -> s { scopes = unwords args : scopes s } )
  case result of
    Nothing  -> return "imported scope"
    Just err -> return err
handle (ReplCommand "v" [var]) = do
  val <- gets $ M.findWithDefault (var ++ " is not defined") var . flip M.union defaultVariables . variables
  return $ var ++ " = " ++ val
handle (ReplCommand "v" _) = do
  vars <- gets $ M.keys . flip M.union defaultVariables . variables
  return $ "All bindings: " ++ unwords vars
handle (ReplCommand "s" _) = do
  scopes <- gets scopes
  return $ "All scopes: " ++ intercalate ", " scopes
--handle (ReplCommand "d" [lit]) = do
--  litDefined <- gets $ M.member lit . variables
--  if litDefined
--    then do
--      modify (\s -> s { variables = M.delete lit (variables s) })
--      return . Just $ "undefined " ++ lit
--    else return . Just $ lit ++ " is not defined"
--handle (ReplCommand "d" _) = return $ Just ":d takes a single argument"
handle (ReplCommand "r" []) = do
  modify (\s -> s { scopes = [], variables = M.empty })
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
  [ ("_show", "x: if overrides.lib.isDerivation x then \"«derivation ${x.drvPath}»\" else x")
  , ("pkgs", "import <nixpkgs> {}")
  , ("lib", "overrides.pkgs.lib")
  , ("import", "fn: scopedImport overrides fn")
  , ("scopedImport", "attrs: fn: scopedImport (overrides // attrs) fn")
  , ("builtins", "builtins // overrides // (overrides.builtinsOverrides or {})")
  ]

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
                      return $ NixState M.empty []
            else
              return $ NixState M.empty []       
            (result, newState) <- runStateT (handle instruction) initialState
            liftIO $ encodeFile stateFile newState
            return $ B.Success $ pack $ "> " ++ result
