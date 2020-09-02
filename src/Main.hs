{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Prelude as P
import Prelude
import Servant.Client
import Web.Telegram.API (Token(..), ChatId(..))
import qualified Web.Telegram.API.Update as U (Polling(..))
import qualified Web.Telegram.API.Sending.Data as D (SMessage(..))
import Web.Telegram.Types (MessageContent(..), MessageMetadata(..), def)
import Web.Telegram.Types (Message(..), MessageEntity(..), MessageEntityType(..))
import Web.Telegram.Types.Update (ReqResult (..), Update(..))
import qualified Web.Telegram.Types as M (Chat(..), MessageEntity(..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Trans.State (modify)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Arrow
import Bot
import qualified Bot as B
import Command.Nix.Repl
import System.Environment
import System.Exit
import System.Directory
import Data.Map

import GHC.Generics
import Data.Aeson hiding (Success)
import qualified Data.ByteString.Lazy as BSL

data BotConfig = BotConfig { nixInstantiatePath  :: FilePath
                           , nixPath             :: [String]
                           , exprFilePath        :: FilePath
                           , predefinedVariables :: Maybe (Map String String)
                           , token               :: T.Text
                           , readWriteMode       :: Maybe Bool
                           } deriving (Generic)
instance FromJSON BotConfig where
    parseJSON = genericParseJSON defaultOptions

main :: IO ()
main = do args <- getArgs
          let configFile = if P.length args >= 1 then args !! 0 else "config.json"
          configStr <- BSL.readFile configFile
          let config = decode configStr :: Maybe BotConfig
          case config of
            Nothing -> do putStrLn "Error parsing config."
                          exitWith (ExitFailure 1)
            Just (BotConfig { Main.nixInstantiatePath = nip
                            , Main.nixPath = np
                            , Main.token = t
                            , Main.exprFilePath = efp
                            , Main.predefinedVariables = pdv
                            , Main.readWriteMode = rwm
                            }) -> do 
                manager' <- newManager tlsManagerSettings
                absExpr <- makeAbsolute efp
                let token' = Token t
                    env'   = mkClientEnv manager' (BaseUrl Https "api.telegram.org" 443 "")
                    ctx    = BotContext { B.chatId = Nothing
                                        , B.token = token'
                                        , env = env'
                                        , B.nixInstantiatePath = nip
                                        , B.nixPath = np
                                        , B.exprFilePath = absExpr
                                        , B.predefinedVariables = case pdv of
                                                                    Nothing  -> empty
                                                                    Just map -> map
                                        , B.readWriteMode = case rwm of
                                                              Nothing    -> False
                                                              Just value -> value
                                        }
                loop ctx 0

    where loop ctx offset_ = do

                            -- Main loop
                            next <- runTelegramBot ctx $ do 

                                        res <- getUpdates $ def { U.offset = Just offset_ }
                                        case res of
                                          Left err           -> do lift $ putStrLn $ "Error trying to get updates: " ++ show err
                                                                   return offset_
                                          Right (Ok updates) -> do lastUpdate <- process updates
                                                                   return $ lastUpdate + 1

                            -- Sleep, then loop
                            threadDelay 200000
                            loop ctx next

process :: [Update] -> TelegraM Int
process = process' 0
    where process' id []              = return id
          process' id (update:others) = processOne update >>= \id_ -> process' id_ others
          processOne update = do
              liftIO $ print update
              let id  = updateId update
                  cid = message >>> metadata >>> chat >>> M.chatId $ update
                  mid = message >>> metadata >>> messageId $ update
                  respond msg = sendMessage $ def { D.chatId = (ChatId cid), D.text = msg, D.replyToMessageId = Just mid }
                  processMessage msg = do result <- processMessage' msg
                                          case result of
                                            Success msg -> void $ respond msg
                                            Fail    msg -> void $ respond $ T.concat ["Error: ", msg]
                                            NoResponse  -> return ()
                                          return id
              modify $ \ctx -> ctx { B.chatId = Just (ChatId cid) }
              case update of
                  Message { message = msg }       -> processMessage msg
                  EditedMessage { message = msg } -> processMessage msg
                  _                               -> return id
          processMessage' (Msg { content = TextM { text = txt, entities = es } }) = executeCommands $ extractCommands txt es
          processMessage' _ = return NoResponse

executeCommands :: [Command] -> TelegraM ExecuteResult
executeCommands []           = return NoResponse
executeCommands [cmd]        = executeCommand cmd
executeCommands (cmd:others) = do executeCommand cmd
                                  executeCommands others

executeCommand (Command { cmdName = cname, input = cinput })
  | cname == "/eval" = evalCommand cinput
  | otherwise        = return NoResponse

extractCommands :: T.Text -> Maybe [MessageEntity] -> [Command]
extractCommands _ Nothing   = []
extractCommands _ (Just []) = []
extractCommands text (Just (entity:entities)) = case entityType entity of
                                                  BotCommand -> [Command { cmdName = subText off cend text, input = subText cend end text }]
                                                      where end  = T.length text
                                                            off  = offset entity
                                                            cend = off + (M.length entity)
                                                            subText begin end txt = T.drop begin $ T.take end txt
                                                  _          -> extractCommands text $ Just entities
-- extractCommands text (Just es) = commands
--     where (_, commands) = extractCommands' text es
--           extractCommands' :: T.Text -> [MessageEntity] -> (Int, [Command])
--           extractCommands' text []                = (T.length text, [])
--           extractCommands' text (entity:entities) = case entityType entity of
--                                                       BotCommand -> (off, Command { cmdName = subText off cend text, input = subText cend end text }:cmds)
--                                                       _ -> (end, cmds)
--                                                     where (end, cmds) = extractCommands' text entities
--                                                           off = offset entity
--                                                           cend = off + (length entity)
--           subText :: Int -> Int -> T.Text -> T.Text
--           subText begin end txt = T.drop begin $ T.take end txt
