module Bot where 

import Data.Proxy
import Data.Text
import Data.Map
import Servant.Client
import Web.Telegram.API (SendMessage, Token(..), ChatId(..))
import Web.Telegram.API.Sending.Data (SMessage(..))
import Web.Telegram.API.Update (Polling(..), GetUpdates)
import Web.Telegram.Types (Message)
import Web.Telegram.Types.Update (ReqResult(..), Update(..))
import Control.Monad.Trans.State (gets, get, StateT, evalStateT)
import Control.Monad.Trans.Class

data BotContext    = BotContext { chatId              :: Maybe ChatId
                                , token               :: Token
                                , env                 :: ClientEnv
                                , nixInstantiatePath  :: FilePath
                                , nixPath             :: [String]
                                , exprFilePath        :: FilePath
                                , predefinedVariables :: Map String String
                                , readWriteMode       :: Bool
                                }
data Command       = Command { cmdName :: Text, input :: Text }
data ExecuteResult = NoResponse | Success Text | Fail Text

type TelegraM      = StateT BotContext IO
type BotClientM    = StateT BotContext ClientM
type APIResponse a = TelegraM (Either ClientError a)

runTelegramBot :: BotContext -> TelegraM a -> IO a
runTelegramBot ctx action = evalStateT action ctx

executeTelegram :: BotClientM a -> APIResponse a
executeTelegram action = do
    env' <- gets env
    ctx <- get
    lift $ runClientM (evalStateT action ctx) env'

sendMessage :: SMessage -> APIResponse (ReqResult Message)
sendMessage = executeTelegram . sendMessage'

sendMessage' :: SMessage -> BotClientM (ReqResult Message)
sendMessage' msg = gets token >>= \token' -> lift $ send token' msg
    where send = (client (Proxy :: Proxy SendMessage)) :: Token -> SMessage -> ClientM (ReqResult Message)

getUpdates :: Polling -> APIResponse (ReqResult [Update])
getUpdates = executeTelegram . getUpdates'

getUpdates' :: Polling -> BotClientM (ReqResult [Update])
getUpdates' polling = gets token >>= \token' -> lift $ get token' polling
    where get = (client (Proxy :: Proxy GetUpdates)) :: Token -> Polling -> ClientM (ReqResult [Update])
