{-# LANGUAGE FunctionalDependencies #-}

module Core.Bot where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map             as DM
import           Data.Maybe
import           Text.Read            (readMaybe)

import           Core.Types

class Monad m =>
      Bot m payload msg
  | m -> payload msg
  where
  getUpdates :: payload -> m (payload, [Update msg])
  sendMessage :: Event msg -> m ()

runBot :: (Bot m p msg) => Env -> p -> m ()
runBot env payload = runStateT (runReaderT (run payload) env) mempty >> pure ()
  where
    run :: (Bot m p msg) => p -> Context m ()
    run p = do
      (nextPayload, messages) <- liftBot $ getUpdates p
      mapM_ (runCommand . identifyCommand) messages
      run nextPayload

runCommand :: (Bot m p msg) => Command msg -> Context m ()
runCommand command =
  case command of
    EchoMessage id msg msgPayload -> echoMessage msgPayload id msg
    Help msgPayload -> showDescription msgPayload
    Repeat msgPayload -> askForNumberOfRepetitions msgPayload
    UnknownCommand msgPayload -> handleUnknownCommand msgPayload
    Select userId times msgPayload -> do
      saveNumberOfRepetitions userId times
      confirmNumberOfRepetitions msgPayload times

showDescription :: (Bot m p msg) => msg -> Context m ()
showDescription msgPayload = do
  helpText <- asks $ helpText . config
  liftBot $ sendMessage (Text helpText msgPayload)

handleUnknownCommand :: (Bot m p msg) => msg -> Context m ()
handleUnknownCommand msgPayload = do
  unknownCommandText <- asks $ unknownCommandText . config
  liftBot $ sendMessage (Text unknownCommandText msgPayload)

echoMessage :: (Bot m p msg) => msg -> UserID -> Message -> Context m ()
echoMessage msgPayload id text = do
  initial <- asks $ initialRepetitions . config
  times <- gets $ fromMaybe initial . DM.lookup id
  liftBot $ replicateM_ times $ sendMessage $ Text text msgPayload

saveNumberOfRepetitions :: (Bot m p msg) => UserID -> Times -> Context m ()
saveNumberOfRepetitions id times = modify $ DM.insert id times

askForNumberOfRepetitions :: (Bot m p msg) => msg -> Context m ()
askForNumberOfRepetitions msgPayload = do
  repeatText <- asks $ repeatText . config
  liftBot $ sendMessage (Keyboard keyboardLayout repeatText msgPayload)

confirmNumberOfRepetitions :: (Bot m p msg) => msg -> Times -> Context m ()
confirmNumberOfRepetitions msgPayload times =
  liftBot $ sendMessage (Text (makeConfirmText times) msgPayload)

makeConfirmText :: Int -> String
makeConfirmText 1 = "Now I will repeat only once"
makeConfirmText 2 = "Now I will repeat twice"
makeConfirmText n = "Now I will repeat " ++ show n ++ " times"

identifyCommand :: Update msg -> Command msg
identifyCommand (Message userId msg msgPayload) =
  case msg of
    "/help" -> Help msgPayload
    ('/':'h':'e':'l':'p':' ':_) -> Help msgPayload
    "/repeat" -> Repeat msgPayload
    ('/':'r':'e':'p':'e':'a':'t':' ':n) ->
      case clamp 1 5 <$> readMaybe n of
        Just n  -> Select userId n msgPayload
        Nothing -> UnknownCommand msgPayload
    text@"/" -> EchoMessage userId text msgPayload
    ('/':_) -> UnknownCommand msgPayload
    text -> EchoMessage userId text msgPayload
identifyCommand (Answer userId times msgPayload) = Select userId times msgPayload

keyboardLayout :: KeyboardLayout
keyboardLayout = [[1, 2, 3, 4, 5]]

liftBot :: (Bot m p msg) => m a -> Context m a
liftBot = lift . lift

clamp :: Ord a => a -> a -> a -> a
clamp low high a = min high (max a low)
