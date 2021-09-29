{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Core.Bot where

import           Control.Monad.Reader           ( MonadReader )
import           Control.Monad.State
import           Core.Types
import           Data.Map

class  (MonadReader Env m) => Bot m payload msg help repeat select uc | m -> payload msg help repeat select uc where
  getCommand ::payload -> m (payload, [Command msg help repeat select uc])
  echoMessage :: msg -> m ()
  showDescription :: help -> m ()
  handleUnknownCommand :: uc -> m ()
  askForNumberOfRepetitions :: repeat -> m ()
  saveNumberOfRepetitions :: select -> m ()
  confirmNumberOfRepetitions :: select -> m ()
  confirmNumberOfRepetitions _ = return ()

runBot :: (Bot m payload msg help repeat select uc) => payload -> m ()
runBot payload = do
  (nextPayload, commands) <- getCommand payload
  mapM_ runCommand commands
  runBot nextPayload
 where
  runCommand command = case command of
    EchoMessage msg    -> echoMessage msg
    Help        help   -> showDescription help
    Repeat      repeat -> askForNumberOfRepetitions repeat
    Select      select -> do
      saveNumberOfRepetitions select
      confirmNumberOfRepetitions select
    UnknownCommand un -> handleUnknownCommand un
