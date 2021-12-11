{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Identity
import qualified Data.Map               as DM
import           Test.Tasty
import           Test.Tasty.Hspec

import qualified Core.Bot               as B
import qualified Core.Types             as BT

deriving instance Show msg => Show (BT.Command msg)

deriving instance Show msg => Show (BT.Event msg)

deriving instance Eq msg => Eq (BT.Command msg)

deriving instance Eq msg => Eq (BT.Event msg)

data SpyState =
  SpyState
    { getUpdatesC           :: Int
    , sendMessageC          :: Int
    , sendMessageCalledWith :: Maybe (BT.Event ())
    }

newtype TestBotT a =
  TestBotT
    { unTestBotT :: StateT SpyState Identity a
    }
  deriving (Functor, Applicative, Monad, MonadState SpyState)

instance B.Bot TestBotT () () where
  getUpdates msg = modify (\s -> s {getUpdatesC = getUpdatesC s + 1}) >> pure ((), [])
  sendMessage event =
    modify (\s -> s {sendMessageC = sendMessageC s + 1, sendMessageCalledWith = Just event}) >>
    pure ()

main :: IO ()
main = do
  botSpecs <-
    join <$>
    mapM
      testSpecs
      [ spec_identifyCommand
      , spec_runCommand
      , spec_saveNumberOfRepetitions
      , spec_askForNumberOfRepetitions
      , spec_handleUnknownCommand
      , spec_showDescription
      , spec_echoMessage
      ]
  defaultMain $ testGroup "Bot Test" botSpecs

spec_runCommand :: Spec
spec_runCommand = do
  describe "runCommand" $ do
    it "runs echoMessage on EchoMessage command" $ do
      let (_, state) = runBotFn (B.runCommand (BT.EchoMessage userId "Test" ())) defaultConfig
      sendMessageCalledWith state `shouldBe` Just (BT.Text "Test" ())
      sendMessageC state `shouldBe` 1
    it "runs showDescription on Help command" $ do
      let (_, state) = runBotFn (B.runCommand (BT.Help ())) defaultConfig
      sendMessageCalledWith state `shouldBe` Just (BT.Text (BT.helpText defaultConfig) ())
      sendMessageC state `shouldBe` 1
    it "runs askForNumberOfRepetitions on Help command" $ do
      let (_, state) = runBotFn (B.runCommand (BT.Repeat ())) defaultConfig
      sendMessageCalledWith state `shouldBe`
        Just (BT.Keyboard B.keyboardLayout (BT.repeatText defaultConfig) ())
      sendMessageC state `shouldBe` 1
    it "runs handleUnknownCommand on UnknownCommand command" $ do
      let (_, state) = runBotFn (B.runCommand (BT.UnknownCommand ())) defaultConfig
      sendMessageCalledWith state `shouldBe` Just (BT.Text (BT.unknownCommandText defaultConfig) ())
      sendMessageC state `shouldBe` 1
    it "runs saveNumberOfRepetitions and confirmNumberOfRepetitions on Select command" $ do
      let (preferences, state) = runBotFn (B.runCommand (BT.Select userId 3 ())) defaultConfig
          times = DM.lookup userId preferences
      DM.lookup userId preferences `shouldBe` Just 3
      sendMessageCalledWith state `shouldBe` Just (BT.Text (B.makeConfirmText 3) ())

spec_askForNumberOfRepetitions :: Spec
spec_askForNumberOfRepetitions = do
  describe "askForNumberOfRepetition" $ do
    it "sends keyboard layout with the repeat text from the config" $ do
      let (_, state) = runBotFn (B.askForNumberOfRepetitions ()) defaultConfig
      sendMessageCalledWith state `shouldBe`
        Just (BT.Keyboard B.keyboardLayout (BT.repeatText defaultConfig) ())

spec_saveNumberOfRepetitions :: Spec
spec_saveNumberOfRepetitions = do
  describe "saveNumberOfRepetitions" $ do
    it "saves number of repetitions by user id" $ do
      let (preferences, _) = runBotFn (B.saveNumberOfRepetitions userId 4) defaultConfig
          times = DM.lookup userId preferences
      times `shouldBe` Just 4

spec_handleUnknownCommand :: Spec
spec_handleUnknownCommand = do
  describe "handleUnknownCommand" $ do
    it "sends message with the unknown text from the config" $ do
      let (_, state) = runBotFn (B.handleUnknownCommand ()) defaultConfig
      sendMessageCalledWith state `shouldBe` Just (BT.Text (BT.unknownCommandText defaultConfig) ())
      sendMessageC state `shouldBe` 1

spec_showDescription :: Spec
spec_showDescription = do
  describe "showDescription" $ do
    it "sends message with the help text from the config" $ do
      let (_, state) = runBotFn (B.showDescription ()) defaultConfig
      sendMessageCalledWith state `shouldBe` Just (BT.Text (BT.helpText defaultConfig) ())
      sendMessageC state `shouldBe` 1

spec_echoMessage :: Spec
spec_echoMessage = do
  describe "echoMessage" $ do
    it "always performs echoMessage with the text event" $ do
      let (_, state) = runBotFn (B.echoMessage () userId "Test") defaultConfig
      sendMessageCalledWith state `shouldBe` Just (BT.Text "Test" ())
    it "performs echoMessage as many times as specified in the config" $ do
      let (_, state1) = runBotFn (B.echoMessage () userId "Test") defaultConfig
      sendMessageC state1 `shouldBe` 1
      let (_, state2) =
            runBotFn (B.echoMessage () userId "Test") defaultConfig {BT.initialRepetitions = 4}
      sendMessageC state2 `shouldBe` 4
    it "performs echoMessage as many times as specified in the user preferences" $ do
      let (_, state) = runBotFn saveAndEcho defaultConfig
      sendMessageC state `shouldBe` 5
  where
    saveAndEcho = do
      B.saveNumberOfRepetitions userId 5
      B.echoMessage () userId "Test"

spec_identifyCommand :: Spec
spec_identifyCommand =
  describe "identifyCommand" $ do
    it "returns Help command when the text is '/help'" $
      B.identifyCommand (BT.Message userId "/help" ()) `shouldBe` BT.Help ()
    it "returns Help command when the text starts with '/help'" $
      B.identifyCommand (BT.Message userId "/help another text" ()) `shouldBe` BT.Help ()
    it "returns Repeat command when the text is '/repeat'" $
      B.identifyCommand (BT.Message userId "/repeat" ()) `shouldBe` BT.Repeat ()
    it "returns Select command with number of repetitions when text after '/repeat' is number" $
      B.identifyCommand (BT.Message userId "/repeat 4" ()) `shouldBe` BT.Select userId 4 ()
    it "returns Select command when it is answer" $
      B.identifyCommand (BT.Answer "1" 2 ()) `shouldBe` BT.Select "1" 2 ()
    it "returns Select command with the number of repetitions between 1 and 5" $
      (B.identifyCommand (BT.Message userId "/repeat 123" ()) `shouldBe` BT.Select userId 5 ()) *>
      (B.identifyCommand (BT.Message userId "/repeat 0" ()) `shouldBe` BT.Select userId 1 ()) *>
      (B.identifyCommand (BT.Message userId "/repeat -20" ()) `shouldBe` BT.Select userId 1 ())
    it "returns EchoMessage for any others text" $
      (B.identifyCommand (BT.Message userId "/" ()) `shouldBe` BT.EchoMessage userId "/" ()) *>
      (B.identifyCommand (BT.Message userId "asd" ()) `shouldBe` BT.EchoMessage userId "asd" ()) *>
      (B.identifyCommand (BT.Message userId "Some text" ()) `shouldBe`
       BT.EchoMessage userId "Some text" ()) *>
      (B.identifyCommand (BT.Message userId "" ()) `shouldBe` BT.EchoMessage userId "" ())
    it "returns UnknownCommand when the text after '/repeat' is not a number" $
      B.identifyCommand (BT.Message userId "/repeat asd" ()) `shouldBe` BT.UnknownCommand ()
    it "returns UnknownCommand when the command could not be recognized" $
      B.identifyCommand (BT.Message userId "/unknown" ()) `shouldBe` BT.UnknownCommand ()

userId :: BT.UserID
userId = "1"

defaultConfig :: BT.Config
defaultConfig = BT.Config "Help" "Unknown" "Repeat" 1

runBotFn :: BT.Context TestBotT () -> BT.Config -> (BT.Preferences, SpyState)
runBotFn fn config = do
  let env = BT.Env config
      spy = SpyState {getUpdatesC = 0, sendMessageC = 0, sendMessageCalledWith = Nothing}
  runIdentity
    (runStateT
       (unTestBotT (execStateT (runReaderT fn env) mempty))
       SpyState {getUpdatesC = 0, sendMessageC = 0, sendMessageCalledWith = Nothing})
