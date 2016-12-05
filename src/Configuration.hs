module Configuration where

import           Data.Monoid ((<>))
import           Data.Version (showVersion)
import           Data.Text (Text, pack, unpack)
import           Network.AWS.Types (LogLevel(..))
import qualified Network.AWS.CloudFormation.Types as CFN
import           Options.Applicative ( info
                                     , helper
                                     , progDesc
                                     , long
                                     , short
                                     , help
                                     , metavar
                                     , strArgument
                                     , showDefault
                                     , subparser
                                     , execParser
                                     , flag
                                     , value
                                     , optional
                                     , infoOption
                                     , hidden
                                     , str
                                     , Parser
                                     , ReadM
                                     , ParserInfo(..)
                                     )
import qualified Options.Applicative.Builder as OB

import qualified Paths_evaporate as PackageInfo

import           Types (StackName(..))

data Command =
    Check
  | Get
  | Delete
  | Create
  | Upsert
  deriving (Show, Eq)

data Options = Options { command         :: Command
                       , stackNameOption :: Maybe StackName
                       , isDryRun        :: Bool
                       , logLevel        :: LogLevel
                       , onCreateFailure :: CFN.OnFailure
                       , configFilePath  :: FilePath }

loadConfiguration :: IO Options
loadConfiguration =
  execParser (info (helper <*> version <*> parseOptions) $
    progDesc "A simple, convention-based CloudFormation Stack deployment tool.\
    \ The deployment region is read from the environment variable AWS_REGION or\
    \ retrieved from the Instance Identity Document (for EC2 instances),\
    \ otherwise it defaults to us-east-1.")

parseOptions :: Parser Options
parseOptions = Options
           <$> parseCommand
           <*> parseStackName
           <*> dryRun
           <*> verbose
           <*> onCreateFailureParser
           <*> configFile

parseCommand :: Parser Command
parseCommand = subparser $
     OB.command "check"   (pure Check  `withInfo` "Check the stack status for a stack")
  <> OB.command "get"     (pure Get    `withInfo` "Get stack resources info for a stack")
  <> OB.command "delete"  (pure Delete `withInfo` "Delete a stack")
  <> OB.command "create"  (parseCreate `withInfo` "Create a stack")
  <> OB.command "upsert"  (parseUpsert `withInfo` "Upsert a stack (Update if exists, otherwise Create)")

dryRun :: Parser Bool
dryRun =
  flag False True
    (  long "dry-run"
    <> help "Print the command that would be executed but don't execute it.")

parseStackName :: Parser (Maybe StackName)
parseStackName =
  optional (OB.option (StackName <$> text)
    (  long "stack-name"
    <> metavar "STACK_NAME"
    <> help "The name of the specific stack to be operated on. \
     \ If omitted, all stacks defined in evaporate.yaml will be operated on. \
     \ Upsert/create operations will fail if the stack being operated on has a \
     \ dependency on another stack." ))

verbose :: Parser LogLevel
verbose =
  flag Info Trace
    (  long "verbose"
    <> short 'V'
    <> help "Enable verbose logging.")

onCreateFailureParser :: Parser CFN.OnFailure
onCreateFailureParser =
  OB.option OB.auto
  (  long "on-create-fail"
  <> metavar "FAILURE_BEHAVIOUR"
  <> value CFN.Rollback
  <> showDefault
  <> help "The behaviour that occurs when a stack cannot be created. \
  \ Possible values: Delete, DoNothing, Rollback.")

version :: Parser (a -> a)
version =
  infoOption displayText
    (  long "version"
    <> short 'v'
    <> help "Print the version of the application."
    <> hidden )
  where displayText = showVersion PackageInfo.version

configFile :: Parser FilePath
configFile =
  strArgument
  (  value "evaporate.yaml"
  <> metavar "CONFIG_FILE_PATH"
  <> showDefault
  <> help "The path to the yaml config file containing your stack \
  \ definitions.")

text :: ReadM Text
text = pack <$> str

parseCreate :: Parser Command
parseCreate = pure Create

parseUpsert :: Parser Command
parseUpsert = pure Upsert

withInfo :: Parser a -> Text -> ParserInfo a
withInfo opts = info (helper <*> opts) . progDesc . unpack
