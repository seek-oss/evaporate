{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module StackDependency where

import           Control.Exception.Safe (throwM, Exception(..), MonadThrow)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Reader.Class (MonadReader(..))
import           Data.Function ((&))
import           Data.Graph.Inductive.Graph ( empty
                                            , mkGraph
                                            , lab
                                            , LNode
                                            , LEdge
                                            , Graph(..)
                                            )
import qualified Data.Graph.Inductive.PatriciaTree as G
import           Data.Graph.Inductive.Query.DFS (topsort', scc)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe, catMaybes)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Text as Text
import           Data.Text (pack, unpack, Text)
import           Data.Tuple.Extra (first, dupe)
import           Network.AWS.Data.Text (ToText(..))
import           Network.AWS.S3.Types (BucketName(..))
import           Text.Trifecta (parseString, Result(..))

import           StackParameters ( parameterValue
                                 , getParameters
                                 , BucketFiles(..)
                                 , ParameterValue(..)
                                 , StackDescription(..)
                                 )
import           Types ( AWSAccountID
                       , OutputName(..)
                       , StackName(..)
                       , StackOutputName(..))

-- An edge from stack1 to stack2 implies that stack1 must exist before stack2
-- can be created, i.e. stack2 is dependent on stack1
type StackNode = LNode StackDescription
type StackDependency = LEdge OutputName
type StackDependencyGraph = G.Gr StackDescription OutputName
type StackNodeMap = Map.Map StackName StackNode

data StackReference = StackReference {
    _srSourceStackName :: StackName
  , _srStackOutputName :: StackOutputName
} deriving (Show, Eq)

newtype UnknownStackReference = UnknownStackReference StackReference
  deriving (Eq, Show)

instance Exception UnknownStackReference where
  displayException (UnknownStackReference StackReference{..}) = unpack $
       "Unknown stack '" <> (getStackName . _soStackName) _srStackOutputName
    <> "' is referenced within a parameter of stack '"
    <> getStackName _srSourceStackName <> "'."

data DependencyCycleDetected = DependencyCycleDetected [[StackName]]
  deriving (Eq, Show)

instance Exception DependencyCycleDetected where
  displayException (DependencyCycleDetected sccStackNames) = unpack $
       "The following group(s) of stacks form a dependency cycle:\n"
    <> Text.intercalate "\n" (cycleToText <$> zip [1..] sccStackNames)
    where
      cycleToText :: (Int, [StackName]) -> Text
      cycleToText (cycleNumber, stackNames) =
           "Cycle" <> (pack . show $ cycleNumber) <> ": "
        <> (Text.intercalate ", " . fmap getStackName) stackNames

determineStackOrdering :: MonadThrow m
                       => StackDependencyGraph
                       -> m [StackDescription]
determineStackOrdering stackDependencyGraph =
  case filter isCycle . scc $ stackDependencyGraph of
    []            -> return $ topsort' stackDependencyGraph
    cycleSccNodes -> do
      let stackCycles = mapMaybe (lab stackDependencyGraph) <$> cycleSccNodes
      throwM $ DependencyCycleDetected $ fmap _stackName <$> stackCycles
  where
    -- See https://en.wikipedia.org/wiki/Strongly_connected_component#Definitions
    -- for an explanation as to why this works
    isCycle :: [a] -> Bool
    isCycle = (> 1) . length

makeStackDependencyGraph :: forall m. (MonadThrow m)
                         => [StackDescription]
                         -> AWSAccountID
                         -> m StackDependencyGraph
makeStackDependencyGraph [] _ = return empty
makeStackDependencyGraph stacks accountID =
  flip runReaderT stackNodesByName loadGraph
  where
    stackNodes = zip [0..] stacks
    stackNodesByName = Map.fromList $ first (_stackName . snd) . dupe <$> stackNodes
    loadGraph = do
      bucketNameDependencies <- mconcat <$> traverse getBucketNameDependencies stackNodes
      parameterDependencies <- mconcat <$> traverse (getParameterDependencies accountID) stackNodes
      let dependencyEdges = Set.toList $ bucketNameDependencies <> parameterDependencies
      stackDescriptionNodes <- Map.elems <$> ask
      return $ mkGraph stackDescriptionNodes dependencyEdges

getBucketNameDependencies :: (MonadThrow m, MonadReader StackNodeMap m)
                          => StackNode
                          -> m (Set StackDependency)
getBucketNameDependencies source@(_, StackDescription{..}) =
  let stackOutputBucketNames = mapMaybe (getStackOutputBucketName . _bucketName) _s3upload
  in Set.fromList . catMaybes <$> traverse (parameterValueToDependency source) stackOutputBucketNames

getParameterDependencies :: (MonadThrow m, MonadReader StackNodeMap m)
                         => AWSAccountID
                         -> StackNode
                         -> m (Set StackDependency)
getParameterDependencies accountID source@(_, StackDescription{..}) = do
  params <- HashMap.elems <$> getParameters _parameters accountID
  Set.fromList . catMaybes <$> traverse (parameterValueToDependency source) params

parameterValueToDependency :: (MonadThrow m, MonadReader StackNodeMap m)
                           => StackNode
                           -> ParameterValue
                           -> m (Maybe StackDependency)
parameterValueToDependency (sourceIndex, StackDescription{..}) (StackOutput output@StackOutputName{..}) = do
  stacks <- ask
  (targetIndex, _) <- _soStackName `Map.lookup` stacks & maybe
    (throwM $ UnknownStackReference (StackReference _stackName output)) return
  return . Just $ (targetIndex, sourceIndex, OutputName _soOutputName)

parameterValueToDependency _ _ = return Nothing

getStackOutputBucketName :: BucketName -> Maybe ParameterValue
getStackOutputBucketName (parseString parameterValue mempty . unpack . toText -> Success s@(StackOutput _)) = Just s
getStackOutputBucketName _ = Nothing
