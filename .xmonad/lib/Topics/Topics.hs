module Topics.Topics where

import Data.Maybe
import Text.Printf (printf)

-- downloaded from cabal
import Actions.DynamicWorkspaceGroups

import XMonad
import XMonad.Actions.TopicSpace
import XMonad.Actions.GridSelect
import XMonad.Prompt
import XMonad.Prompt.Workspace
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import Control.Arrow

-- meh duplicated
myTerminal2 = "gnome-terminal"
myXPConfig = defaultXPConfig

data TopicItem = TI
  {
    topicName :: Topic
  , topicDir  :: Dir
  , topicAct  :: X ()
  }

topicGroups :: [TopicItem]
topicGroups =
 [ TI "home"           "workspace"               (spawnInTopicDir "./home.sh")
 , TI "courtroom"      "workspace/courtroom"     (spawnInTopicDir "./workspace.sh")
 , TI "rothenberg"     "workspace/rothenberg"    (return())
 , TI "healthtalker"   "workspace/healthtalker"  (return())
 , TI "cyclist"        "workspace/cyclist"       (spawnInTopicDir "./workspace.sh")
 , TI "misc"           "workspace"               (return())
 ]

topicWithScreen :: Int -> TopicItem -> TopicItem
topicWithScreen sid (TI n d a) = TI (show sid ++ '_':n) d a

topicsWithScreens :: Int -> [TopicItem] -> [TopicItem]
topicsWithScreens n topics = [topicWithScreen sid topic | topic <- topics, sid <- [0..n-1]]

myTopics :: [TopicItem]
myTopics = topicsWithScreens 3 topicGroups

myTopicNames :: [Topic]
myTopicNames = map topicName myTopics

topicGroupNames :: [Topic]
topicGroupNames = map topicName topicGroups

myTopicConfig = TopicConfig
  { topicDirs          = M.fromList $ map (topicName &&& topicDir) myTopics
  , defaultTopicAction = const $ return ()
  , defaultTopic       = "home"
  , topicActions       = M.fromList $ map (topicName &&& topicAct) myTopics
  , maxTopicHistory    = 10
  }


goto :: Topic -> X ()
goto = switchTopic myTopicConfig

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "cd " ++ dir ++ " && " ++ myTerminal2

spawnShellWith :: String -> X ()
spawnShellWith what = spawn $ myTerminal2 ++ printf " -e '%s'" what

spawnInTopicDir act = currentTopicDir myTopicConfig >>= spawnIn act
spawnIn act dir = spawn $ "cd " ++ dir ++ "; " ++ act

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedTopic = do
  -- g <- gridselect defaultGSConfig $ zip currentScreenTopics currentScreenTopics
  g <- gridselect defaultGSConfig $ zip topicGroupNames topicGroupNames
  -- whenJust g goto
  whenJust g viewWSGroup

-- currentScreen = do
--   topic <- gets (W.tag . W.workspace . W.current . windowset)
--   return $ Just . topic
-- 
-- currentScreenTopics = map (currentScreen ++) myTopicNames

--where screenID = withWindowSet $ return . Just . W.currentTag

-- let screenID = gets (withWindowSet $ W.screen . W.current)
-- in map (screenID ++) myTopicNames

-- currentScreenTopics = do
--   ws <- gets windowset
--   let screenID = W.screen (W.current ws)
--   --map (screenID ++) myTopicNames
--   return $ map (myMarshall screenID) myTopicNames
--   -- where screenID = withWindowSet $ return . Just . W.currentTag
--   -- screenID <- withWindowSet $ W.screen . W.current

myMarshall :: ScreenId -> Topic -> Topic
myMarshall s t = show s ++ '_':t
