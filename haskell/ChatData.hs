module ChatData (ChatRooms
                ,ChatRoom()
                ,initChatRooms
                ,channel
                ,users
                ,addUser
                ,removeUser
                ,newChatRoom
                ,chatRoom
                ,addChatRoom
                ,hasChannel
                ,channels
                ,channelInformation
                ,addUserToChatRoom
                ,removeUserFromChatRoom) where

import           Control.Concurrent.STM
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text
import           Prelude

data ChatRoom = ChatRoom
                   { _channel :: TChan Text
                   , _users   :: Set Text }

channel :: ChatRoom -> TChan Text
channel (ChatRoom chan _) = chan

users :: ChatRoom -> [Text]
users  (ChatRoom _ us) = Set.toAscList us

addUser :: Text -> ChatRoom -> ChatRoom
addUser usr r = r {_users = Set.insert usr (_users r)}

removeUser :: Text -> ChatRoom -> ChatRoom
removeUser usr r = r {_users = Set.delete usr (_users r)}

type ChatRooms = Map Text ChatRoom

newChatRoom :: STM ChatRoom
newChatRoom = do c <- newBroadcastTChan
                 return $ ChatRoom c Set.empty

addChatRoom :: Text -> ChatRooms -> STM ChatRooms
addChatRoom c cs = do channel' <- newChatRoom
                      return $ if hasChannel c cs
                                  then cs
                                  else M.insert c channel' cs

chatRoom :: Text -> ChatRooms -> STM (ChatRoom, ChatRooms)
chatRoom c cs = if hasChannel c cs
                   then return (cs M.! c, cs)
                   else do room <- newChatRoom
                           let rooms = M.insert c room cs
                           return (room, rooms)

hasChannel :: Text -> ChatRooms -> Bool
hasChannel = M.member

modifyUserForChatRoom :: Text -> (ChatRoom -> ChatRoom) -> ChatRooms  -> STM ChatRooms
modifyUserForChatRoom c f cs = do defaultRoom <- newChatRoom
                                  let room = M.findWithDefault defaultRoom c cs
                                  return $ M.insert c (f room) cs

addUserToChatRoom :: Text -> Text -> ChatRooms -> STM ChatRooms
addUserToChatRoom u c = modifyUserForChatRoom c (addUser u)

removeUserFromChatRoom :: Text -> Text -> ChatRooms -> STM ChatRooms
removeUserFromChatRoom u c = modifyUserForChatRoom c (removeUser u)

channelInformation :: Map Text ChatRoom -> Map Text Int
channelInformation = M.map (Set.size . _users)

channels :: ChatRooms -> [Text]
channels = M.keys

initChatRooms :: ChatRooms
initChatRooms = M.empty
