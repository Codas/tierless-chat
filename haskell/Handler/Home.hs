{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Home where

import           Conduit
import           Control.Monad         (forever, void)
import qualified Data.Map              as M
import           Import
import           Yesod.Form.Bootstrap3
import           Yesod.WebSockets

data ChannelName = ChannelName { name :: Text}

channelForm :: Html -> MForm Handler (FormResult ChannelName, Widget)
channelForm = renderBootstrap3 BootstrapInlineForm $ ChannelName
    <$> areq textField (withPlaceholder "Room Name" $ bfs ("Room Name" :: Text)) Nothing

createNewRoom :: Text -> App -> STM ChatRooms
createNewRoom c app = do let roomTVar = chatRooms app
                             crn = broadcast app
                         newRooms <- do rooms <- readTVar roomTVar
                                        addChatRoom c rooms
                         do writeTVar roomTVar newRooms
                            writeTChan crn $ roomsToText newRooms
                         return newRooms

getHomeR :: Handler Html
getHomeR = do webSockets homeSocket
              app <- getYesod
              (widget, enctype) <- generateFormPost channelForm
              rooms <- atm $ readTVar $ chatRooms app
              let roomInformation = M.toAscList $ channelInformation rooms
              defaultLayout $ do setTitle "Haskell Chat"
                                 $(widgetFile "home")


homeSocket :: WebSocketsT Handler ()
homeSocket = do app <- getYesod
                let chanListChan = broadcast app
                readChan <- atm $ dupTChan chanListChan
                race_
                  (forever $ atm (readTChan readChan) >>= sendTextData)
                  (sourceWS $$ mapM_C (\chan -> void $ atm $ createNewRoom chan app))
